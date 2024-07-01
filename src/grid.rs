use color_eyre::eyre;
use color_eyre::eyre::Result;
use indicatif::{MultiProgress, ProgressBar};
use itertools::{Itertools, MultiProduct};
use log::{debug, info};
use polars::prelude::*;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;
use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::Cursor;
use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Deserialize)]
struct GridConfig {
    settings: Settings,
    grid: HashMap<String, ConfigParameter>,
}

#[derive(Debug, Deserialize)]
struct Settings {
    seed: Option<u64>,
    replicates: Option<usize>,
    script: PathBuf,
    slim_executable: Option<PathBuf>,
    output_dir: PathBuf,
    cores: Option<usize>,
    annotate: Option<bool>,
    write_every: Option<usize>,
    limit: Option<usize>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum ConfigParameter {
    Value(f64),
    List(Vec<f64>),
    Range(Range),
    String(String),
}

#[derive(Debug, Deserialize)]
struct Range {
    from: f64,
    to: f64,
    #[serde(flatten)]
    seq: SequenceOption,
}

#[derive(Debug, Deserialize)]
enum SequenceOption {
    #[serde(rename = "num")]
    Num(usize),
    #[serde(rename = "step")]
    Step(f64),
}

impl Range {
    fn values(&self) -> Vec<f64> {
        match &self.seq {
            // generate specific number of values
            SequenceOption::Num(n) => {
                // generate the sequence that includes the `to` value
                // like np.linspace(self.from, self.to, n)
                let step = (self.to - self.from) / (*n as f64 - 1.0);
                (0..*n).map(|i| self.from + i as f64 * step).collect()
            }
            // generate values with a specific step size
            SequenceOption::Step(step) => {
                // "+1" and "filter" make sure that the endpoint is included in the sequence
                let n = ((self.to - self.from) / step).ceil() as usize + 1;
                (0..n)
                    .map(|i| self.from + i as f64 * step)
                    .filter(|&x| x <= self.to)
                    .collect()
            }
        }
    }
}

// CommandIterator wraps the underlying MultiProd iterator
// to generate new slim commands on demand
#[derive(Debug)]
pub struct CommandIterator {
    slim_executable: PathBuf,
    slim_script: PathBuf,
    rng: SmallRng,
    replicates: usize,
    current_replicate: usize,
    current_iteration: usize,
    limit: Option<usize>,
    annotate: bool,
    num_keys: Vec<String>,
    current_parameters: HashMap<String, f64>,
    // this stores the current numerical parameters
    string_parameters: Vec<(String, String)>,
    // this should store the current state of the iterator
    num_cartprod: MultiProduct<std::vec::IntoIter<f64>>,
}

impl CommandIterator {
    #[allow(clippy::too_many_arguments)] // it's okay since it's only used once
    fn init(
        slim_executable: PathBuf,
        slim_script: PathBuf,
        seed: usize,
        replicates: usize,
        annotate: bool,
        limit: Option<usize>,
        string_parameters: HashMap<String, String>,
        num_parameters: HashMap<String, Vec<f64>>,
    ) -> Result<Self> {
        let rng = SmallRng::seed_from_u64(seed as u64);

        let num_keys: Vec<String> = num_parameters.keys().cloned().collect();

        let values: Vec<Vec<f64>> = num_parameters.values().cloned().collect();
        let mut num_cartprod = values.into_iter().multi_cartesian_product();

        // unwrap or signal empty with error
        let current_parameters = num_cartprod
            .next()
            .ok_or_else(|| eyre::eyre!("Provided parameter set is empty"))?;

        let current_parameters = num_keys
            .iter()
            .zip(current_parameters)
            .map(|(k, v)| (k.clone(), v))
            .collect();

        // sort string parameters by key
        let string_parameters = string_parameters
            .into_iter()
            .sorted_by(|(k1, _), (k2, _)| k1.cmp(k2))
            .collect();

        Ok(Self {
            slim_executable,
            slim_script,
            rng,
            current_replicate: 0,
            current_iteration: 0,
            limit,
            annotate,
            replicates,
            num_keys,
            current_parameters,
            string_parameters,
            num_cartprod,
        })
    }
}

type StrAnnotation = Vec<(String, String)>;
type NumAnnotation = Vec<(String, f64)>;
type IteratorOutput = (usize, NumAnnotation, StrAnnotation, Command);

// the iterator returns Strings that are valid calls to slim
impl Iterator for CommandIterator {
    type Item = IteratorOutput;

    fn next(&mut self) -> Option<Self::Item> {
        // if limit is set, check if we have reached it
        if let Some(k) = self.limit {
            if self.current_iteration > k {
                return None;
            }
        }

        // obtain new parameters if we have exhausted the replicates
        if self.current_replicate > self.replicates {
            if let Some(new_parameters) = self.num_cartprod.next() {
                self.current_parameters = self
                    .num_keys
                    .iter()
                    .zip(new_parameters)
                    .map(|(k, v)| (k.clone(), v))
                    .collect();
                self.current_replicate = 0;
            } else {
                return None;
            }
        }

        // now we are guaranteed to have parameters
        let mut command = Command::new(&self.slim_executable);

        // add -l 0 to suppress output
        command.arg("-l").arg("0");

        // add -d to specify stdout output
        command.arg("-d").arg("outfile='/dev/stdout'");

        // add seed
        command.arg("-s").arg(self.rng.gen::<u64>().to_string());

        // add string parameters
        for (name, value) in &self.string_parameters {
            command.arg("-d").arg(format!("{}='{}'", name, value));
        }

        // add numerical parameters
        for (name, value) in &self.current_parameters {
            command.arg("-d").arg(format!("{}={}", name, value));
        }

        // add script
        command.arg(&self.slim_script);

        // build annotations
        let mut num_annotation = vec![(String::from("replicate"), self.current_replicate as f64)];
        if self.annotate {
            for (name, value) in &self.current_parameters {
                num_annotation.push((name.clone(), *value));
            }
        }
        let str_annotation = self.string_parameters.clone();

        // sort annotations by key
        num_annotation = num_annotation
            .into_iter()
            .sorted_by(|(k1, _), (k2, _)| k1.cmp(k2))
            .collect();

        let output = (
            self.current_iteration,
            num_annotation,
            str_annotation,
            command,
        );

        self.current_replicate += 1;
        self.current_iteration += 1;

        Some(output)
    }
}

#[derive(Debug)]
pub struct Grid {
    cores: usize,
    output_dir: PathBuf,
    write_every: usize,
    total_runs: usize,
    command_iter: CommandIterator,
}

impl Grid {
    pub fn new<P: AsRef<Path>>(config_file: P) -> Result<Self> {
        let config = std::fs::read_to_string(config_file)?;
        let config: GridConfig = toml::from_str(&config)?;

        let mut string_parameters = HashMap::new();
        let mut num_parameters = HashMap::new();

        for (name, param) in config.grid {
            match param {
                ConfigParameter::Value(val) => {
                    num_parameters.insert(name, vec![val]);
                }
                ConfigParameter::List(list) => {
                    num_parameters.insert(name, list);
                }
                ConfigParameter::Range(range) => {
                    num_parameters.insert(name, range.values());
                }
                ConfigParameter::String(string) => {
                    string_parameters.insert(name, string);
                }
            }
        }

        // default to seed 0
        let seed = config.settings.seed.unwrap_or(0) as usize;
        debug!("Using seed: {}", seed);

        // default to 1 replicate per parameter set
        let replicates = config.settings.replicates.unwrap_or(1);
        debug!("Using {} replicates per parameter set", replicates);

        // default to 1 core
        let cores = config.settings.cores.unwrap_or(1);
        debug!("Using {} cores", cores);

        // if slim_executable is not provided, try $SLIM_EXE, else default to "slim"
        let slim_executable = config
            .settings
            .slim_executable
            .or_else(|| env::var("SLIM_EXE").ok().map(PathBuf::from))
            .unwrap_or_else(|| PathBuf::from("slim"));
        debug!("Using SLiM executable: {:?}", slim_executable);

        let annotate = config.settings.annotate.unwrap_or(false);
        debug!("Annotating output: {}", annotate);

        let write_every = config.settings.write_every.unwrap_or(100);
        debug!("Writing output every {} runs", write_every);

        let mut total_runs = replicates * num_parameters.values().map(Vec::len).product::<usize>();

        let limit = config.settings.limit;
        if let Some(k) = limit {
            if k == 0 {
                return Err(eyre::eyre!("Trim value is 0, no simulation will be run"));
            }
            debug!("Limiting to {} runs", k);

            total_runs = total_runs.min(k);
        }

        let command_iter = CommandIterator::init(
            slim_executable,
            config.settings.script,
            seed,
            replicates,
            annotate,
            limit,
            string_parameters,
            num_parameters,
        )
        .map_err(|e| eyre::eyre!("Failed to initialize CommandIterator: {}", e))?;

        Ok(Self {
            cores,
            output_dir: config.settings.output_dir,
            write_every,
            total_runs,
            command_iter,
        })
    }

    pub fn run(self, bar: MultiProgress) -> Result<()> {
        // first, set the envvar for rayon to limit the number of threads
        env::set_var("RAYON_NUM_THREADS", self.cores.to_string());

        // draw the progress bar
        let pb = bar.add(ProgressBar::new(self.total_runs as u64));

        // create the output directory if it does not exist
        if !self.output_dir.exists() {
            debug!("Creating output directory: {}", self.output_dir.display());
            std::fs::create_dir_all(&self.output_dir).map_err(|e| {
                eyre::eyre!(
                    "Failed to create output directory {}: {}",
                    self.output_dir.display(),
                    e
                )
            })?;
        }

        // i have two ideas how to arrange parallel running here
        // one is implemented below with chunking the iterator,
        //   then running a single chunk in parallel and writing a compressed parquet file
        // another one is to have a sort of IO thread that writes the output to disk in parallel
        //   that thread would have a mutex'd buffer to which the other threads would write their data

        self.command_iter
            // split iterator into chunks
            // we will write data to disk after each chunk
            .chunks(self.write_every)
            .into_iter()
            .enumerate()
            .map(|(i_chunk, chunk)| {
                debug!("Running chunk {}", i_chunk);
                let results = chunk
                    .collect::<Vec<IteratorOutput>>()
                    .par_iter_mut()
                    .map(|(i, num_ann, str_ann, command)| {
                        // even if iterations are processed out of order,
                        // the progress bar should still update approximately correctly
                        if *i % 5 == 0 {
                            pb.inc(5);
                        }

                        let output = command
                            .output()
                            .map_err(|e| eyre::eyre!("Failed to execute SLiM command: {}", e))?;

                        if !output.status.success() {
                            return Err(eyre::eyre!(
                                "SLiM command failed with exit code {}: {}",
                                output.status,
                                String::from_utf8_lossy(&output.stderr)
                            ));
                        }

                        let d = CsvReader::new(Cursor::new(&output.stdout))
                            .finish()
                            .map_err(|e| {
                                eyre::eyre!("Failed to parse SLiM output into a table: {}", e)
                            })?
                            .lazy();

                        // add numerical annotations
                        let d = d.with_columns(
                            num_ann
                                .iter()
                                .map(|(k, v)| lit(*v).alias(k))
                                .collect::<Vec<_>>(),
                        );

                        //debug!("got dataframe {:?}", d.clone().collect());

                        // make replicate an integer column
                        let d = d.with_column(col("replicate").cast(DataType::Int64));

                        // add string annotations if there are any
                        if str_ann.is_empty() {
                            Ok(d)
                        } else {
                            let d = d.with_columns(
                                str_ann
                                    .iter()
                                    .map(|(k, v)| lit(v.clone()).alias(k))
                                    .collect::<Vec<_>>(),
                            );
                            Ok(d)
                        }
                    })
                    .collect::<Result<Vec<_>>>()
                    .map_err(|e| eyre::eyre!("Failed to run SLiM commands: {}", e))?;

                // combine all results into a single DataFrame
                let mut d = concat(results, UnionArgs::default())?.collect()?;

                // write the output to files
                let path = self.output_dir.join(format!("output_{}.parquet", i_chunk));
                let file = fs::File::create(&path).map_err(|e| {
                    eyre::eyre!("Failed to create output file {}: {}", path.display(), e)
                })?;

                debug!("Writing output to {}", path.display());
                ParquetWriter::new(file).finish(&mut d).map_err(|e| {
                    eyre::eyre!(
                        "Failed to write parquet output to {}: {}",
                        path.display(),
                        e
                    )
                })?;
                Ok(())
            })
            // make sure all elements are ok
            .collect::<Result<Vec<_>>>()?;

        pb.finish();

        info!("All chunks have been run successfully");

        Ok(())
    }
}
