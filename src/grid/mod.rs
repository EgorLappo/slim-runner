use color_eyre::eyre::bail;
use color_eyre::eyre::{self, Result, WrapErr};
use core::panic;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use log::{debug, info};
use polars::prelude::*;
use serde::Deserialize;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::env;
use std::fs;
use std::io::Cursor;
use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use threadpool::ThreadPool;

mod cmditer;
use cmditer::{CommandIterator, IteratorOutput};

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
    custom_script: Option<PathBuf>,
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

pub struct Grid {
    cores: usize,
    output_dir: PathBuf,
    write_every: usize,
    total_runs: usize,
    chunking: Option<(usize, usize)>,
    command_iter: CommandIterator,
}

impl Grid {
    pub fn new<P: AsRef<Path> + Clone>(
        config_file: P,
        chunking: Option<(usize, usize)>,
    ) -> Result<Self> {
        let config = std::fs::read_to_string(&config_file).wrap_err_with(|| {
            format!(
                "Failed to read config file {}",
                config_file.as_ref().display()
            )
        })?;
        let config: GridConfig = toml::from_str(&config).wrap_err_with(|| {
            format!(
                "Failed to parse config file {}",
                config_file.as_ref().display(),
            )
        })?;

        let mut string_parameters = HashMap::new();
        let mut num_parameters = HashMap::new();

        for (name, param) in config.grid {
            match param {
                ConfigParameter::Value(val) => {
                    let value = vec![val];
                    debug!("Adding numerical parameter {} with value {:?}", name, value);
                    num_parameters.insert(name, value);
                }
                ConfigParameter::List(list) => {
                    debug!("Adding numerical parameter {} with values {:?}", name, list);
                    num_parameters.insert(name, list);
                }
                ConfigParameter::Range(range) => {
                    let values = range.values();
                    debug!(
                        "Adding numerical parameter {} with values {:?}",
                        name, values
                    );
                    num_parameters.insert(name, values);
                }
                ConfigParameter::String(string) => {
                    debug!("Adding string parameter {} with value {:?}", name, string);
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
            config.settings.custom_script,
            seed,
            replicates,
            annotate,
            limit,
            string_parameters,
            num_parameters,
        )
        .wrap_err("Failed to initialize CommandIterator")?;

        Ok(Self {
            cores,
            output_dir: config.settings.output_dir,
            write_every,
            total_runs,
            chunking,
            command_iter,
        })
    }

    pub fn run(self, bar: MultiProgress) -> Result<()> {
        // first, figure out if we need to skip some commands due to chunking
        let (skip, take) = if let Some((nchunks, chunk)) = self.chunking {
            let chunk_len = (self.total_runs as f64) / (nchunks as f64);
            let chunk_len = chunk_len.ceil() as usize;

            let skip = chunk * chunk_len;
            let take = if (chunk + 1) * chunk_len > self.total_runs {
                self.total_runs - skip
            } else {
                chunk_len
            };

            debug!(
                "Chunking: starting with run {} and doing {} runs",
                skip + 1,
                take
            );
            (skip, take)
        } else {
            debug!(
                "No chunking: starting with run {} and doing all {} runs",
                0, self.total_runs
            );
            (0, self.total_runs)
        };

        // draw the progress bar
        let pb = bar.add(ProgressBar::new(take as u64));
        let style = ProgressStyle::with_template(
            "{spinner:.purple} [{elapsed}/{duration}] [{bar:.cyan/blue}] {human_pos}/{human_len}",
        )
        .unwrap();
        pb.set_style(style);

        // let (sender, receiver) = crossbeam_channel::unbounded();
        let (wrt_sender, wrt_receiver) = mpsc::channel();
        // spawn the "writer" thread that will write the output to disk
        let writer = std::thread::spawn(self.writer(wrt_receiver));

        let pool = ThreadPool::with_name("slim_worker".into(), self.cores);
        let (pool_sender, pool_receiver) = mpsc::channel();

        let consumer = std::thread::spawn(move || -> Result<()> {
            for r in pool_receiver.iter() {
                r?;
                pb.inc(1);
            }

            pb.finish();
            Ok(())
        });

        // run the commands in parallel with the threadpool
        for cmd in self.command_iter {
            let cmd = cmd.wrap_err("generating SLiM command failed")?;
            if (cmd.0 >= skip) & (cmd.0 < skip + take) {
                // first, make sure that previous commands did not panic:
                if consumer.is_finished() {
                    // this would trigger only when we sent an Err to consumer,
                    // as otherwise it would just be blocked on the recv iter (we still hold sender so its not hung up)
                    match consumer
                        .join()
                        .expect("failed to join consumer thread")
                        .wrap_err("error executing commands in threadpool")
                    {
                        Ok(_) => panic!("consumer cannot return early if everythign is okay"),
                        Err(e) => bail!(e),
                    }
                }

                let wrt_sender = wrt_sender.clone();
                let pool_sender = pool_sender.clone();
                let worker = Grid::worker();
                pool.execute(move || {
                    let result = worker(wrt_sender, cmd);
                    pool_sender
                        .send(result)
                        .expect("there should be a channel waiting to receive a result");
                });
            } else if cmd.0 >= (skip + take) {
                break;
            }
        }

        // drop instance of snd in this thread (threadpool may still own some)
        drop(pool_sender);
        drop(wrt_sender);

        // wait for the writer thread to finish
        writer
            .join()
            .expect("couldn't join on the writer thread")
            .wrap_err("error writing simulation results to disk")?;
        // wait for consumer to finish
        consumer
            .join()
            .expect("couldn't join consumer thread")
            .wrap_err("error executing commands in thread pool")?;

        info!("All simulations have been run successfully");

        Ok(())
    }

    // writer thread function
    // receives dataframes from the worker threads and writes them to disk in batches
    fn writer(
        &self,
        receiver: mpsc::Receiver<(usize, LazyFrame)>,
    ) -> impl FnOnce() -> std::result::Result<(), eyre::Error> + 'static {
        // duplicate to not borrow self
        let write_every = self.write_every;
        let mut output_dir = self.output_dir.clone();
        if let Some((_, chunk)) = self.chunking {
            output_dir.push(format!("chunk_{}", chunk));
        }

        fn write(mut df: DataFrame, file_counter: usize, output_directory: PathBuf) -> Result<()> {
            let path = output_directory.join(format!("output_{}.parquet", file_counter));
            let file = fs::File::create(&path)
                .wrap_err_with(|| format!("Failed to create output file {}", path.display()))?;

            info!("Begin writing output to file: {}", path.display());

            ParquetWriter::new(file).finish(&mut df).wrap_err_with(|| {
                format!("Failed to write parquet output to {}", path.display())
            })?;

            info!("Finished writing output to file: {}", path.display());
            Ok(())
        }

        move || -> Result<()> {
            debug!("Starting writer thread");

            // create the output directory if it does not exist
            if !output_dir.exists() {
                debug!("Creating output directory: {}", output_dir.display());
                std::fs::create_dir_all(&output_dir).wrap_err_with(|| {
                    format!("Failed to create output directory {}", output_dir.display())
                })?;
            }

            // counter for output file names
            let mut file_counter: usize = 0;
            let mut queue: VecDeque<LazyFrame> = VecDeque::with_capacity(write_every);

            for (_, d) in receiver {
                // push the dataframe to the queue
                queue.push_back(d);

                // if the queue is full, write the data to disk
                if queue.len() >= write_every {
                    // create the dataframe
                    let to_write: Vec<LazyFrame> = queue.drain(0..write_every).collect();
                    // concatenate the dataframes
                    let d_combined = concat(to_write, UnionArgs::default())
                        .wrap_err("Failed to concatenate dataframes: ")?
                        .collect()
                        .wrap_err("Failed to collect LazyFrames")?;

                    write(d_combined, file_counter, output_dir.clone())?;

                    file_counter += 1;
                }
            }

            // write the remaining data to disk
            if !queue.is_empty() {
                let to_write: Vec<LazyFrame> = queue.drain(..).collect();
                let d_combined = concat(to_write, UnionArgs::default())
                    .wrap_err("Failed to concatenate dataframes")?
                    .collect()
                    .wrap_err("Failed to collect LazyFrames")?;

                write(d_combined, file_counter, output_dir)?;
            }

            Ok(())
        }
    }

    // rayon worker function
    // runs SLiM and packages the output into an annotated dataframe
    fn worker() -> impl Fn(mpsc::Sender<(usize, LazyFrame)>, IteratorOutput) -> Result<()> {
        move |s, (i, num_ann, str_ann, mut command)| {
            // run SLiM
            // debug!("Running command for iteration {:?}", i);
            let output = command
                .output()
                .wrap_err("Failed to execute SLiM command")?;

            if !output.status.success() {
                return Err(eyre::eyre!(
                    "SLiM command failed with exit code {}: {}\nCommand: {:?}",
                    output.status,
                    String::from_utf8_lossy(&output.stderr),
                    command
                ));
            }

            // parse the csv output from SLiM
            // debug!("reading csv output from SLiM for iteration {:?}", i);
            let mut d = CsvReader::new(Cursor::new(&output.stdout))
                .finish()
                .wrap_err("Failed to parse SLiM output into a table")?
                .lazy();

            // debug!("annotating dataframe for iteration {:?}", i);
            // add numerical annotations
            d = d.with_columns(
                num_ann
                    .iter()
                    .map(|(k, v)| lit(*v).alias(k))
                    .collect::<Vec<_>>(),
            );

            // make replicate into an integer column
            d = d.with_column(col("replicate").cast(DataType::Int64));

            // add string annotations if there are any
            d = if str_ann.is_empty() {
                d
            } else {
                d.with_columns(
                    str_ann
                        .iter()
                        .map(|(k, v)| lit(v.clone()).alias(k))
                        .collect::<Vec<_>>(),
                )
            };

            // send the dataframe to the channel
            // debug!("sending dataframe for iteration {:?}", i);
            s.send((i, d))
                .expect("Failed to send dataframe to writer channel");

            // doing great!
            Ok(())
        }
    }
}
