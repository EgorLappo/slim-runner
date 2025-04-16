use color_eyre::eyre::{self, Context, Result};
use itertools::{Itertools, MultiProduct};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;
use std::iter::Iterator;
use std::path::PathBuf;
use std::process::Command;
use steel::rvals::FromSteelVal;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use steel::SteelVal;
use steel_derive::Steel;

#[derive(Steel, Clone, Debug)]
struct SteelParameter {
    name: String,
    value: f64,
}

impl From<SteelParameter> for (String, f64) {
    fn from(value: SteelParameter) -> Self {
        (value.name, value.value)
    }
}

impl SteelParameter {
    fn new(name: String, value: f64) -> Self {
        SteelParameter { name, value }
    }
}

#[derive(Clone)]
struct SteelEngine {
    vm: Engine,
    script: String,
}

fn extract_last<T>(output: &[SteelVal]) -> Result<T>
where
    T: FromSteelVal,
{
    let val = &output[output.len() - 1];
    T::from_steelval(val).wrap_err("failed to convert last script expression to parameter vector")
}

impl SteelEngine {
    fn new(script_file: PathBuf) -> Result<Self> {
        let script = std::fs::read_to_string(&script_file).wrap_err_with(|| {
            format!("error reading custom script file {}", script_file.display())
        })?;

        let mut vm = Engine::new_sandboxed();
        vm.register_fn("parameter", SteelParameter::new);

        let init_code = r#"
            (define (force-convert n) (* n 1.0))
        
            (define-syntax parameters
               (syntax-rules () 
                 [(parameters a) 
                    (cons (parameter (symbol->string (quote a)) (force-convert a)) (list))]
                 [(parameters a b ...) 
                    (cons (parameter (symbol->string (quote a)) (force-convert a)) (parameters b ...))]
            ))
        "#;
        vm.run(init_code)
            .wrap_err("error running steel init code")?;

        Ok(SteelEngine { vm, script })
    }

    fn run(
        &mut self,
        current_replicate: usize,
        current_parameters: &HashMap<String, f64>,
    ) -> Result<HashMap<String, f64>> {
        self.vm
            .register_external_value("replicate", current_replicate)
            .unwrap();
        for (name, value) in current_parameters.iter() {
            self.vm.register_external_value(name, *value).unwrap();
        }

        let result = self
            .vm
            .run(self.script.clone())
            .wrap_err("failed while running custom script")?;

        let parameters: Vec<SteelParameter> = extract_last(&result).wrap_err("failed extracting results from the custom script: the last like should evaluate to a vector of parameters")?;

        Ok(parameters
            .iter()
            .map(|p| (p.name.clone(), p.value))
            .collect())
    }
}

// CommandIterator wraps the underlying MultiProd iterator
// to generate new slim commands on demand
#[derive(Clone)]
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
    // steel engine for running custom scripts
    engine: Option<SteelEngine>,
}

impl CommandIterator {
    #[allow(clippy::too_many_arguments)] // it's okay since it's only used once
    pub fn init(
        slim_executable: PathBuf,
        slim_script: PathBuf,
        steel_script: Option<PathBuf>,
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

        let engine = if let Some(steel_script) = steel_script {
            Some(SteelEngine::new(steel_script).wrap_err("failed to initialize steel engine")?)
        } else {
            None
        };

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
            engine,
        })
    }
}

type StrAnnotation = Vec<(String, String)>;
type NumAnnotation = Vec<(String, f64)>;
pub type IteratorOutput = (usize, NumAnnotation, StrAnnotation, Command);

// the iterator returns Strings that are valid calls to slim
impl Iterator for CommandIterator {
    type Item = Result<IteratorOutput>;

    fn next(&mut self) -> Option<Self::Item> {
        // if limit is set, check if we have reached it
        if let Some(k) = self.limit {
            if self.current_iteration > k {
                return None;
            }
        }

        // obtain new parameters if we have exhausted the replicates
        if self.current_replicate >= self.replicates {
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

        // build annotations
        // note: have to do this before building up the command
        //       to possibly save generated parameters from the custom script
        let mut num_annotation = vec![(String::from("replicate"), self.current_replicate as f64)];
        if self.annotate {
            for (name, value) in &self.current_parameters {
                num_annotation.push((name.clone(), *value));
            }
        }
        let str_annotation = self.string_parameters.clone();

        // now we are guaranteed to have parameters
        let mut command = Command::new(&self.slim_executable);

        // add -l 0 to suppress output
        command.arg("-l").arg("0");

        // add -d to specify stdout output
        command.arg("-d").arg("outfile='/dev/stdout'");

        // add seed
        command.arg("-s").arg(self.rng.random::<u64>().to_string());

        // add string parameters
        for (name, value) in &self.string_parameters {
            command.arg("-d").arg(format!("{}='{}'", name, value));
        }

        // add numerical parameters
        for (name, value) in &self.current_parameters {
            command.arg("-d").arg(format!("{}={}", name, value));
        }

        // run the steel interpreter if we have to
        if let Some(e) = self.engine.as_mut() {
            // run and check output
            match e.run(self.current_replicate, &self.current_parameters) {
                // if ok, add all parameters as above
                Ok(hm) => {
                    for (name, value) in hm {
                        command.arg("-d").arg(format!("{}={}", name, value));
                        // also save parameters for annotating
                        num_annotation.push((name, value));
                    }
                }
                // if run failed, signal
                Err(e) => return Some(Err(e)),
            }
        }
        // add script
        command.arg(&self.slim_script);

        // sort annotations by key before returning
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

        Some(Ok(output))
    }
}
