## `SLiM` simulation wrapper

This is a small program to wrap SLiM grid runs. It targets cases in which __per-cycle, tabular output__ is the primary output of the simulation. Any `.csv`-like output is automatically consumed by this wrapper, annotated with parameter values, and batched into larger files. With shorter simulations, this saves a lot of time spent on disk writes and compression. The program also is able to run many instances of SLiM in parallel, speeding up simulations over large parameter grids.

Output is written as a sequence of parquet files `output_{i}.parquet`. Many parquet readers (e.g. `pandas` and `polars` python packages) know how to treat a directory as a single big parquet file.

### Installing and using the program

If you are using Nix, you can try `slim-runner` by running
```
nix shell github:EgorLappo/slim-runner
```

You can also use `cargo` to install `slim-runner`. First, install Rust with [rustup](https://rustup.rs). Then, run
```
cargo install --git https://github.com/EgorLappo/slim-runner.git   
```

The executable is called `slim-runner`. The usage instructions are 
```
$ slim-runner --help
Launch SLiM grid runs

Usage: slim-runner [CONFIG_PATH] [N_CHUNKS] [CHUNK]

Arguments:
  [CONFIG_PATH]  config TOML file [default: slim_config.toml]
  [N_CHUNKS]     number of chunks if evaluating only part of the grid is desired (optional)
  [CHUNK]        index of chunk to evaluate now (optional)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

Sometimes the total number of runs is too large to execute on a single machine even when many cores are available. To solve this, one would typically use an HPC cluster to spread the workload over many nodes. `slim-runner` supports this workflow through optional command line arguments: when both `N_CHUNKS` and `CHUNK` command line arguments are provided, it will only evaluate that fraction of all simulation runs. For example, if the total number of simulations is 1000, `slim-runner config.toml 10 2` would only execute simulation runs 101-200 and save the results to a subdirectory of the output directory to avoid conflicts with other chunks.

### Config format

Configuration is done through a `.toml` file that contains several top-level tables. The file must be passed to `slim-runner` as a command line argument (see usage instructions above).

The table *`settings`* contains global parameters such as SLiM script location, number of available cores, etc. 

``` toml
[settings]
# global random seed (optional, defaults to 0)
seed = 231
# number of replicates per parameter set (optional, defaults to 1)
replicates = 10
# slim script to run
script = "example_script.slim"
# output directory
output_dir = "output"
# number of parallel instances of SLiM to use (optional, defaults to 1)
cores = 6
# interval to write output to disk in parameter sets (optional, defaults to 100)
write_every = 1000
# annotate the output log files with grid parameter values (optional, defaults to `true`)
annotate = true
# only run first `limit' simulations (optional, for debugging purposes)
limit = 100
# custom slim executable path (optional, defaults to `slim`) 
slim_exe = "/path/to/slim"
# path to a custom script for parameter generation (optional)
custom_script = "example_custom_script.scm"
```

The parameters are set in the *`grid`* table. Valid parameter values are a string, a number, a list of number, and a range of values.

``` toml
[grid]
N = [500, 1000, 2000, 5000]
s = { from = 0, to = 0.1, num = 11 }
h = { from = 0, to = 1, step = 0.25 }
mu = 10e-7
rbp = 10e-8
tag = "example"
```

The ranges are always inclusive of the `to` value: for example, the range for `h` above would be expanded to `h=[0,0.25,0.5,0.75,1.0]`. 

The string parameters are only used as tags in case you want to somehow tag the whole run of `slim-runner`.

### Writing SLiM scripts

`slim-runner` works well with simulations that make use of logging or other standard output. This means that currently it does not deal with any output dumped at the end of the simulation explicitly, unless you manually make it into a csv file.

*Types.* All numerical parameters are passed as floats, so don't forget to cast required parameters to integers. For example, write 
```
sim.addSubpop("p1", asInteger(N));
```

*Required argument.* The SLiM script is **always** passed an argument titled `outfile`, and this file handle should be used for the `Logfile`:

```
log = community.createLogFile(outfile, logInterval=10, flushInterval=1000);
log.addCycle();
log.addMeanSDColumns('k', 'sim.subpopulations.individuals.countOfMutationsOfType(m1);');
```

You can use this argument for debugging your output while running SLiM on your own. When running SLiM through `slim-runner`, however, the value `/dev/stdout` would be used as `outfile` to SLiM so that the logfile is written to standard output. Many simulation runs will then be combined into bigger files and written to disk (the frequency of disk writes is controlled by the `write_every` parameter in the config). 

*Annotation.* If `annotate = true` in the config, the output for each will be annotated with the grid parameter values and the replicate number. If `annotate = false`, only the replicate number will be recorded. Use this if you want to annotate the table yourself from inside slim, e.g. if you want to record transformed parameters.

### Custom scripts for parameter generation

`slim-runner` allows users to write custom scripts for parameter generation using an embedded Scheme interpreter.

The main reason for introducing this feature is to keep the specification of the config file minimal while allowing for many different ways of introducing parameters. For example, currently the program takes a multi-cartesian product of all parameter value ranges/list to create the grid (which essentially corresponds to a regular `List` monad in Haskell). However, I have found that sometimes I need to "zip" the parameters together rather than taking a cartesian product (which in Haskell is implemented as the `ZipList` monad). Another situation came up recently where I wanted every point in the parameter grid to represent some probability distribution rather than a single set of parameters. You can implement both of these alterations to the parameter grid using custom user scripts.

*But why not do it in SLiM?* If you could not tell already, I actually hate writing SLiM code despire its dominant position in population genetics software. For me, SLiM scripts are hard to write, hard to debug efficiently, and any nontrivial script becomes an intertwined mess of code dealing with input parameters, demographic changes, mutation counting, logging, etc. So, I am inclined to write a SLiM script once and keep it immutable, while also extracting as much code that is not pertinent to the Wright-Fisher model out.

More fundamentally, I really do not understand why we as a society needed yet another scripting language (Eidos) that would only be used in a single piece of software... But even then, out of all possible options, why did it have to be based on R??? Why does R have automatic implicit conversion between floats and ints while Eidos doesnt? Why can't I have consitent array indexing across my tools? and why do functions called in the `initialize()` block implicitly declare global variables? and why can I not call my mutations anything beyond `m1`, `m2`... why can I not call my mutation types `mBackground` and `mFocal`???

So, in my work I gravitate towards writing a single SLiM script for a set of experiments (or, better, one script per project) and then *really* making sure it works correctly. After the script is ready, I never want to touch it again.

Ok, rant over. How do you actually write custom scripts?

#### Writing custom scripts

A custom script will run once per every replicate simulation per every point in the parameter grid as specified in the config TOML file. This means that custom scripts cannot change the size of the grid or the total number of simulations (unless you force SLiM to stop early inside the SLiM script using some condition).

In its simplest form, a custom script would look like this:

```scheme
(define x 1.0)
(define y 256)
(parameters x y)
```

Using this custom script would pass additional arguments `-d x=1.0 -d y=256.0` to every run of SLiM, with all parameters converted to floating point numbers.

The key part of the script is the last expression: it *must* look like `(parameters a b c ...)` with all newly introduced parameter variables listed in a call to `parameters`. Any number of arguments may be passed. SLiM then would be given access to these arguments under exact same names. Currently, names of parameters produced by the custom script **must not** conflict with the names of parameters in the grid.

All custom scripts are provided with all current grid parameter values as variables with the same name, as well as with variables `replicate` and `seed` that contain current replicate number and random seed for the current SLiM run.

This means that if your parameter grid looks like this

```toml
[grid]
N = [500, 1000, 2000, 5000]
s = { from = 0, to = 0.1, num = 11 }
```

then you can use variables `N`, `s`, `replicate`, and `seed` in the custom script. For example, you can write

```scheme
(define sel (+ 1 s))
(define twoN (* 2 N))
(parameters sel twoN)
```

The main purpose of passing `seed` is, of course, for deterministic "random" sampling. A simple random number generator is again provided by `slim-runner`, and here is an example of how to use it:
```scheme
;; initialize rng (function provided to the script) 
(define rng (new-rng seed))
;; currently, three sampling functions are available for use:
;; random-uniform samples floats in [0.0, 1.0)
(define randf (random-uniform rng))
;; random-range-float samples floats in a given range, also half-open
(define randr (random-range-float rng 0.0 0.2))
;; random-range-int works the same, but for integers
(define randi (random-range-int rng 0 10))

;; use random values to create new parameters
(define sr (* s randf))
(define Nr (+ N randi))

;; pass new parameters to SLiM
(parameters Nr sr randr)
```

Scripting is implemented using the [`steel`](https://github.com/mattwparas/steel) package, so there is a very quick way to debug and test your custom code! Just use the online [Steel Playground](https://mattwparas.github.io/steel-playground/dev/). Of course, random sampling functionality and the `parameters` macro are provided by the internals of `slim-runners` so you can't use them elsewhere, but they are also typically not the parts of the script that need testing/debugging.
