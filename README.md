## `SLiM` simulation wrapper

This is a small program to wrap SLiM grid runs. It targets cases in which per-cycle output is the primary output of the simulation. Any `.csv`-like output is automatically consumed by this wrapper, annotated with parameter values, and batched into larger files. With shorter simulations, this saves a lot of time spent on disk writes and compression.

Output is written as a sequence of parquet files `output_{i}.parquet`. Many parquet readers know how to treat a directory as a single big parquet file.

### Config format

Configuration is done through a `.toml` file that contains several top-level tables.

The table *`settings`* contains global parameters such as SLiM script location, number of available cores, etc. 

``` toml
[settings]
# global random seed
seed = 231
# number of replicates per parameter set
replicates = 10
# slim script to run
script = "example_script.slim"
# output directory
output_dir = "output"
# number of threads to use
cores = 6
# interval to write output to disk in parameter sets
write_every = 1000
# annotate the output log files with grid parameter values
annotate = true
# only run first `limit' simulations (for debugging purposes)
limit = 100
# custom slim executable path (also can be set with $SLIM_EXE environment variable)
slim_exe = "/path/to/slim"
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

*Required argument.* The SLiM script is **always** passed an argument titled `outfile`, and this file handle should be used for the `Logfile`:

```
log = community.createLogFile(outfile, logInterval=10);
log.addCycle();
log.addMeanSDColumns('k', 'sim.subpopulations.individuals.countOfMutationsOfType(m1);');
```

You can use this argument for debugging your output, and `slim-runner` will then pass `/dev/stdout` to SLiM to read this file from standard output. Many simulation runs will then be combined into bigger files and written to disk.

*Annotation.* If `annotate = true` in the config, the output for each will be annotated with the grid parameter values and the replicate number. If `annotate = false`, only the replicate number will be recorded. Use this if you want to annotate the table yourself from inside slim, e.g. if you want to record transformed parameters.
