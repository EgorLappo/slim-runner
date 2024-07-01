## `SLiM` simulation wrapper

This is a small program to wrap SLiM grid runs. It targets cases in which per-cycle output is the primary output of the simulation. The `LogFile` output is automatically consumed by this wrapper, annotated with parameter values, and merged into a larger file. With shorter simulations, this saves a lot of time spend on disk writes and compression.

### Config format

Configuration is done through a `.toml` file that contains several top-level tables.

The table *`settings`* contains global parameters such as SLiM script location, number of available cores, etc. 

``` toml
[settings]
// number of cores to use in parallel runs
cores = 6
// slim script file
script = "script.slim"
// number of replicate simulation for each set of parameters
replicates = 100
// global seed for the run
seed = 231
```

The parameters are set in the *`grid`* table. Valid parameter values are a string, a number, a list of number, and a range of values.

``` toml

```

