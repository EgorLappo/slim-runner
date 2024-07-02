use clap::Parser;
use color_eyre::eyre::Result;
use indicatif::MultiProgress;
use jemallocator::Jemalloc;
use log::info;
use std::path::PathBuf;

#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

mod grid;

use grid::Grid;

#[derive(Parser, Debug)]
#[command(author, version, about = "Launch SLiM grid runs", long_about = None)]
struct Opts {
    #[clap(default_value = "slim_config.toml")]
    config: PathBuf,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let logger =
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).build();
    let bar = MultiProgress::new();
    indicatif_log_bridge::LogWrapper::new(bar.clone(), logger)
        .try_init()
        .unwrap();

    let opts = Opts::parse();

    info!("Starting SLiM grid run with config: {:?}", opts.config);

    let grid = Grid::new(opts.config)?;
    grid.run(bar)
}
