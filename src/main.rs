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
    #[arg(
        value_name = "CONFIG_PATH",
        default_value = "slim_config.toml",
        help = "config TOML file"
    )]
    config: PathBuf,
    #[arg(
        value_name = "N_CHUNKS",
        help = "number of chunks if evaluating only part of the grid is desired (optional)"
    )]
    nchunks: Option<usize>,
    #[arg(
        value_name = "CHUNK",
        help = "index of chunk to evaluate now (optional)"
    )]
    chunk: Option<usize>,
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

    // only if two entries are present, use chunking in evaluation
    let chunking = match (opts.nchunks, opts.chunk) {
        (Some(nchunks), Some(chunk)) => {
            if (chunk < nchunks) & (nchunks > 1) {
                info!("Only evaluating chunk {} of {}", chunk, nchunks);
                Some((nchunks, chunk))
            } else {
                color_eyre::eyre::bail!(
                    "invalid chunk settings {}/{}: N_CHUNKS > 1, 0 <= CHUNK < N_CHUNKS is required",
                    chunk,
                    nchunks
                )
            }
        }
        _ => None,
    };

    info!("Starting SLiM grid run with config: {:?}", opts.config);

    let grid = Grid::new(opts.config, chunking)?;
    grid.run(bar)
}
