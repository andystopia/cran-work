//! This module intends to implement toolchain
//! management for R. I should be able to do
//! deliver toolchain add R, and have it download
//! and install a working copy of R onto my machine.
//!
//! I don't want to have to go to the website, or get
//! gfortran or worry about system capabilities or any
//! of that, I just want an easy way to to install R,
//! without any of the headaches, and this should be a
//! somewhat optimal way of doing that using the conda
//! ecosystem, and a few protected versions of R
//!
//! I think that having the R install be global a la
//! Cargo makes sense because the language itself is
//! quite a big dependency and it's invariant in the
//! rest of the dependencies, so it makes sense to factor
//! it out into the global environment, and it's just
//! convenient to have a good version on the path.
//!
//! I would also imagine being able to do deliver toolchain update R
//! would make it a lot easier to upgrade R when the time comes,
//! so we will be sure that that is also possible. We will no
//! longer need to create such intricate environments when we install R.
//!
//!
//! A bit of the harder part is that we will need aa way to install
//! "trampolines", like Cargo does for Rust. These are honestly super
//! handy and something that I think every ecosystem should at least
//! give a try to. They are really *really* nice, for this sort of thing.
//!
//! Oh one other perk of having the R enviroment seperate is removing
//! false dependency conflicts between system libraries, R will use
//! pcre, but so will some other package in the ecosystem, so it makes
//! sense to keep R seperate so that it can be used with minimal conflicts.
use clap::Subcommand;
use fs_err::create_dir_all;
use nix::{
    sys::signal::{self, Signal},
    unistd::Pid,
};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use std::{
    io::Write,
    path::PathBuf,
    process::{ExitStatus, Stdio},
};
use tokio::signal::ctrl_c;

use miette::{bail, Context, IntoDiagnostic};

struct PathContainer {
    home: PathBuf,
    install_dir: PathBuf,
    toolchain_dir: PathBuf,
    bin_dir: PathBuf,
}

fn construct_path_container() -> miette::Result<PathContainer> {
    let Some(home) = std::env::var_os("HOME") else {
        bail!("Failed to get HOME environment variable");
    };

    let home = PathBuf::from(home);

    let install_dir = home.join(".deliver/");
    let bin_dir = install_dir.join("bin/");
    let toolchain_dir = install_dir.join("toolchains/");

    Ok(PathContainer {
        home,
        install_dir,
        bin_dir,
        toolchain_dir,
    })
}

/// ensures that all paths that could be
/// used by the installation all exist.
fn construct_install_directory_structure(pc: &PathContainer) -> miette::Result<()> {
    create_dir_all(&pc.install_dir).into_diagnostic()?;
    create_dir_all(&pc.bin_dir).into_diagnostic()?;
    create_dir_all(&pc.toolchain_dir).into_diagnostic()?;
    Ok(())
}

fn write_env_script(paths: &PathContainer) -> miette::Result<()> {
    // lifted straight from cargo.
    let script = r#"#!/bin/sh
# deliver shell setup
# affix colons on either side of $PATH to simplify matching
case ":${PATH}:" in
    *:"$HOME/.deliver/bin":*)
        ;;
    *)
        # Prepending path in case a system-installed deliver needs to be overridden
        export PATH="$HOME/.deliver/bin:$PATH"
        ;;
esac
    "#;

    fs_err::write(paths.install_dir.join("env"), script).into_diagnostic()?;

    Ok(())
}

fn add_to_zsh(pc: &PathContainer) -> miette::Result<()> {
    // .. the line that we want to add to our configuration
    let add_line = r#". "$HOME/.deliver/env""#;

    // the file we want to add the line to.
    let zshenv_file = pc.home.join(".zshenv");

    // but before we add the line, let's make that
    // the file that we want exists, and that it's not
    // already in there.
    if zshenv_file.exists() && zshenv_file.is_file() {
        let zshenv = fs_err::read_to_string(&zshenv_file).into_diagnostic()?;

        // if the line is already present then bail early
        if zshenv.lines().any(|line| line == add_line) {
            return Ok(());
        }
    }

    // carefully construct the bounds for opening
    // the file, and then open it. We only want to
    // create the file if it does already exist, and then
    // we want to say we're going to append and write to
    // it
    let mut oo = fs_err::OpenOptions::new()
        .append(true)
        .write(true)
        .create(!zshenv_file.exists())
        .open(zshenv_file)
        .into_diagnostic()?;

    // now add the line to the file
    oo.write_all(add_line.as_bytes()).into_diagnostic()?;
    oo.write_all(&['\n' as u8]).into_diagnostic()?;

    oo.flush().into_diagnostic()?;

    Ok(())
}

#[derive(Subcommand, Debug)]
/// the toolchain command manages
/// installations of R for you, and allows
/// for a declarative and easy way to install
/// R in a flexible way which will allow substituting
/// out versions later.
pub enum ToolchainCli {
    /// Create all the necessary directories for
    /// this program and setup some shell integrations
    /// (currently on zsh), hopefully we'll be able to
    /// add more in the future.
    Setup,
    ListRVersions,
    List,
    Add {
        /// the R version to install,
        /// if this isn't present, then we'll
        /// install the latest version from the conda-forge.
        /// The conda-forge may lag the CRAN or source R
        /// distributions by a few days, while it builds R
        /// and while the conda-forge prepare and the test the
        /// release. You can elect this as 'latest' to install
        /// the latest version of R that is available.
        r_version: String,
    },
}

fn create_r_symlink(pc: &PathContainer) -> miette::Result<()> {
    // we want to symlink both R and Rscript to deliver
    // so that it can proxy them in the future, for nice,
    // fast easy installs.

    let r_proxy = &pc.bin_dir.join("R");
    let r_script_proxy = &pc.bin_dir.join("Rscript");

    if !r_proxy.exists() {
        std::os::unix::fs::symlink(&pc.bin_dir.join("deliver"), &r_proxy).into_diagnostic()?;
    }
    if !r_script_proxy.exists() {
        std::os::unix::fs::symlink(&pc.bin_dir.join("deliver"), &r_script_proxy)
            .into_diagnostic()?;
    }
    Ok(())
}

#[allow(dead_code)]
pub struct RMostRecentVersionApi {
    package_name: String,
    url: Url,
}

fn conda_most_recent_version_api() -> RMostRecentVersionApi {
    RMostRecentVersionApi {
        package_name: "r-base".to_owned(),
        url: Url::parse("https://api.anaconda.org/package/conda-forge/r-base")
            .expect("the hard coded url to the conda-forge r-base should be correct, or made to be correct. I don't expect a runtime failure to parse from this"),
    }
}

pub struct AvailableRVersions {
    most_recent: String,
    all_versions: Vec<String>,
}

async fn get_most_recent_r_version(
    r_api: &RMostRecentVersionApi,
) -> miette::Result<AvailableRVersions> {
    let response = reqwest::get(r_api.url.clone()).await.into_diagnostic()?;

    let body = response.text().await.into_diagnostic()?;

    let parsed: serde_json::Value = serde_json::from_str(&body).into_diagnostic()?;

    let latest_version = parsed
        .get("latest_version")
        .context("r version api response was expected to contain latest_version but it did not.")?
        .as_str()
        .context("r version api latest version was expected to be a string, but it was not")?
        .to_owned();

    let all_versions = parsed
        .get("versions")
        .context("r version api has no key versions")?
        .as_array()
        .context("r version api versions was not an array, but was expected to be one")?
        .into_iter()
        .map(|el| el.as_str().map(ToOwned::to_owned))
        .collect::<Option<Vec<_>>>()
        .context("not all versions were strings")?;

    Ok(AvailableRVersions {
        most_recent: latest_version,
        all_versions,
    })
}

fn setup() -> miette::Result<()> {
    let pc = construct_path_container()?;
    construct_install_directory_structure(&pc)?;
    create_r_symlink(&pc)?;
    write_env_script(&pc)?;
    add_to_zsh(&pc)?;

    Ok(())
}

async fn install_version_of_r(r_version: &str, pc: &PathContainer) -> miette::Result<ExitStatus> {
    let destination_path = pc.toolchain_dir.join(r_version);

    if destination_path.exists() {
        bail!("R Version {r_version} is already installed :) Exiting.");
    }

    let mut cmd = tokio::process::Command::new("rattler");
    cmd.arg("create");

    cmd.arg(format!("r-base=={r_version}"));
    cmd.args(&["compilers", "sed", "pkg-config", "openblas", "make"]);
    cmd.arg("--target-prefix");
    cmd.arg(destination_path.clone());

    cmd.stdout(Stdio::inherit());
    cmd.stderr(Stdio::inherit());

    let mut running = cmd.spawn().into_diagnostic()?;

    let exit_status = running.wait().await.into_diagnostic()?;

    if exit_status.success() {
        let mut toolchains = ToolchainsFile::load(pc)?;
        toolchains.toolchain.push(ToolchainPaths {
            path: destination_path
                .to_str()
                .expect("only utf8 install paths supported currently")
                .to_owned(),
            version: r_version.to_owned(),
        });
        toolchains.save(pc)?;
    }

    Ok(exit_status)
}

pub async fn invoke_r() -> miette::Result<()> {
    let pc = construct_path_container()?;
    let args = std::env::args_os().skip(1).collect::<Vec<_>>();

    let toolchains = ToolchainsFile::load(&pc)?;
    let Some(latest_toolchain) = toolchains.toolchain.first() else {
        bail!("there doesn't appear to be an R version available. You can install the latest version by running `deliver toolchain add latest`");
    };

    let command = std::path::PathBuf::from(&latest_toolchain.path).join("bin/R");

    let mut command = tokio::process::Command::new(command);

    command.args(args);

    let mut running = command.spawn().into_diagnostic()?;

    let exit_status = loop {
        tokio::select! {
            output = running.wait() => {
                let exit_status = output.into_diagnostic()?;
                let exit_status = exit_status.code().unwrap_or(0);

                break exit_status;
            }
            _ = ctrl_c() => {
                // R doesn't actually seem to really care very much
                // much about receiving a SIGINT, so we'll send one to it
                // because it's polite, I guess, but I think it just eats them.
                // if some future / past version that exits, then wait
                // will be the next resolved future, so this works
                // just fine.
                signal::kill(Pid::from_raw(running.id().unwrap() as i32), Signal::SIGINT).into_diagnostic()?;
            }
        }
    };

    std::process::exit(exit_status);
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ToolchainPaths {
    path: String,
    version: String,
}

impl ToolchainPaths {
    pub fn cmp_versions(&self, other: &ToolchainPaths) -> std::cmp::Ordering {
        let self_sections = self
            .version
            .split('.')
            .map(|v| v.parse::<u16>().expect("R versions are purely numerical"));
        let other_sections = other
            .version
            .split('.')
            .map(|v| v.parse::<u16>().expect("R versions are purely numerical"));

        self_sections
            .zip(other_sections)
            .find_map(|(s, o)| match s.cmp(&o) {
                std::cmp::Ordering::Equal => None,
                v => Some(v),
            })
            .unwrap_or(std::cmp::Ordering::Equal)
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ToolchainsFile {
    toolchain: Vec<ToolchainPaths>,
}

impl ToolchainsFile {
    fn toolchains_file_path(pc: &PathContainer) -> std::path::PathBuf {
        pc.toolchain_dir.join("toolchains.toml")
    }
    fn load(pc: &PathContainer) -> Result<ToolchainsFile, miette::Error> {
        let toml_path = Self::toolchains_file_path(pc);

        if !toml_path.exists() {
            return Ok(Self::default());
        }

        let toml_contents = fs_err::read_to_string(toml_path).into_diagnostic()?;
        let mut toolchains: ToolchainsFile =
            toml_edit::de::from_str(&toml_contents).into_diagnostic()?;

        toolchains
            .toolchain
            .sort_by(|a, b| a.cmp_versions(b).reverse());

        Ok(toolchains)
    }

    fn save(&self, pc: &PathContainer) -> Result<(), miette::Error> {
        let as_str = toml_edit::ser::to_string(self).into_diagnostic()?;
        let toml_file = Self::toolchains_file_path(pc);

        std::fs::write(toml_file, as_str).into_diagnostic()?;
        Ok(())
    }
}

fn list_toolchains(pc: &PathContainer) -> miette::Result<()> {
    let toolchains = ToolchainsFile::load(pc)?;

    if toolchains.toolchain.is_empty() {
        println!("no installed toolchains.\n");
        println!("you can install the most recent version of R with");
        println!("`deliver toolchain add latest`");

        return Ok(());
    }

    println!("====================");
    println!("Installed Toolchains");
    println!("====================");
    for toolchain in toolchains.toolchain {
        println!(" - R {} ({})", toolchain.version, toolchain.path);
    }

    Ok(())
}

pub async fn handle_toolchain(args: ToolchainCli) -> miette::Result<()> {
    match args {
        ToolchainCli::List => {
            let pc = construct_path_container()?;
            list_toolchains(&pc)?;
            Ok(())
        }
        ToolchainCli::Setup => setup(),
        ToolchainCli::Add { r_version } => {
            let r_version = if r_version == "latest" {
                let provider = conda_most_recent_version_api();
                let version_information = get_most_recent_r_version(&provider).await?;

                version_information.most_recent
            } else {
                r_version
            };

            let pc = construct_path_container()?;
            construct_install_directory_structure(&pc)?;

            let status = install_version_of_r(&r_version, &pc).await?;

            // propagate up the exit status of the rattler
            std::process::exit(status.code().unwrap_or(0));
        }
        ToolchainCli::ListRVersions => {
            let provider = conda_most_recent_version_api();
            let version_information = get_most_recent_r_version(&provider).await?;

            for version in version_information.all_versions.iter().rev() {
                if version == &version_information.most_recent {
                    println!("- R@{version} (most recent version)")
                } else {
                    println!("- R@{version}")
                }
            }
            Ok(())
        }
    }
}
