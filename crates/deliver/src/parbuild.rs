use std::{
    ffi::{OsStr, OsString},
    process::{Child, Stdio},
};

use crate::cran::CranPackagesFileDownloadObject;

pub struct RPackageBuilder {
    path: std::path::PathBuf,
    flags: Vec<String>,
    library_install_path: std::path::PathBuf,
    library_source_path: std::path::PathBuf,
    env: Vec<(OsString, OsString)>,
    build: bool,
}

#[derive(Debug, Copy, Clone)]
pub enum IOMode {
    Piped,
    Inherited,
    Null,
}

impl IOMode {
    pub fn as_stdio(self) -> Stdio {
        match self {
            IOMode::Piped => Stdio::piped(),
            IOMode::Inherited => Stdio::inherit(),
            IOMode::Null => Stdio::null(),
        }
    }
}

impl RPackageBuilder {
    /// Shells out to R to indicate that it should
    /// compile / install this package. It would be
    /// interesting to not have to shell out here,
    /// but I'm worried that that would lose out
    /// to future changes that R could make to packaging
    /// and a great deal of the R packaging things are written
    /// in base R itself, so it's not particularly clear
    /// how easy or straightforward it would be to emulate since
    /// building could query a *specific* R installation, and I think
    /// you need an R interpreter to install some packages anyways.
    /// Again, would be interesting, just isn't all that clear.
    pub fn install_r_package(&self, io_mode: IOMode) -> std::io::Result<std::process::Output> {
        // TODO: use toolchain instead of
        // PATH selection.

        let mut r_cmd = std::process::Command::new("R");

        r_cmd.args(&["CMD", "INSTALL"]);
        // TODO: is there a better way to possibly manage env vars without
        // falling back to UTF8? Maybe bstr?
        r_cmd.env(
            "PATH",
            [
                std::env::current_dir()?
                    .join(".prefix")
                    .join("bin")
                    .to_string_lossy()
                    .as_ref(),
                "/bin",
                "/usr/bin",
            ]
            .join(":"),
        );

        for (key, value) in &self.env {
            r_cmd.env(key, value);
        }

        r_cmd.arg(&self.library_source_path);

        if self.build {
            r_cmd.arg("--build");
        }

        // TODO: again, doing this without utf8 would
        // be really nice.
        r_cmd.arg(format!(
            "--library={}",
            self.library_install_path.to_string_lossy()
        ));

        r_cmd.stdin(io_mode.as_stdio());
        r_cmd.stdout(io_mode.as_stdio());
        r_cmd.stderr(io_mode.as_stdio());
        let cmd = r_cmd.spawn()?;

        let output = cmd.wait_with_output()?;

        Ok(output)
    }
}
