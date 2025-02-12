use cran_description_file_parser::{
    from_str, parse_description_file, strip_indents, Field, RVersion,
};
use miette::IntoDiagnostic;

pub struct FieldWrapper<'a>(Vec<Field<'a>>);

impl<'a> FieldWrapper<'a> {
    pub fn name(&self) -> Option<&str> {
        for field in &self.0 {
            if let Field::Package(name) = field {
                return Some(name.trim());
            }
        }
        None
    }
    pub fn system_requirements(&self) -> Option<String> {
        for field in &self.0 {
            if let Field::Any {
                key: "SystemRequirements",
                value,
            } = field
            {
                return Some(strip_indents(value.trim()));
            }
        }
        None
    }
    pub fn version(&self) -> Option<&RVersion> {
        for field in &self.0 {
            if let Field::Version(version) = field {
                return Some(version);
            }
        }
        None
    }
}

pub struct Package {
    name: String,
    version: RVersion<'static>,
    system_dependencies: String,
}
fn main() -> miette::Result<()> {
    let read = fs_err::read_dir("./description_files/").into_diagnostic()?;

    for file in read {
        let file = file.into_diagnostic()?;
        let file_type = file.file_type().into_diagnostic()?;

        let mut packages = vec![];
        if file_type.is_file() {
            let description_contents = fs_err::read_to_string(file.path()).into_diagnostic()?;

            let parsed = FieldWrapper(from_str(&description_contents).unwrap());

            let parsed_name = parsed.name().unwrap();
            let parsed_version = parsed.version().unwrap();
            let Some(parsed_sysreqs) = parsed.system_requirements() else {
                continue;
            };

            println!("{} {} {}", parsed_name, parsed_version, parsed_sysreqs)
        }
    }
    Ok(())
}
