use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use chumsky::Parser;
use miette::LabeledSpan;
use text::{inline_whitespace, newline};

/// Strips leading and trailing whitespace from
/// every line in a block of text. This is useful
/// because the DESCRIPTION file (Debian Control)
/// uses indents to declare that the field
/// is wrapping onto the next line. This function
/// can be used to strip those indents, and return
/// the text to it's intended meaning. The reason
/// this isn't done automatically is because it
/// requires an allocation for every string, and
/// the literal contents of the field suffice
/// for a great many use cases.
pub fn strip_indents(input: &str) -> String {
    input
        .lines()
        .map(|line| line.trim())
        .collect::<Vec<_>>()
        .join("\n")
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VersionSeperator {
    Dot,
    Dash,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RVersion<'a> {
    pub components: Vec<&'a str>,
    pub separators: Vec<VersionSeperator>,
}

impl<'a> std::fmt::Display for RVersion<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (component, sep) in self.components.iter().zip(
            self.separators
                .iter()
                .map(Some)
                .chain(std::iter::once(None)),
        ) {
            f.write_str(component)?;
            if let Some(sep) = sep {
                match sep {
                    VersionSeperator::Dot => f.write_str(".")?,
                    VersionSeperator::Dash => f.write_str("-")?,
                }
            }
        }
        Ok(())
    }
}

/// The ord implementation is based on the R
/// utils::compareVersions from R, and should
/// have the exact same characteristics. Any deviation
/// is considered a bug
impl<'a> PartialOrd for RVersion<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let self_iter = self.components.iter().map(|c| c.parse::<i64>().ok());
        let mut other_iter = other.components.iter().map(|c| c.parse::<i64>().ok());

        for a_k in self_iter {
            if let Some(b_k) = other_iter.next() {
                // in R, if a component is not a number,
                // then the items are not comparable.
                let Some((a_k, b_k)) = a_k.zip(b_k) else {
                    return None;
                };

                if a_k > b_k {
                    return Some(std::cmp::Ordering::Greater);
                } else if a_k < b_k {
                    return Some(std::cmp::Ordering::Less);
                }
            } else {
                return Some(std::cmp::Ordering::Greater);
            }
        }

        if other.components.len() > self.components.len() {
            return Some(std::cmp::Ordering::Less);
        } else {
            return Some(std::cmp::Ordering::Equal);
        }
    }
}

#[derive(Debug, Clone)]
pub enum Field<'a> {
    Package(&'a str),
    Type(&'a str),
    Title(&'a str),
    Description(&'a str),
    License(&'a str),
    Version(RVersion<'a>),
    RoxygenNote(&'a str),
    Depends(Vec<Dependency<'a>>),
    Imports(Vec<Dependency<'a>>),
    NeedsCompilation(bool),
    LinkingTo(Vec<Dependency<'a>>),
    Suggests(Vec<Dependency<'a>>),
    ErrorLine(&'a str),
    AuthorsR(&'a str),
    Any { key: &'a str, value: &'a str },
}

// we need to be able to decide what to parse
// each field with since each has a slightly
// different format, and it matters that
// we can parse them semantically.

#[derive(Copy, Clone, Debug)]
pub enum KeyImpliesValueType<'a> {
    Valued,
    Empty(&'a str),
}

pub fn within_value_ws_certain<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>>
{
    inline_whitespace()
        .at_least(1)
        .to_slice()
        .or(text::newline()
            .repeated()
            .exactly(1)
            .then(inline_whitespace().at_least(1))
            .to_slice())
}

pub fn parse_key<'a>(
) -> impl Parser<'a, &'a str, (&'a str, KeyImpliesValueType<'a>), extra::Err<Rich<'a, char>>> {
    // I believe if key's have a
    // value after them, then they need
    // at least a little bit of whitespace
    // to denote this.
    any()
        .filter(|c: &char| c != &':' && !c.is_whitespace())
        .repeated()
        .at_least(1)
        .to_slice()
        .then_ignore(inline_whitespace())
        .then_ignore(just(':'))
        .then(choice((
            // if a field has inline whitespace, then it must be valued.
            inline_whitespace()
                .at_least(1)
                .to(KeyImpliesValueType::Valued),
            // or if it has a newline followed by inline whitespace, then
            // it must be valued,
            newline()
                .then(inline_whitespace().at_least(1))
                .to(KeyImpliesValueType::Valued),
            // if a field has neither a whitespace nor a newline,
            // then we can say for sure the field is empty.
            empty().to_slice().map(KeyImpliesValueType::Empty),
        )))
        .labelled("key")
        .as_context()
}

pub fn parse_version<'a>() -> impl Parser<'a, &'a str, RVersion<'a>, extra::Err<Rich<'a, char>>> {
    let any_version_component = any()
        .filter(|c: &char| c.is_alphabetic() || c.is_numeric() || *c == '_' || *c == '+')
        .repeated()
        .at_least(1)
        .to_slice()
        .padded_by(debian_ws());

    any_version_component
        .then(
            choice((
                just('.').to(VersionSeperator::Dot),
                just('-').to(VersionSeperator::Dash),
            ))
            .or_not(),
        )
        .repeated()
        .at_least(1)
        .collect()
        .map(|items: Vec<_>| {
            let mut version_parts = Vec::new();
            let mut separators = Vec::new();
            for (ver, sep) in items {
                version_parts.push(ver);
                if let Some(sep) = sep {
                    separators.push(sep);
                }
            }
            RVersion {
                components: version_parts,
                separators,
            }
        })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RVersionOrdering {
    EQ,
    GT,
    GE,
    LT,
    LE,
}

impl RVersionOrdering {
    pub fn as_str(&self) -> &'static str {
        match self {
            RVersionOrdering::EQ => "==",
            RVersionOrdering::GT => ">",
            RVersionOrdering::GE => ">=",
            RVersionOrdering::LT => "<",
            RVersionOrdering::LE => "<=",
        }
    }
}

pub fn parse_version_ordering<'a>(
) -> impl Parser<'a, &'a str, RVersionOrdering, extra::Err<Rich<'a, char>>> {
    choice((
        just("==").map(|_| RVersionOrdering::EQ),
        just(">=").map(|_| RVersionOrdering::GE),
        just("<=").map(|_| RVersionOrdering::LE),
        just(">>").or(just(">")).map(|_| RVersionOrdering::GT),
        just("<<").or(just("<")).map(|_| RVersionOrdering::LT),
    ))
}

pub fn parse_version_constraint<'a>(
) -> impl Parser<'a, &'a str, (RVersionOrdering, RVersion<'a>), extra::Err<Rich<'a, char>>> {
    just('(')
        .ignore_then(parse_version_ordering())
        .then_ignore(debian_ws())
        .then(parse_version())
        .then_ignore(just(')'))
}

pub fn parse_dependency<'a>() -> impl Parser<'a, &'a str, Dependency<'a>, extra::Err<Rich<'a, char>>>
{
    // so we are going to parse an ident, that's a valid package name
    // then we're going to consume any optional whitespace,
    // then we'll see if there are parenthesis, and if there is than
    // we'll parse the version contained inside of them.

    any()
        .filter(|&c: &char| c.is_alphanumeric() || c == '-' || c == '.')
        .repeated()
        .at_least(1)
        .to_slice()
        .then_ignore(debian_ws())
        .then(parse_version_constraint().or_not())
        .map(|(name, constraint)| Dependency {
            name,
            constraint: constraint
                .map(|(ordering, version)| VersionConstraint { ordering, version }),
        })
}

pub fn parse_field<'a>() -> impl Parser<'a, &'a str, Field<'a>, extra::Err<Rich<'a, char>>> {
    custom(|inp| {
        let (key, implies) = inp.parse(parse_key())?;

        match implies {
            KeyImpliesValueType::Valued => match key {
                "Package" => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::Package(value))
                }
                "Authors@R" => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::AuthorsR(value))
                }
                "Title" => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::Title(value))
                }
                "Type" => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::Type(value))
                }
                "Description" => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::Description(value))
                }
                "License" => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::License(value))
                }
                "Depends" => {
                    let value = inp.parse(parse_dependency_list())?;
                    Ok(Field::Depends(value))
                }
                "Imports" => {
                    let value = inp.parse(parse_dependency_list())?;
                    Ok(Field::Imports(value))
                }
                "Suggests" => {
                    let value = inp.parse(parse_dependency_list())?;
                    Ok(Field::Suggests(value))
                }
                "LinkingTo" => {
                    let value = inp.parse(parse_dependency_list())?;
                    Ok(Field::LinkingTo(value))
                }
                "RoxygenNote" => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::RoxygenNote(value))
                }
                "NeedsCompilation" => {
                    let value = inp.parse(
                        just("yes")
                            .map(|_| true)
                            .or(just("no").map(|_| false))
                            .padded_by(debian_ws()),
                    )?;
                    Ok(Field::NeedsCompilation(value))
                }
                "Version" => inp.parse(parse_version().map(|version| Field::Version(version))),
                _ => {
                    let value = inp.parse(parse_any_value())?;
                    Ok(Field::Any { key, value })
                }
            },
            KeyImpliesValueType::Empty(emp) => match key {
                "Package" => Ok(Field::Package(emp)),
                "Title" => Ok(Field::Title(emp)),
                "Type" => Ok(Field::Type(emp)),
                "Description" => Ok(Field::Description(emp)),
                "License" => Ok(Field::License(emp)),
                "Depends" => Ok(Field::Depends(vec![])),
                "Imports" => Ok(Field::Imports(vec![])),
                "Suggests" => Ok(Field::Suggests(vec![])),
                "LinkingTo" => Ok(Field::LinkingTo(vec![])),
                "RoxygenNote" => Ok(Field::RoxygenNote(emp)),
                "Version" => Ok(Field::Version(RVersion {
                    components: vec![],
                    separators: vec![],
                })),
                _ => Ok(Field::Any { key, value: emp }),
            },
        }
    })
}

pub fn parse_any_value<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    any()
        .and_is(text::newline().not())
        .repeated()
        .labelled("line of values")
        .as_context()
        .separated_by(within_value_ws_certain())
        .padded_by(debian_ws())
        .to_slice()
        .labelled("field value")
        .as_context()
}

pub fn parse_description_file<'a>(
) -> impl Parser<'a, &'a str, Vec<Field<'a>>, extra::Err<Rich<'a, char>>> {
    parse_field()
        .recover_with(via_parser(
            any()
                .and_is(just('\n').not())
                .repeated()
                .at_least(1)
                .then(just('\n'))
                .to_slice()
                .map(|res| Field::ErrorLine(res)),
        ))
        .separated_by(text::newline().repeated())
        .collect()
        .then_ignore(newline().or(end()).or_not())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VersionConstraint<'a> {
    pub ordering: RVersionOrdering,
    pub version: RVersion<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency<'a> {
    pub name: &'a str,
    pub constraint: Option<VersionConstraint<'a>>,
}

/// debian whitespace is a tricky beast, because it's either like ' '
/// or it's like ' \n\t' or like '\n\t' or like '\n  ', and it seems
/// to be fine to just split a bunch of seemingly atomic concepts
/// this way :shrug:
pub fn debian_ws<'a>() -> impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char>>> {
    inline_whitespace().then_ignore(
        text::newline()
            .repeated()
            .exactly(1)
            .then(inline_whitespace().at_least(1))
            .or_not(),
    )
}
pub fn parse_dependency_list<'a>(
) -> impl Parser<'a, &'a str, Vec<Dependency<'a>>, extra::Err<Rich<'a, char>>> {
    debian_ws().ignore_then(
        parse_dependency()
            .labelled("parsing dependency")
            .separated_by(just(',').padded_by(debian_ws()))
            .allow_trailing()
            .collect()
            .labelled("parsing dependency list")
            .as_context(),
    )
}

pub fn from_str(file: &str) -> Result<Vec<Field<'_>>, Vec<Rich<'_, char, SimpleSpan>>> {
    parse_description_file().parse(file).into_result()
}

pub fn rich_to_miette_inline(
    source_code: String,
) -> impl Fn(Vec<Rich<'_, char, SimpleSpan>>) -> Vec<miette::Report> {
    move |rich_errs: Vec<Rich<'_, char, SimpleSpan>>| rich_to_miette(rich_errs, source_code.clone())
}

pub fn rich_to_miette(
    rich_errs: Vec<Rich<'_, char, SimpleSpan>>,
    source_code: String,
) -> Vec<miette::Report> {
    rich_errs
        .into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .map(|e| {
            let primary = LabeledSpan::new_primary_with_span(
                Some(e.to_string()),
                e.span().start..e.span().end,
            );

            let contexts = e.contexts().map(|(label, span)| {
                LabeledSpan::new(Some(label.to_string()), span.start, span.end)
            });

            let mut combined = Vec::new();

            combined.push(primary);
            combined.extend(contexts);

            let res = miette::miette!(
                severity = miette::Severity::Error,
                labels = combined,
                "Failed to Parse CRAN PACKAGES File: {}",
                e.to_string(),
            )
            .with_source_code(source_code.clone());

            res
        })
        .collect::<Vec<_>>()
}

pub fn pretty_errors(file: &str, errs: Vec<Rich<'_, char, SimpleSpan>>) {
    let filename = "DESCRIPTION";

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .for_each(|e| {
            let report = Report::build(
                ReportKind::Error,
                (filename, e.span().start..e.span().end()),
            )
            .with_message(e.to_string())
            .with_label(
                Label::new((filename, e.span().start()..e.span().end()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new((filename, span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish();

            report.print(sources([(filename, file)])).unwrap();
        });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_parse() {
        let version = "1.0-0";
        println!("{:#?}", parse_version().parse(version));
    }

    #[test]
    fn test_version() {
        let version = "ggplot2 (<= 4.0)";

        println!("{:#?}", parse_dependency().parse(version));
    }

    #[test]
    fn test_dependency_list() {
        let version_list = "ggplot2,\n    janitor (>> 2.0),\n    magrittr (<< 4.0),\n    dplyr,\n    stats,\n    purrr,\n    rstanarm,\n    e1071,\n    groupdata2";

        println!("{:#?}", parse_dependency_list().parse(version_list));
    }

    #[test]
    fn test_empty_depends() {
        let file = r#"Package: xtable
Version: 1.0-1
Date: 2000/12/08
Title: Export tables
Author: David Dahl <dbdahl@stat.wisc.edu>
Description: Coerce data to LaTeX and HTML tables
Depends:
License: GPL version 2 or later"#;

        println!("{:#?}", parse_description_file().parse(file));
    }

    #[test]
    pub fn incorrect_line() {
        // ideally an incorrect line won't fully fail a parse
        let file = r#"Package: hello
THIS LINE IS WRONG
Version: 1.0
"#;
        let res = parse_description_file().parse(file);
        match res.output() {
            Some(vals) => dbg!(vals),
            None => {
                panic!("partially incorrect file generated no outputs");
            }
        };
    }
    #[test]
    pub fn wrong_indent() {
        // ideally an incorrect line won't fully fail a parse
        let file = r#"  What: THIS LINE IS WRONG
Version: 1.0
"#;
        let res = parse_description_file().parse(file);
        match res.output() {
            Some(vals) => dbg!(vals),
            None => {
                panic!("partially incorrect file generated no outputs");
            }
        };
    }

    #[test]
    pub fn active_pathways() {
        let file = r#"Package: ActivePathways
URL:
BugReports: https://github.com/reimandlab/ActivePathways/issues
"#;
        match parse_description_file().parse(file).into_result() {
            Ok(fields) => {
                for field in fields {
                    println!("{:#?}", field);
                }
            }
            Err(errs) => {
                pretty_errors(file, errs);
            }
        };
    }

    #[test]
    pub fn test_colon_in_field() {
        let content = r#"Where: This
Package: ActivePathways: Hello
This: here.
"#;
        match parse_description_file().parse(content).into_result() {
            Ok(fields) => {
                for field in fields {
                    println!("{:#?}", field);
                }
            }
            Err(errs) => {
                pretty_errors(content, errs);
            }
        };
    }

    #[test]
    pub fn test_space_after_key() {
        let content = r#"Package : This
Package: ActivePathways: Hello
This: here.
"#;
        match parse_description_file().parse(content).into_result() {
            Ok(fields) => {
                for field in fields {
                    println!("{:#?}", field);
                }
            }
            Err(errs) => {
                pretty_errors(content, errs);
            }
        };
    }

    #[test]
    pub fn test_r_authors() {
        let content = include_str!("../tricky-test-cases/Matrix");
        match parse_description_file().parse(content).into_result() {
            Ok(fields) => {
                for field in fields {
                    println!("{:#?}", field);
                }
            }
            Err(errs) => {
                pretty_errors(content, errs);
            }
        };
    }

    #[test]
    pub fn test_r_authors_2() {
        let content = include_str!("../tricky-test-cases/cluster");
        match parse_description_file().parse(content).into_result() {
            Ok(fields) => {
                for field in fields {
                    println!("{:#?}", field);
                }
            }
            Err(errs) => {
                pretty_errors(content, errs);
            }
        };
    }
}
