use cran_description_file_parser::RVersionOrdering;
use cran_description_file_parser::VersionConstraint;

use cran_description_file_parser::RVersion;

#[derive(Debug, Clone)]
pub struct VersionRangeBound<'a> {
    pub(crate) version: RVersion<'a>,
    pub(crate) included: bool,
}

#[derive(Debug, Clone)]
pub struct PackageBoundsConstraint<'a> {
    pub(crate) lower: Option<VersionRangeBound<'a>>,
    pub(crate) upper: Option<VersionRangeBound<'a>>,
}

impl<'a> PackageBoundsConstraint<'a> {
    pub fn new() -> Self {
        Self {
            lower: None,
            upper: None,
        }
    }
    pub fn is_singleton(&self) -> bool {
        if let Some((lower, upper)) = self.lower.as_ref().zip(self.upper.as_ref()) {
            lower.version == upper.version && lower.included && upper.included
        } else {
            false
        }
    }

    pub fn get_singleton(&self) -> Option<&RVersion> {
        if let Some((lower, upper)) = self.lower.as_ref().zip(self.upper.as_ref()) {
            if lower.version == upper.version && lower.included && upper.included {
                Some(&lower.version)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn create_only(version: RVersion<'a>) -> Self {
        let mut se = Self::new();

        se.only(version);
        return se;
    }
    pub fn only(&mut self, version: RVersion<'a>) {
        self.lower = Some(VersionRangeBound {
            version: version.clone(),
            included: true,
        });
        self.upper = Some(VersionRangeBound {
            version,
            included: true,
        });
    }

    pub fn subset(&mut self, constraint: &VersionConstraint<'a>) {
        let inclusive = match constraint.ordering {
            RVersionOrdering::EQ => true,
            RVersionOrdering::GT => false,
            RVersionOrdering::GE => true,
            RVersionOrdering::LT => false,
            RVersionOrdering::LE => true,
        };

        match constraint.ordering {
            RVersionOrdering::EQ => {
                self.lower = Some(VersionRangeBound {
                    version: constraint.version.clone(),
                    included: true,
                });
                self.upper = Some(VersionRangeBound {
                    version: constraint.version.clone(),
                    included: true,
                });
            }
            RVersionOrdering::GT | RVersionOrdering::GE => {
                let proposed_bound = VersionRangeBound {
                    version: constraint.version.clone(),
                    included: inclusive,
                };

                if let Some(version_bound) = &self.lower {
                    if proposed_bound.version > version_bound.version {
                        self.lower = Some(proposed_bound);
                    }
                } else {
                    self.lower = Some(proposed_bound);
                }
            }
            RVersionOrdering::LT | RVersionOrdering::LE => {
                let proposed_bound = VersionRangeBound {
                    version: constraint.version.clone(),
                    included: inclusive,
                };

                if let Some(version_bound) = &self.upper {
                    if proposed_bound.version < version_bound.version {
                        self.upper = Some(proposed_bound);
                    }
                } else {
                    self.upper = Some(proposed_bound);
                }
            }
        };
    }

    pub fn is_satisfied_by(&self, version: &RVersion) -> bool {
        if let Some(lower) = &self.lower {
            if lower.included {
                if version < &lower.version {
                    return false;
                }
            } else {
                if version <= &lower.version {
                    return false;
                }
            }
        }

        if let Some(upper) = &self.upper {
            if upper.included {
                if version > &upper.version {
                    return false;
                }
            } else {
                if version >= &upper.version {
                    return false;
                }
            }
        }

        true
    }
}

impl std::fmt::Display for PackageBoundsConstraint<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(singleton) = self.get_singleton() {
            write!(f, "=={}", singleton)?;
            return Ok(());
        }
        if let Some(lower) = &self.lower {
            let cmp = if lower.included { ">=" } else { ">" };
            write!(f, "{}{}", cmp, lower.version)?;
        }

        if self.lower.is_some() && self.upper.is_some() {
            write!(f, " && ")?;
        }
        if let Some(upper) = &self.upper {
            let cmp = if upper.included { "<=" } else { "<" };
            write!(f, "{}{}", cmp, upper.version)?;
        }

        if self.lower.is_none() && self.upper.is_none() {
            write!(f, "*")?;
        }

        Ok(())
    }
}
