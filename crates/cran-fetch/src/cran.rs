use cran_description_file_parser::{RVersion, VersionSeperator};
use ecow::EcoString;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RVersionEco {
    pub components: Vec<EcoString>,
    pub separators: Vec<VersionSeperator>
}

impl PartialOrd for RVersionEco {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // comparing two (in non-semver) is somewhat, tricky,
        // from the most forwards part of the version to the most
        // backwards, if two components are equal, then we move to the next component,
        // if two components are unequal, then the component with the longer
        // component is considered greater, if there is no component with a longer
        // component, then the component which sorts to a higher lexical order
        // is considered greater.

        let mut self_iter = self.components.iter();
        let mut other_iter = other.components.iter();

        loop {
            let self_comp = self_iter.next();
            let other_comp = other_iter.next();

            match (self_comp, other_comp) {
                (None, None) => return Some(std::cmp::Ordering::Equal),
                (None, Some(_)) => return Some(std::cmp::Ordering::Less),
                (Some(_), None) => return Some(std::cmp::Ordering::Greater),
                (Some(self_comp), Some(other_comp)) => {
                    if self_comp.len() == other_comp.len() {
                        match self_comp.cmp(other_comp) {
                            std::cmp::Ordering::Equal => continue,
                            x => return Some(x),
                        }
                    } else if self_comp.len() > other_comp.len() {
                        return Some(std::cmp::Ordering::Greater);
                    } else {
                        return Some(std::cmp::Ordering::Less);
                    }
                }
            }
        }
    }
}

impl Ord for RVersionEco {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl RVersionEco {
    pub fn from_str(s: &str) -> Self {
        let components = s.split(&['.', '-']).map(|s| EcoString::from(s)).collect::<Vec<_>>();
        Self { components, separators: s.chars().filter_map(|c| match c {
            '.' => Some(VersionSeperator::Dot),
            '-' => Some(VersionSeperator::Dash),
            _ => None
        }).collect() 
        }
    }
}

impl std::fmt::Display for RVersionEco {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("v")?;
        for (component, sep) in self.components.iter().zip(self.separators.iter().map(Some).chain(std::iter::once(None))) {
            f.write_str(component.as_str())?;
            if let Some(sep) = sep { 
                match sep {
                    VersionSeperator::Dot => f.write_str(".")?,
                    VersionSeperator::Dash => f.write_str("-")?
                }
            }
        }
        Ok(())
    }
}

impl pubgrub::version::Version for RVersionEco {
    fn lowest() -> Self {
        RVersionEco {
            components: Vec::new(),
            separators: Vec::new()
        }
    }

    fn bump(&self) -> Self {
        let mut comp = self.components.clone();
        comp.push(EcoString::from("0"));
        Self { components: comp, separators: {
            let mut seps = self.separators.clone();
            seps.push(VersionSeperator::Dot);
            seps
        } }
    }
}

impl<'s> From<RVersion<'s>> for RVersionEco {
    fn from(value: RVersion<'s>) -> Self {
        Self {
            components: value.components.into_iter().map(|s| s.into()).collect(),
            separators: value.separators
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    pub fn test_version_equality() {
        use super::RVersionEco;

        assert!(RVersionEco::from_str("100.0") > RVersionEco::from_str("3.0.0"));
        assert!(RVersionEco::from_str("0.1.0") < RVersionEco::from_str("3.0.0"));
        assert!(RVersionEco::from_str("0.1.0") > RVersionEco::from_str("0.0.1"));
        assert!(RVersionEco::from_str("0.30.0") > RVersionEco::from_str("0.4.0"));
        assert!(RVersionEco::from_str("1.1.1.1") == RVersionEco::from_str("1.1.1.1"));

        assert!(RVersionEco::from_str("2.0.0") > RVersionEco::from_str("1.9.9"));
        assert!(RVersionEco::from_str("1.10.0") > RVersionEco::from_str("1.2.0"));
        assert!(RVersionEco::from_str("1.0.0") < RVersionEco::from_str("1.0.1"));
        assert!(RVersionEco::from_str("1.0.0") < RVersionEco::from_str("1.0.0.1"));
        assert!(RVersionEco::from_str("1.0.0.0.1") > RVersionEco::from_str("1.0.0.0"));
        assert!(RVersionEco::from_str("0.97-4") > RVersionEco::from_str("0.8-9"));

        // I don't think this will handle alpha's and release candidates
        // right, but I'm really not sure what you can really do about that
        // because R packages are NOT semver, so it's really a harder place to be in.
    }
}
