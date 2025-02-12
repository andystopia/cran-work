//! # Parallelizing building.
//!
//! topobuild can take a dependency DAG, and is a
//! state machine which will provide dependencies
//! which can be built right away.
//!
//! The goal of this crate is to very efficiently parallelize
//! building. I believe this data structure to be time-space
//! optimal. There exists no smaller data that can represent this
//! n + e, and the time is equalivalent to the space complexity, therefore
//! the time is the most efficient it can be, since time complexity
//! cannot be less than space complexity.

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// construct a hashmap literal. Does so via array.
/// Literally just syntactic sugar, shorthand, for convenience.
#[macro_export]
macro_rules! hashset {
    {$($v: expr),* $(,)?} => {
        ::std::collections::HashSet::from([$($v,)*])
    };
}

#[derive(Clone, Debug)]
struct BuildStepDeps<D> {
    depended_on_by: Vec<D>,
    depends_on: HashSet<D>,
}

/// Standardizes the way to describe dependencies
#[derive(Debug)]
pub struct BuildStep<D> {
    dep: D,
    depends_on: HashSet<D>,
}

/// Topobuild can, provided a list of dependencies
/// and their direct dependencies, will generate a
/// structure which will yield the immediately buildable
/// dependencies. As each are built, call the `built` method
/// on this struct, to indicate what's been completed thusfar,
/// once everything is built, then the `completed` function will
/// return true. To get a new job call `yield_next()`. This will provide
/// a new job. Just remember to pass return back to built if you want
/// the build to eventually complete.
///
/// This structure will reach a state where it is neither completed
/// nor yielding where all things requested from `yield_next()` have been
/// built if the structure passed to the constructor has a cycle. This only
/// really builds things with DAG.
#[derive(Debug)]
pub struct TopoBuild<D> {
    dep_lookup: HashMap<D, BuildStepDeps<D>>,
    unmet_deps: HashSet<D>,
    met_deps: Vec<D>,
}

impl<D: Hash + Eq + Clone> TopoBuild<D> {
    pub fn new(dependencies: Vec<BuildStep<D>>) -> Self {
        let mut met = Vec::new();
        let mut unmet = HashSet::new();

        let mut dep_lookup = HashMap::new();

        // we need the structures for dependencies /
        // reverse dependencies created out of order, because
        // we're not really sure when everything will all exist.
        for dep in &dependencies {
            dep_lookup.insert(
                dep.dep.clone(),
                BuildStepDeps {
                    depended_on_by: Vec::new(),
                    depends_on: HashSet::new(),
                },
            );
        }

        for BuildStep { dep, depends_on } in dependencies {
            let dep_clone = dep.clone();
            if depends_on.is_empty() {
                met.push(dep_clone);
            } else {
                unmet.insert(dep_clone);
            }

            // construct the reverse dependencies
            for depend_on in &depends_on {
                let rev_dep = dep_lookup.get_mut(depend_on).unwrap();
                rev_dep.depended_on_by.push(dep.clone());
            }

            // now construct the dependencies
            let looked_up = dep_lookup.get_mut(&dep).unwrap();
            looked_up.depends_on.extend(depends_on);
        }

        Self {
            unmet_deps: unmet,
            met_deps: met,
            dep_lookup,
        }
    }

    pub fn yield_next(&mut self) -> Option<D> {
        self.met_deps.pop()
    }

    pub fn built(&mut self, element: D) {
        // TODO: maybe remove this clone at some point or
        // replace it with a guaranteed cheaper to clone
        // structure. Definitely room for a slight performance
        // benefit here.
        let Some(BuildStepDeps { depended_on_by, .. }) = self.dep_lookup.get(&element).cloned()
        else {
            return;
        };

        for reverse_dep in depended_on_by {
            let Some(BuildStepDeps { depends_on, .. }) = self.dep_lookup.get_mut(&reverse_dep)
            else {
                continue;
            };

            depends_on.remove(&element);

            // once all dependencies are met,
            // we can safely move this into the
            // the category.
            if depends_on.is_empty() {
                if let Some(dep) = self.unmet_deps.take(&reverse_dep) {
                    self.met_deps.push(dep)
                }
            }
        }
    }

    pub fn completed(&mut self) -> bool {
        self.met_deps.is_empty() && self.unmet_deps.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_constructor() {
        let bg = TopoBuild::new(vec![BuildStep {
            dep: "A",
            depends_on: HashSet::new(),
        }]);

        assert!(bg.met_deps.len() == 1);
    }

    #[test]
    fn test_graph() {
        let mut bg = TopoBuild::new(vec![
            BuildStep {
                dep: "A",
                depends_on: hashset! { "B" },
            },
            BuildStep {
                dep: "B",
                depends_on: hashset! {},
            },
            BuildStep {
                dep: "C",
                depends_on: hashset! {},
            },
        ]);

        dbg!(&bg);

        let first = bg.yield_next().unwrap();
        let second = bg.yield_next().unwrap();

        // ensure that we are first provided with B and C.
        let mut both = [first, second];
        both.sort();
        assert_eq!(both, ["B", "C"]);

        // now ensure that we don't yield A, before we're fully ready
        assert_eq!(bg.yield_next(), None);

        // now build C.
        bg.built("C");

        // since A doesn't depend on C, it should *not* be yielded.
        assert_eq!(bg.yield_next(), None);

        // now let's build B.
        bg.built("B");

        dbg!(&bg);

        assert_eq!(bg.yield_next(), Some("A"))
    }
}
