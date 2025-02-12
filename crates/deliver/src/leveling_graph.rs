use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

pub struct LevelingGraph<T> {
    nodes: HashMap<T, HashSet<T>>,
}
impl<T: Hash + Debug + Eq + Clone> LevelingGraph<T> {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    pub fn insert(&mut self, item: T, children: HashSet<T>) {
        self.nodes.entry(item).or_default().extend(children);
    }

    fn get_leaf_deps(&mut self) -> HashSet<T> {
        let mut output_set = HashSet::new();

        for k in self
            .nodes
            .iter()
            .filter(|(_, v)| v.len() == 0)
            .map(|(k, _)| k)
        {
            output_set.insert(k.clone());
        }

        for s in &output_set {
            self.nodes.remove(s);
        }

        output_set
    }
    pub fn into_leveled(mut self) -> Vec<HashSet<T>> {
        let mut layers = Vec::new();

        while self.nodes.len() > 0 {
            let removed = self.get_leaf_deps();

            for values in self.nodes.values_mut() {
                for removed_entry in &removed {
                    values.remove(removed_entry);
                }
            }
            layers.push(removed);
        }
        layers
    }
}
