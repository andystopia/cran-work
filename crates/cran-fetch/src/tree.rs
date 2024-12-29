use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub struct Tree<T> {
    value: T,
    children: Vec<Tree<T>>,
}

impl<T> Tree<T> {
    pub fn new(value: T) -> Self {
        Tree {
            value,
            children: Vec::new(),
        }
    }

    pub fn push_child(&mut self, child: T) {
        self.children.push(Tree::new(child));
    }

    pub fn push_tree(&mut self, tree: Tree<T>) {
        self.children.push(tree);
    }
}

impl<T: Hash + Eq + Clone> Tree<T> {
    pub fn create_distance_map(&self) -> HashMap<T, usize> {
        let mut max_depth = 0;

        if self.children.len() == 0 {
            return {
                let mut h = HashMap::new();
                h.insert(self.value.clone(), 0);
                h
            };
        }

        let mut output_hashmap = HashMap::new();

        for child in &self.children {
            let child_depth_map = child.create_distance_map();

            for (key, child_depth) in child_depth_map {
                max_depth = max_depth.max(child_depth);
                match output_hashmap.get(&key) {
                    Some(existing_depth) => {
                        if child_depth > *existing_depth {
                            output_hashmap.insert(key, child_depth);
                        }
                    }
                    None => {
                        output_hashmap.insert(key, child_depth);
                    }
                }
            }
        }

        output_hashmap.insert(self.value.clone(), max_depth + 1);

        output_hashmap
    }

    pub fn create_leveled(&self) -> Vec<HashSet<T>> { 
        let depths = self.create_distance_map();

        let max_depth = *depths.values().max().unwrap();
        let mut leveled = vec![HashSet::new(); max_depth + 1];

        for (key, depth) in depths {
            leveled[depth].insert(key);
        }

        leveled
    }
}

#[cfg(test)]
mod test {
    #[test]
    pub fn test_depth() {
        use super::Tree;

        let mut tree = Tree::new("Meals");
        tree.push_child("Breakfast");
        tree.children[0].push_child("Cereal");
        tree.push_child("Lunch");
        tree.push_child("Dinner");

        dbg!(tree.create_leveled());
    }
}
