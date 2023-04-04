use std::collections::{BTreeMap, BTreeSet};

use rustc_data_structures::graph::{scc::Sccs, vec_graph::VecGraph};
use rustc_index::vec::Idx;

pub fn transitive_closure<T: Clone + Eq + PartialOrd + Ord>(
    mut map: BTreeMap<T, BTreeSet<T>>,
) -> BTreeMap<T, BTreeSet<T>> {
    let empty = BTreeSet::new();
    loop {
        let new = map
            .iter()
            .map(|(k, vs)| {
                let nvs = vs
                    .iter()
                    .flat_map(|v| map.get(v).unwrap_or(&empty).clone())
                    .collect();
                (k.clone(), vs.union(&nvs).cloned().collect())
            })
            .collect();
        if map == new {
            return map;
        }
        map = new;
    }
}

fn symmetric_closure<T: Clone + Eq + PartialOrd + Ord>(
    map: &BTreeMap<T, BTreeSet<T>>,
) -> BTreeMap<T, BTreeSet<T>> {
    let mut clo = map.clone();
    for (node, succs) in map {
        for succ in succs {
            clo.get_mut(succ).unwrap().insert(node.clone());
        }
    }
    clo
}

pub fn inverse<T: Clone + Eq + PartialOrd + Ord>(
    map: &BTreeMap<T, BTreeSet<T>>,
) -> BTreeMap<T, BTreeSet<T>> {
    let mut inv: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
    for node in map.keys() {
        inv.insert(node.clone(), BTreeSet::new());
    }
    for (node, succs) in map {
        for succ in succs {
            inv.get_mut(succ).unwrap().insert(node.clone());
        }
    }
    inv
}

/// `map` must not have a cycle.
pub fn post_order<T: Clone + Eq + PartialOrd + Ord>(
    map: &BTreeMap<T, BTreeSet<T>>,
    inv_map: &BTreeMap<T, BTreeSet<T>>,
) -> Vec<Vec<T>> {
    let mut res = vec![];
    let clo = symmetric_closure(map);
    let (_, components) = compute_sccs(&clo);

    for (_, component) in components {
        let mut v = vec![];
        let mut reached = BTreeSet::new();
        for node in component {
            if inv_map.get(&node).unwrap().is_empty() {
                dfs_walk(&node, &mut v, &mut reached, map);
            }
        }
        res.push(v);
    }

    res
}

fn dfs_walk<T: Clone + Eq + PartialOrd + Ord>(
    node: &T,
    v: &mut Vec<T>,
    reached: &mut BTreeSet<T>,
    map: &BTreeMap<T, BTreeSet<T>>,
) {
    reached.insert(node.clone());
    for succ in map.get(node).unwrap() {
        if !reached.contains(succ) {
            dfs_walk(succ, v, reached, map);
        }
    }
    v.push(node.clone());
}

pub fn compute_sccs<T: Clone + Eq + PartialOrd + Ord>(
    map: &BTreeMap<T, BTreeSet<T>>,
) -> (BTreeMap<Id, BTreeSet<Id>>, BTreeMap<Id, BTreeSet<T>>) {
    let id_map: BTreeMap<_, _> = map
        .keys()
        .enumerate()
        .map(|(i, f)| (i, f.clone()))
        .collect();
    let inv_id_map: BTreeMap<_, _> = id_map.iter().map(|(i, f)| (f.clone(), *i)).collect();
    let edges = map
        .iter()
        .flat_map(|(node, succs)| {
            succs.iter().map(|succ| {
                (
                    Id::new(*inv_id_map.get(node).unwrap()),
                    Id::new(*inv_id_map.get(succ).unwrap()),
                )
            })
        })
        .collect();
    let sccs: Sccs<Id, Id> = Sccs::new(&VecGraph::new(map.len(), edges));

    let component_graph: BTreeMap<_, _> = sccs
        .all_sccs()
        .map(|node| (node, sccs.successors(node).iter().cloned().collect()))
        .collect();

    let mut component_elems: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
    for i in 0..(map.len()) {
        let scc = sccs.scc(Id::new(i));
        let node = id_map.get(&i).unwrap().clone();
        component_elems.entry(scc).or_default().insert(node);
    }

    (component_graph, component_elems)
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Id(usize);

impl Idx for Id {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}
