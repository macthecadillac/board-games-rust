/// A tree structure that does not allow adding siblings but optimizes serial
/// children access
use std::ops::{Index, IndexMut, Range};

#[derive(Copy, Clone, From)]
/// FIXME: implement std::iter::Step once it is stable
pub struct NodeIndx(usize);

#[derive(Clone)]
pub struct Node<T> where T: Clone {
    pub data: T,
    children: Option<Range<usize>>
}

pub struct Tree<T> where T: Clone {
    arena: Vec<Node<T>>
}

impl NodeIndx {
    pub fn children<'a, T>(&self, tree: &'a Tree<T>) -> Option<Range<usize>>
        where T: Clone {
        tree[*self].children.clone()
    }

    pub fn append<T>(&self, tree: &mut Tree<T>, mut children: Vec<Node<T>>)
        where T: Clone {
        match tree[*self].children {
            Some(_) => panic!("Can't add to an already populated node"),
            None => {
                let range_start = tree.arena.len();
                tree.arena.append(&mut children);
                let range_end = tree.arena.len();
                tree[*self].children = Some(range_start..range_end);
            }
        }
    }
}

impl<T> From<T> for Node<T> where T: Clone {
    fn from(item: T) -> Self { Node { data: item, children: None } }
}

impl<'a, T> Tree<T> where T: Clone {
    pub fn new() -> Self { Tree { arena: Vec::new() } }

    pub fn add_node(&'a mut self, node_data: T) -> NodeIndx {
        self.arena.push(Node { data: node_data, children: None });
        NodeIndx(self.arena.len() - 1)
    }

    // FIXME: if you ever find a more idiomatic way to do this, be my guest
    pub fn slice(&self, range: Range<usize>) -> &[Node<T>] {
        &self.arena[range]
    }
}

/// TODO: Remove once std::iter::Step is stabilized
impl<T> Index<usize> for Tree<T> where T: Clone {
    type Output = Node<T>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.arena[index]
    }
}

/// TODO: Remove once std::iter::Step is stabilized
impl<T> IndexMut<usize> for Tree<T> where T: Clone {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.arena[index]
    }
}

impl<T> Index<NodeIndx> for Tree<T> where T: Clone {
    type Output = Node<T>;

    fn index(&self, index: NodeIndx) -> &Self::Output {
        let NodeIndx(i) = index;
        &self.arena[i]
    }
}

impl<T> IndexMut<NodeIndx> for Tree<T> where T: Clone {
    fn index_mut(&mut self, index: NodeIndx) -> &mut Self::Output {
        let NodeIndx(i) = index;
        &mut self.arena[i]
    }
}
