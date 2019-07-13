/// A tree structure that does not allow adding siblings but optimizes serial
/// children access
use std::ops::{Index, IndexMut, Range};
use crate::mcts::{NodeType, ReturnType};

#[derive(Copy, Clone)]
pub struct NodeIndx(usize);

#[derive(Clone)]
pub struct Node<T> where T: Clone {
    pub data: T,
    indx: Option<NodeIndx>,
    children: Option<Range<usize>>
}

pub struct Tree<T> where T: Clone {
    arena: Vec<Node<T>>
}

impl<'a, T> Node<T> where T: Clone {
    pub fn children(&'a self, tree: &'a mut Tree<T>) -> &mut [Node<T>] {
        match self.children.clone() {
            None => &mut [],
            Some(range) => &mut tree.arena[range.clone()]
        }
    }

    pub fn append(&mut self, tree: &mut Tree<T>, mut children: Vec<Node<T>>) {
        match self.children {
            Some(_) => panic!("Can't add to an already populated node"),
            None => {
                let range_start = tree.arena.len();
                tree.arena.append(&mut children);
                let range_end = tree.arena.len();
                self.children = Some(range_start..range_end);
            }
        }
    }

    pub fn index(&self) -> Option<NodeIndx> {
        self.indx
    }
}

impl<T> From<T> for Node<T> where T: Clone{
    fn from(item: T) -> Self { Node { data: item, indx: None, children: None } }
}

impl<T> ReturnType<T> for Node<T> where T: Clone {
    fn node_type(&self, tree: &Tree<T>) -> NodeType {
        match self.children {
            Some(_) => NodeType::Node,
            None => NodeType::Leaf
        }
    }
}

impl<'a, T> Tree<T> where T: Clone {
    pub fn new() -> Self { Tree { arena: Vec::new() } }

    pub fn add_node(&'a mut self, node_data: T) -> &mut Node<T> {
        let indx = Some(NodeIndx(self.arena.len()));
        self.arena.push(Node { data: node_data, indx, children: None });

        let len = self.arena.len();
        &mut self.arena[len]
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
