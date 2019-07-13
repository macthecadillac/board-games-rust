/// A tree structure that does not allow adding siblings but optimizes serial
/// children access
use std::ops::Range;

#[derive(Clone)]
pub struct Cell<T> where T: Clone {
    indx: usize,
    data: T,
    children: Option<Range<usize>>
}

impl<T> Cell<T> where T: Clone {
    fn to_node(self) -> Node<T> {
        match self.children {
            None => Node::Leaf(self),
            Some(_) => Node::Node(self)
        }
    }
}

pub struct Tree<T> where T: Clone {
    arena: Vec<Node<T>>
}

pub enum Node<T> where T: Clone {
    Node(Cell<T>),
    Leaf(Cell<T>)
}

impl<'a, T> Node<T> where T: Clone {
    pub fn children(&'a self, tree: &'a mut Tree<T>) -> &mut [Node<T>] {
        match self {
            Node::Leaf(_) => &mut [],
            Node::Node(cell) => &mut tree.arena[cell.children.clone().unwrap()]
        }
    }

    pub fn append(self, tree: &mut Tree<T>, mut children: Vec<Cell<T>>) {
        match self {
            Node::Node(_) => panic!("Can't add to already populated node"),
            Node::Leaf(mut cell) => {
                let range_start = tree.arena.len();
                let mut children: Vec<_> = children.iter_mut()
                    .map(|x| x.clone().to_node())
                    .collect();
                tree.arena.append(&mut children);
                let range_end = tree.arena.len();
                cell.children = Some(range_start..range_end);
                let indx = cell.indx;
                tree.arena[indx] = Node::Node(cell);
            }
        }
    }
}
