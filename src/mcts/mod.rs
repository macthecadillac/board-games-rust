use std::fmt;
use std::cmp;

// use indextree::{Arena, NodeId};
use rand::seq::SliceRandom;

mod score;
use score::Score;

use crate::tree;
use crate::tree::NodeIndx;

#[derive(Clone, Copy)]
/// The Outcome type.
pub enum Outcome {
    /// A win
    Win,
    /// A loss
    Loss,
    /// A draw
    Draw
}

pub trait AsUsize {
    fn as_usize(&self) -> usize;
}

/// The Status type
pub enum Status {
    /// The game is still ongoing
    Ongoing,
    /// The game is finished
    Finished
}

/// This provides the interface for all structs that describe the game state.
/// Any such struct must provide the methods listed here to function properly.
pub trait Game {
    type Move: Initialize + AsUsize + Clone + Copy;
    type Player: fmt::Display + Eq + Clone + Copy;

    /// Initialize game
    fn new(players: Vec<Self::Player>) -> Self;

    /// Return all the available moves for the current player in the current
    /// game state. For games in which repeated moves are possible, this method
    /// *must* remove such moves from the list of available moves to avoid
    /// possible infinite loops. This method also *must* return the moves
    /// already sorted when as_usize() is called.
    fn available_moves(&self) -> Vec<Self::Move>;

    /// Return the player that plays the next turn.
    fn current_player(&self) -> Self::Player;

    /// Return the player that plays the next turn.
    fn next_player(&self) -> Self::Player;

    /// Check if the game has reached the end.
    fn status(&self) -> Status;

    /// Make the specified move and return the updated game state.
    fn mv(&self, mv: &Self::Move) -> Self;

    /// Return the winner if the game has reached the end. Return None
    /// otherwise.
    fn winner(&self) -> Option<Self::Player>;
}

/// Initialize type
pub trait Initialize {
    /// Return some initial value
    fn new() -> Self;
}

/// The Debug type
pub enum Debug { Debug, Release }

/// The cell in indextree. This is useful for storing the index that
/// parametrizes a move, the player that made the move, the score of the branch,
/// and the game state at this point in the game.
#[derive(Clone)]
struct Cell<M, P, T>
    where
        M: Eq + PartialEq,
        P: Eq + PartialEq {
    /// The index of a move
    index: M,
    /// The player that made the move
    player: P,
    /// The score of the branch the emenates from this node
    score: Score,
    /// The current game state
    state: T
}

impl<M, P, T> PartialEq for Cell<M, P, T>
    where
        M: Eq + PartialEq,
        P: Eq + PartialEq {
    fn eq(&self, rhs: &Cell<M, P, T>) -> bool {
        self.index == rhs.index && self.player == rhs.player
    }
}

/// Type alias of `Arena<Cell<M, P, T>>`
type Tree<M, P, T> = tree::Tree<Cell<M, P, T>>;

/// Type alias of `Node<Cell<M, P, T>>`
type Node<M, P, T> = tree::Node<Cell<M, P, T>>;

/// Generic function that compares two nodes using a user provided function.
fn comp<T, U, V, O>(func: fn(&Score, &Score) -> O,
                    a: (NodeIndx, &Node<T, U, V>),
                    b: (NodeIndx, &Node<T, U, V>)) -> O
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: Clone {
    let (_, node_a) = a;
    let (_, node_b) = b;
    func(&node_a.data.score, &node_b.data.score)
}

/// Comparison test for nodes, using `Score::gt_self`. See documentation of that
/// function.
fn fav_score_self<T, U, V>(a: (NodeIndx, &Node<T, U, V>),
                           b: (NodeIndx, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: Clone {
    comp(Score::gt_self, a, b)
}

/// Comparison test for nodes, using `Score::gt_other`. See documentation of that
/// function.
fn fav_score_other<T, U, V>(a: (NodeIndx, &Node<T, U, V>),
                            b: (NodeIndx, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: Clone {
    comp(Score::gt_other, a, b)
}

/// Comparison test for nodes, using `Score::feq`. See documentation of that
/// function.
fn score_eq<T, U, V>(a: (NodeIndx, &Node<T, U, V>),
                     b: (NodeIndx, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: Clone {
    comp(Score::feq, a, b)
}

/// Comparison test of nodes
fn _gt<T, U, V>(a: (NodeIndx, &Node<T, U, V>),
                b: (NodeIndx, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: Clone {
    match comp(Score::cmp, a, b) {
        cmp::Ordering::Greater => true,
        _ => false
    }
}

/// Equality test of nodes
fn _eq<T, U, V>(a: (NodeIndx, &Node<T, U, V>),
                b: (NodeIndx, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: Clone {
    comp(Score::eq, a, b)
}

/// Pick according to the criterion given (as function f). When undecided,
/// randomly pick one from among the equals.
fn pick<'a, U, V>(greater: fn((NodeIndx, &'a U), (NodeIndx, &'a U)) -> bool,
                  eq: fn((NodeIndx, &'a U), (NodeIndx, &'a U)) -> bool,
                  l: V) -> NodeIndx
    where V: Iterator<Item=(NodeIndx, &'a U)> {
    let mut peekable = l.peekable();
    let first = *peekable.peek().unwrap();
    let max = peekable.fold(vec![first], |mut eqs, x| {
        if greater(x, eqs[0]) { vec![x] }
        else if eq(x, eqs[0]) { eqs.push(x); eqs }
        else { eqs }
    });
    let mut r = rand::thread_rng();
    let (a, _) = *max[..].choose(&mut r).unwrap();
    a
}

/// Expand the tree by one level
fn expand_one_level<M, P, T>(tree: &mut Tree<M, P, T>, node_idx: tree::NodeIndx)
    where
        M: Initialize + AsUsize + Eq + Copy + Clone,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {

    let make_move = |parent_total: usize, state: &T, index: &M| {
        let new_state = state.mv(index);
        let next_player = new_state.current_player();
        (next_player, Score::new(parent_total), new_state)
    };

    match node_idx.children(tree) {
        Some(range) => {
            // FIXME: remove map once std::iter::Step is stable
            range.map(NodeIndx::from).for_each(|x| {
                match tree[x].data.state.status() {
                    Status::Ongoing => {
                        let player1 = tree[x].data.player;
                        let player2 = tree[node_idx].data.player;
                        // if tree[x].data.player == tree[node_idx].data.player {
                        if player1 == player2 {
                            expand_one_level(tree, x);
                        } else {
                        }
                    },
                    Status::Finished => ()
                }
            })
        },
        None => {
            let children: Vec<_> = tree[node_idx].data.state
                .available_moves()
                .iter()
                .map(|&index| {
                    let (player, score, state) = {
                        let parent_total = tree[node_idx].data.score.total();
                        let state = &tree[node_idx].data.state;
                        make_move(parent_total, state, &index)
                    };
                    let cell = Cell { index, player, score, state };
                    cell.into()
                })
                .collect();
            node_idx.append(tree, children);
            expand_one_level(tree, node_idx);
        }
    }
}

/// The core of the function that carries out simulations
fn playout_core<M, P, T>(player: P, node_idx: tree::NodeIndx,
                         tree: &mut Tree<M, P, T>) -> Outcome
    where
        M: Initialize + AsUsize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {
    match node_idx.children(tree) {
        Some(range) => {
            let branch_idx = {
                let nodes = range.map(NodeIndx::from)
                    .map(|x| (x, &tree[x]));
                if tree[node_idx].data.player == player {
                    pick(fav_score_self, score_eq, nodes)
                } else {
                    pick(fav_score_other, score_eq, nodes)
                }
            };
            let outcome = playout_core(player, branch_idx, tree);
            tree[node_idx].data.score.update(outcome);
            outcome
        },
        None => {
            match tree[node_idx].data.state.status() {
                Status::Finished => {
                    let outcome = match tree[node_idx].data.state.winner() {
                        None => Outcome::Draw,
                        Some(p) => {
                            if p == player { Outcome::Win }
                            else { Outcome::Loss }
                        }
                    };
                    tree[node_idx].data.score.update(outcome);
                    outcome
                },
                Status::Ongoing => {
                    expand_one_level(tree, node_idx);
                    playout_core(player, node_idx, tree)
                }
            }
        }
    }
}

/// Append tree2 to the given node. Assumes "start_node" is the same as "node"
/// in tree1 and merges "start_node" with "node" then append the children.
fn append_tree<M, P, T>(tree1: &mut Tree<M, P, T>, node: tree::NodeIndx,
                  tree2: &Tree<M, P, T>, start_node: tree::NodeIndx,
                  indx: usize)
    where
        M: Initialize + AsUsize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {
    match start_node.children(tree2) {
        None => (),
        Some(range2) => {
            match node.children(tree1) {
                Some(range1) => {
                    range1.map(NodeIndx::from)
                        .zip(range2.map(NodeIndx::from))
                        .for_each(|(i, j)| {
                            println!("for each (310)");
                            let score2 = &tree2[j].data.score;
                            println!("for each (312)");
                            tree1[i].data.score += score2;
                            println!("one level down");
                            append_tree(tree1, i, tree2, j, indx + 1);
                            println!("going up");
                        })
                },
                None => {
                    let children = tree2.slice(range2).to_vec();
                    node.append(tree1, children);
                    append_tree(tree1, node, tree2, start_node, indx + 1);
                }
            }
        }
    }
}

/// Perform playouts of the game
fn playout<M, P, T>(player: P, node: &Cell<M, P, T>)
                   -> (tree::NodeIndx, Tree<M, P, T>)
    where
        M: Initialize + AsUsize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {
    let mut tree = Tree::new();
    let root_node = tree.add_node(node.clone());
    let _ = playout_core(player, root_node, &mut tree);
    (root_node, tree)
}

/// Pick the most favorable move from the current game state
pub fn most_favored_move<M, P, T>(maxiter: usize, game_state: &T,
                                  dbg: &Debug) -> M
    where
        M: Initialize + AsUsize + Copy + Clone + Eq + fmt::Display,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {
    let player = game_state.current_player();
    let init_score = Score::new(1);
    let mut tree = Tree::new();
    let i_init: M = Initialize::new();
    let state = game_state.clone();
    let cell = Cell { index: i_init, player, score: init_score, state };
    let root_node = tree.add_node(cell.clone());

    for i in 0..maxiter {
        let (rootv2, treev2) = playout(player, &cell);
        append_tree(&mut tree, root_node, &treev2, rootv2, 1);
        println!("Finished play-out {}", i);
    }

    match dbg {
        Debug::Release => (),
        Debug::Debug => {
            tree.slice(root_node.children(&tree).unwrap())
                .iter()
                .map(|x| &x.data)
                .for_each(|cell| println!("{}  {}", cell.index, cell.score));
            println!("");
        }
    };

    let branches = root_node.children(&tree).unwrap()
        .map(NodeIndx::from)
        .map(|x| (x, &tree[x]));
    let most_promising_branch = pick(_gt, _eq, branches);
    tree[most_promising_branch].data.index
}
