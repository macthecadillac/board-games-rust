use std::fmt;
use std::cmp;

use indextree::{Arena, NodeId};
use rand::seq::SliceRandom;

mod score;
use score::Score;

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
    type Move: Initialize + Clone + Copy;
    type Player: fmt::Display + Eq + Clone + Copy;

    /// Initialize game
    fn new(players: Vec<Self::Player>) -> Self;

    /// Return all the available moves for the current player in the current
    /// game state. For games in which repeated moves are possible, this method
    /// *must* remove such moves from the list of available moves to avoid
    /// possible infinite loops.
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

/// The type of a node in a tree (indextree).
enum NodeType {
    /// A Node is a node that has descendents
    Node,
    /// A Leaf is a childless node
    Leaf
}

trait ReturnType<T> {
    /// Return the `NodeType` of an indextree node
    fn node_type(&self, tree: &Arena<T>) -> NodeType;
}

impl<T> ReturnType<T> for NodeId {
    fn node_type(&self, tree: &Arena<T>) -> NodeType {
        match self.children(tree).next() {
            Some(_) => NodeType::Node,
            None => NodeType::Leaf
        }
    }
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
        M: PartialEq + Eq + Clone,
        P: PartialEq + Eq + Clone,
        T: PartialEq + Eq + Clone {
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
        M: PartialEq + Eq + Clone,
        P: PartialEq + Eq + Clone,
        T: PartialEq + Eq + Clone {
    fn eq(&self, rhs: &Self) -> bool {
        self.state == rhs.state
    }
}

impl<M, P, T> Eq for Cell<M, P, T>
    where
        M: PartialEq + Eq + Clone,
        P: PartialEq + Eq + Clone,
        T: PartialEq + Eq + Clone {}

/// Type alias of `Arena<Cell<M, P, T>>`
type Tree<M, P, T> = Arena<Cell<M, P, T>>;

/// Type alias of `Node<Cell<M, P, T>>`
type Node<M, P, T> = indextree::Node<Cell<M, P, T>>;

/// Generic function that compares two nodes using a user provided function.
fn comp<T, U, V, O>(func: fn(&Score, &Score) -> O,
                    a: (NodeId, &Node<T, U, V>),
                    b: (NodeId, &Node<T, U, V>)) -> O
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: PartialEq + Eq + Clone {
    let (_, node_a) = a;
    let (_, node_b) = b;
    func(&node_a.data.score, &node_b.data.score)
}

/// Comparison test for nodes, using `Score::gt_self`. See documentation of that
/// function.
fn fav_score_self<T, U, V>(a: (NodeId, &Node<T, U, V>),
                           b: (NodeId, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: PartialEq + Eq + Clone {
    comp(Score::gt_self, a, b)
}

/// Comparison test for nodes, using `Score::gt_other`. See documentation of that
/// function.
fn fav_score_other<T, U, V>(a: (NodeId, &Node<T, U, V>),
                            b: (NodeId, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: PartialEq + Eq + Clone {
    comp(Score::gt_other, a, b)
}

/// Comparison test for nodes, using `Score::feq`. See documentation of that
/// function.
fn score_eq<T, U, V>(a: (NodeId, &Node<T, U, V>),
                     b: (NodeId, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: PartialEq + Eq + Clone {
    comp(Score::feq, a, b)
}

/// Comparison test of nodes
fn _gt<T, U, V>(a: (NodeId, &Node<T, U, V>),
                b: (NodeId, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: PartialEq + Eq + Clone {
    match comp(Score::cmp, a, b) {
        cmp::Ordering::Greater => true,
        _ => false
    }
}

/// Equality test of nodes
fn _eq<T, U, V>(a: (NodeId, &Node<T, U, V>),
                b: (NodeId, &Node<T, U, V>)) -> bool
    where
        T: PartialEq + Eq + Clone,
        U: PartialEq + Eq + Clone,
        V: PartialEq + Eq + Clone {
    comp(Score::eq, a, b)
}

/// Pick according to the criterion given (as function f). When undecided,
/// randomly pick one from among the equals.
fn pick<'a, T, U, V>(greater: fn((T, &'a U), (T, &'a U)) -> bool,
                     eq: fn((T, &'a U), (T, &'a U)) -> bool,
                     l: V) -> T
    where
        T: Copy,
        V: Iterator<Item=(T, &'a U)> {
    let iter = l.zip(vec![None].into_iter().cycle());
    let (x, _) = pick_opt(greater, eq, iter);
    x
}

/// Pick according to the criterion given (as function f). When undecided,
/// randomly pick one from among the equals.
fn pick_opt<'a, T, U, V>(greater: fn((T, &'a U), (T, &'a U)) -> bool,
                         eq: fn((T, &'a U), (T, &'a U)) -> bool,
                         l: V) -> (T, Option<T>)
    where
        T: Copy,
        V: Iterator<Item=((T, &'a U), Option<(T, &'a U)>)> {
    let mut peekable = l.peekable();
    let first = *peekable.peek().unwrap();
    let max = match first {
        // None on the right means there is no old data to go off with. Proceed
        // to random selection
        (_, None) => peekable.collect::<Vec<_>>(),
        (_, Some(_)) =>
            peekable.fold(vec![first], |mut eqs, (x, y)| {
                if greater(y.unwrap(), eqs[0].1.unwrap()) { vec![(x, y)] }
                else if eq(y.unwrap(), eqs[0].1.unwrap()) { eqs.push((x, y)); eqs }
                else { eqs }
            })
    };
    let mut r = rand::thread_rng();
    match *max[..].choose(&mut r).unwrap() {
        ((a, _), None) => (a, None),
        ((a, _), Some((x, _))) => (a, Some(x))
    }
}

/// Expand the tree by one level
fn expand_one_level<M, P, T>(tree: &mut Tree<M, P, T>, node_id: NodeId)
    where
        M: Initialize + PartialEq + Eq + Copy + Clone,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Eq + Clone {

    let make_move = |parent_total: usize, state: &T, index: &M| {
        let new_state = state.mv(index);
        let next_player = new_state.current_player();
        (next_player, Score::new(parent_total), new_state)
    };

    let add_node =
        |index: M, player: P, score: Score, state: T, t: &mut Tree<M, P , T>| {
        let new_node = t.new_node(Cell { index, player, score, state });
        node_id.append(new_node, t).unwrap();
        new_node
    };

    match node_id.node_type(tree) {
        NodeType::Node => panic!("Expansion error"),
        NodeType::Leaf => {
            match &tree[node_id].data.state.available_moves()[..] {
                &[] => panic!("Expansion error"),
                moves => moves.iter().for_each(|&index| {
                    let (player, score, state) = {
                        let parent_total = tree[node_id].data.score.total();
                        let state = &tree[node_id].data.state;
                        make_move(parent_total, state, &index)
                    };
                    if player != tree[node_id].data.player {
                        add_node(index, player, score, state, tree);
                    } else {
                        match state.status() {
                            Status::Finished => {
                                add_node(index, player, score, state, tree);
                            },
                            Status::Ongoing => {
                                let new_node = add_node(index, player, score, state, tree);
                                expand_one_level(tree, new_node);
                            }
                        }
                    }
                })
            }
        }
    }
}

/// The core of the function that carries out simulations
fn _playout_aux<M, P, T>(player: P,
                         tmp_node_id: NodeId, tmp_tree: &mut Tree<M, P, T>,
                         node_id_opt: Option<NodeId>, tree: &Tree<M, P, T>)
                        -> Outcome
    where
        M: Initialize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Eq + Clone {

    match tmp_tree[tmp_node_id].data.state.status() {
        Status::Finished => {
            let outcome = match tmp_tree[tmp_node_id].data.state.winner() {
                None => Outcome::Draw,
                Some(p) => {
                    if p == player { Outcome::Win }
                    else { Outcome::Loss }
                }
            };
            tmp_tree[tmp_node_id].data.score.update(outcome);
            outcome
        },
        Status::Ongoing => {
            expand_one_level(tmp_tree, tmp_node_id);
            let choose_branches = || {
                let nodes = tmp_node_id.children(tmp_tree)
                    .map(|x| (x, &tmp_tree[x]));
                if tmp_tree[tmp_node_id].data.player == player {
                    (pick(fav_score_self, score_eq, nodes), None)
                } else {
                    (pick(fav_score_other, score_eq, nodes), None)
                }
            };
            let (tmp_branch, branch) = match node_id_opt {
                None => choose_branches(),
                Some(node_id) => {
                    match node_id.node_type(tree) {
                        NodeType::Leaf => choose_branches(),
                        NodeType::Node => {
                            let nodes = tmp_node_id.children(tmp_tree)
                                .map(|x| (x, &tmp_tree[x]))
                                .zip(node_id.children(tree)
                                     .map(|x| Some((x, &tree[x]))));
                            if tmp_tree[tmp_node_id].data.player == player {
                                pick_opt(fav_score_self, score_eq, nodes)
                            } else {
                                pick_opt(fav_score_other, score_eq, nodes)
                            }
                        }
                    }
                }
            };
            let outcome = _playout_aux(player, tmp_branch, tmp_tree, branch, tree);
            tmp_tree[tmp_node_id].data.score.update(outcome);
            outcome
        }
    }
}

/// Perform playouts of the game
fn playout<M, P, T>(player: P, root_node: NodeId, tree: &Tree<M, P, T>)
                   -> (NodeId, Tree<M, P, T>)
    where
        M: Initialize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Eq + Clone {
    let mut tmp_tree = Tree::new();
    let root_node_data = tree[root_node].data.clone();
    let tmp_root_node = tmp_tree.new_node(root_node_data);
    let _ = _playout_aux(player, tmp_root_node, &mut tmp_tree, Some(root_node), tree);
    (root_node, tmp_tree)
}

/// Append tree2 to the given node. Assumes "start_node" is the same as "node"
/// in tree1 and skips that during the operation.
fn append_tree<'a, M, P, T>(tree1: &mut Tree<M, P, T>, node: NodeId,
                            tree2: &Tree<M, P, T>, start_node: NodeId)
    where
        M: Initialize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Eq + Clone {
    match start_node.node_type(tree2) {
        NodeType::Leaf => (),
        NodeType::Node => {
            match node.node_type(tree1) {
                NodeType::Leaf => {
                    // Add all the first-level descendents before moving on to
                    // the next level so that we don't need to test for equality
                    // of cells later, which helps performance
                    for i in start_node.children(tree2) {
                        let child_node = tree1.new_node(tree2[i].data.clone());
                        node.append(child_node, tree1).unwrap();
                    };
                    append_tree(tree1, node, tree2, start_node);
                },
                NodeType::Node => {
                    node.children(tree1)
                        .zip(start_node.children(tree2))
                        // terminate borrow so we can mutate later
                        .collect::<Vec<_>>()
                        .into_iter()
                        .for_each(|(i, j)| {
                            tree1[i].data.score += &tree2[j].data.score;
                            append_tree(tree1, i, tree2, j);
                        })
                }
            }
        }
    };
}

/// Pick the most favorable move from the current game state
pub fn most_favored_move<M, P, T>(maxiter: usize, game_state: &T,
                                  dbg: &Debug) -> M
    where
        M: Initialize + Copy + Clone + Eq + fmt::Display,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Eq + Clone {
    let player = game_state.current_player();
    let init_score = Score::new(1);
    let mut tree = Tree::new();
    let i_init: M = Initialize::new();
    let state = game_state.clone();
    let init_cell = Cell { index: i_init, player, score: init_score, state };
    let root_node = tree.new_node(init_cell);

    for _ in 0..maxiter {
        let (rootv2, treev2) = playout(player, root_node, &tree);
        append_tree(&mut tree, root_node, &treev2, rootv2);
    }

    match dbg {
        Debug::Release => (),
        Debug::Debug => {
            root_node.children(&tree)
                .map(|x| &tree[x].data)
                .for_each(|cell| println!("{}  {}", cell.index, cell.score));
            println!("");
        }
    };

    let branches = root_node.children(&tree).map(|x| (x, &tree[x]));
    let most_promising_branch = pick(_gt, _eq, branches);
    tree[most_promising_branch].data.index
}
