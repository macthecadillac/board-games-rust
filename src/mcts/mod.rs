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
struct Cell<M, P, T> {
    /// The index of a move
    index: M,
    /// The player that made the move
    player: P,
    /// The score of the branch the emenates from this node
    score: Score,
    /// The current game state
    state: T
}

/// Type alias of `Arena<Cell<M, P, T>>`
type Tree<M, P, T> = Arena<Cell<M, P, T>>;

/// Type alias of `Node<Cell<M, P, T>>`
type Node<M, P, T> = indextree::Node<Cell<M, P, T>>;

/// Generic function that compares two nodes using a user provided function.
fn comp<T, U, V, O>(func: fn(&Score, &Score) -> O,
                    a: (NodeId, &Node<T, U, V>),
                    b: (NodeId, &Node<T, U, V>)) -> O {
    let (_, node_a) = a;
    let (_, node_b) = b;
    func(&node_a.data.score, &node_b.data.score)
}

/// Comparison test for nodes, using `Score::gt_self`. See documentation of that
/// function.
fn fav_score_self<T, U, V>(a: (NodeId, &Node<T, U, V>),
                           b: (NodeId, &Node<T, U, V>)) -> bool {
    comp(Score::gt_self, a, b)
}

/// Comparison test for nodes, using `Score::gt_other`. See documentation of that
/// function.
fn fav_score_other<T, U, V>(a: (NodeId, &Node<T, U, V>),
                            b: (NodeId, &Node<T, U, V>)) -> bool {
    comp(Score::gt_other, a, b)
}

/// Comparison test for nodes, using `Score::feq`. See documentation of that
/// function.
fn score_eq<T, U, V>(a: (NodeId, &Node<T, U, V>),
                     b: (NodeId, &Node<T, U, V>)) -> bool {
    comp(Score::feq, a, b)
}

/// Comparison test of nodes
fn _gt<T, U, V>(a: (NodeId, &Node<T, U, V>),
                b: (NodeId, &Node<T, U, V>)) -> bool {
    match comp(Score::cmp, a, b) {
        cmp::Ordering::Greater => true,
        _ => false
    }
}

/// Equality test of nodes
fn _eq<T, U, V>(a: (NodeId, &Node<T, U, V>),
                b: (NodeId, &Node<T, U, V>)) -> bool {
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
fn expand_one_level<M, P, T>(tree: &mut Tree<M, P, T>, node_id: NodeId)
    where
        M: Initialize + Copy + Clone,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {

    let make_move = |ptot: usize, state: &T, index: &M| {
        let new_state = state.mv(index);
        let next_player = new_state.current_player();
        (next_player, Score::new(ptot), new_state)
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
                        let ptot = tree[node_id].data.score.total();
                        let state = &tree[node_id].data.state;
                        make_move(ptot, state, &index)
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
fn _playout_aux<M, P, T>(player: P, node_id: NodeId, tree: &mut Tree<M, P, T>)
                        -> Outcome
    where
        M: Initialize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {
    match node_id.node_type(tree) {
        NodeType::Node => {
            // TODO: Consider renaming this to something more descriptive
            let branch: NodeId = {
                let nodes = node_id.children(tree).map(|x| (x, &tree[x]));
                if tree[node_id].data.player == player {
                    pick(fav_score_self, score_eq, nodes)
                } else {
                    pick(fav_score_other, score_eq, nodes)
                }
            };
            let outcome = _playout_aux(player, branch, tree);
            tree[node_id].data.score.update(outcome);
            outcome
        },
        NodeType::Leaf => {
            match tree[node_id].data.state.status() {
                Status::Finished => {
                    let outcome = match tree[node_id].data.state.winner() {
                        None => Outcome::Draw,
                        Some(p) => {
                            if p == player { Outcome::Win }
                            else { Outcome::Loss }
                        }
                    };
                    tree[node_id].data.score.update(outcome);
                    outcome
                },
                Status::Ongoing => {
                    expand_one_level(tree, node_id);
                    _playout_aux(player, node_id, tree)
                }
            }
        }
    }
}

/// Perform playouts of the game
fn playout<M, P, T>(player: P, node: NodeId, tree: &mut Tree<M, P, T>)
    where
        M: Initialize + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {
    let _ = _playout_aux(player, node, tree);
}

/// Pick the most favorable move from the current game state
pub fn most_favored_move<M, P, T>(maxiter: usize, game_state: &T,
                                  dbg: &Debug) -> M
    where
        M: Initialize + Copy + Clone + Eq + fmt::Display,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {
    let player = game_state.current_player();
    let init_score = Score::new(1);
    let mut tree = Tree::new();
    let i_init: M = Initialize::new();
    let state = game_state.clone();
    let root_node = tree.new_node(Cell {
       index: i_init,
       player,
       score: init_score,
       state
    });

    for _ in 0..maxiter {
        playout(player, root_node, &mut tree)
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
