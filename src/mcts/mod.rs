use std::fmt;
use std::cmp;

use atree::{Arena, Token};
use rand::seq::SliceRandom;

mod score;
use score::Score;
use crate::repetition_guard::{Repetition, RepetitionGuard};

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
    type Move: Default + Clone + Copy;
    type Moves: Iterator<Item=Self::Move>;
    type Player: fmt::Display + Eq + Clone + Copy;

    /// Initialize game
    fn new(players: Vec<Self::Player>) -> Self;

    /// Return all the available moves for the current player in the current
    /// game state. For games in which repeated moves are possible, this method
    /// *must* remove such moves from the list of available moves to avoid
    /// possible infinite loops.
    fn available_moves(&self) -> Self::Moves;

    /// Return the player that plays the next turn.
    fn current_player(&self) -> Self::Player;

    /// Return the player that plays the next turn.
    fn next_player(&self) -> Self::Player;

    /// Check if the game has reached the end.
    fn status(&self) -> Status;

    /// Make the specified move and return the updated game state.
    fn mv(&self, mv: Self::Move) -> Self;

    /// Return the winner if the game has reached the end. Return None
    /// otherwise.
    fn winner(&self) -> Option<Self::Player>;
}

/// The type of a node in a tree (atree).
enum NodeType {
    /// A Node is a node that has descendents
    Node,
    /// A Leaf is a childless node
    Leaf
}

trait ReturnType<T> {
    /// Return the `NodeType` of an atree node
    fn node_type(&self, tree: &Arena<T>) -> NodeType;
}

impl<T> ReturnType<T> for Token {
    fn node_type(&self, tree: &Arena<T>) -> NodeType {
        match self.children(tree).next() {
            Some(_) => NodeType::Node,
            None => NodeType::Leaf
        }
    }
}

/// The Debug type
pub enum Debug { Debug, Release }

/// The cell in atree. This is useful for storing the index that
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
type Node<M, P, T> = atree::Node<Cell<M, P, T>>;

/// Utility function that compares two nodes using a user provided function.
fn comp<M, P, T, O>(func: fn(&Score, &Score) -> O,
                    a: (Token, &Node<M, P, T>),
                    b: (Token, &Node<M, P, T>)) -> O {
    let (_, node_a) = a;
    let (_, node_b) = b;
    func(&node_a.data.score, &node_b.data.score)
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
fn expand_one_level<M, P, T>(tree: &mut Tree<M, P, T>, node_id: Token)
    where
        M: Default + Copy + Clone,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone {

    let make_move = |ptot: usize, state: &T, index: M| {
        let new_state = state.mv(index);
        let next_player = new_state.current_player();
        (next_player, Score::new(ptot), new_state)
    };

    match node_id.node_type(tree) {
        NodeType::Node => panic!("Expansion error"),
        NodeType::Leaf => {
            let mut available_moves = tree[node_id].data.state
                .available_moves()
                .peekable();
            if available_moves.peek().is_none() { panic!("No available moves"); }
            available_moves.for_each(|index| {
                let (player, score, state) = {
                    let ptot = tree[node_id].data.score.total();
                    let state = &tree[node_id].data.state;
                    make_move(ptot, state, index)
                };
                if player != tree[node_id].data.player {
                    node_id.append(tree, Cell {index, player, score, state });
                } else {
                    match state.status() {
                        Status::Finished => {
                            node_id.append(tree, Cell { index, player, score, state });
                        },
                        Status::Ongoing => {
                            let new_node = node_id
                                .append(tree, Cell { index, player, score, state });
                            expand_one_level(tree, new_node);
                        }
                    }
                }
            })
        }
    }
}

fn playout<M, P, T>(player: P, node_id: Token, tree: &mut Tree<M, P, T>,
                    nfold_guard: &Option<RepetitionGuard<T>>)
    where
        M: Default + Copy + Clone + Eq,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone + Eq + std::hash::Hash {

    // The core of the function that carries out simulations
    fn aux<M, P, T>(player: P, node_id: Token, tree: &mut Tree<M, P, T>,
                    loop_guard: &mut RepetitionGuard<T>,
                    nfold_guard: &Option<RepetitionGuard<T>>)
                   -> Outcome
        where
            M: Default + Copy + Clone + Eq,
            P: fmt::Display + Eq + Clone + Copy,
            T: Game<Move=M, Player=P> + Clone + Eq + std::hash::Hash {
        let fav_score_self = |a, b| comp(Score::gt_self, a, b);
        let fav_score_other = |a, b| comp(Score::gt_other, a, b);
        let score_eq = |a, b| comp(Score::feq, a, b);
        match node_id.node_type(tree) {
            NodeType::Node => {
                // TODO: Consider renaming this to something more descriptive
                let branch: Option<Token> = {
                    let mut nodes = node_id.children(tree)
                        .map(|x| (x.token(), x))
                        .filter(|(_, node)| 
                            match loop_guard.inc(node.data.state.clone()) {
                                Repetition::Acceptable => true,
                                Repetition::Excessive => false
                            }
                        )
                        .peekable();
                    if nodes.peek().is_some() {
                        if tree[node_id].data.player == player {
                            Some(pick(fav_score_self, score_eq, nodes))
                        } else {
                            Some(pick(fav_score_other, score_eq, nodes))
                        }
                    } else {
                        None
                    }
                };
                match branch {
                    Some(b) => {
                        let outcome = aux(player, b, tree, loop_guard, nfold_guard);
                        tree[node_id].data.score.update(outcome);
                        outcome
                    },
                    // in this case, both players are forced to return to the same
                    // game state the n-th time, in which case, the game is no
                    // longer meaningful and is therefore considered a draw
                    None => Outcome::Draw
                }
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
                        match nfold_guard {
                            Some(g) => {
                                let curr_state = &tree[node_id].data.state;
                                match g.query(curr_state) {
                                    Repetition::Acceptable => {
                                        expand_one_level(tree, node_id);
                                        aux(player, node_id, tree, loop_guard, nfold_guard)
                                    },
                                    Repetition::Excessive => Outcome::Draw
                                }
                            },
                            None => {
                                expand_one_level(tree, node_id);
                                aux(player, node_id, tree, loop_guard, nfold_guard)
                            }
                        }
                    }
                }
            }
        }
    }

    let mut loop_guard = RepetitionGuard::new(1);
    aux(player, node_id, tree, &mut loop_guard, nfold_guard);
}

/// Pick the most favorable move from the current game state
pub fn most_favored_move<M, P, T>(maxiter: usize, game_state: &T,
                                  nfold_guard: &Option<RepetitionGuard<T>>,
                                  dbg: &Debug) -> M
    where
        M: Default + Copy + Clone + Eq + fmt::Display,
        P: fmt::Display + Eq + Clone + Copy,
        T: Game<Move=M, Player=P> + Clone + Eq + std::hash::Hash {
    // short circuit if there's only one possible move
    // let count = game_state.available_moves().take(2).count();
    // if count == 1 {
    //     game_state.available_moves().next().unwrap()
    // } else {
        let player = game_state.current_player();
        let init_score = Score::new(1);
        let mut tree = Tree::new();
        let i_init = M::default();
        let state = game_state.clone();
        let root_node = tree.new_node(Cell {
           index: i_init,
           player,
           score: init_score,
           state
        });

        for _ in 0..maxiter {
            // FIXME: make 3 an argument in the function
            playout(player, root_node, &mut tree, nfold_guard);
        }

        match dbg {
            Debug::Release => (),
            Debug::Debug => {
                root_node.children(&tree)
                    .map(|x| &x.data)
                    .for_each(|cell| println!("{}  {}", cell.index, cell.score));
                println!();
            }
        };

        let _gt = |a, b| match comp(Score::cmp, a, b) {
            cmp::Ordering::Greater => true,
            _ => false
        };
        let _eq = |a, b| comp(Score::eq, a, b);
        let branches = root_node.children(&tree).map(|x| (x.token(), x));
        let most_promising_branch = pick(_gt, _eq, branches);
        tree[most_promising_branch].data.index
    // }
}
