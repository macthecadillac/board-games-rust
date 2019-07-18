#[macro_use]
extern crate clap;

#[macro_use]
extern crate derive_more;

extern crate console;
extern crate indextree;
extern crate itertools;
extern crate rand;
extern crate unicode_segmentation;

use std::{fmt, num, str};
use std::ops::{Mul, Div};

use console::{Style, Term};
use itertools::Itertools;
use unicode_segmentation::UnicodeSegmentation;

mod mcts;
mod ui;

use mcts::{Debug, Initialize, Game, Status};
use ui::{Interactive, PlayerKind, Builder, Turing};
use ui::terminal;

static POW2: [BitBoard; 32] = [
    BitBoard(2), BitBoard(8), BitBoard(32), BitBoard(128), BitBoard(256),
    BitBoard(1024), BitBoard(4096), BitBoard(16384), BitBoard(131072),
    BitBoard(524288), BitBoard(2097152), BitBoard(8388608), BitBoard(16777216),
    BitBoard(67108864), BitBoard(268435456), BitBoard(1073741824),
    BitBoard(8589934592), BitBoard(34359738368), BitBoard(137438953472),
    BitBoard(549755813888), BitBoard(1099511627776), BitBoard(4398046511104),
    BitBoard(17592186044416), BitBoard(70368744177664),
    BitBoard(562949953421312), BitBoard(2251799813685248),
    BitBoard(9007199254740992), BitBoard(36028797018963968),
    BitBoard(72057594037927936), BitBoard(288230376151711744),
    BitBoard(1152921504606846976), BitBoard(4611686018427387904)
];

enum Error {
    ParseInputError,
}

#[derive(Copy, Clone)]
/// The player type
enum Player {
    /// Player one
    Black(PlayerKind),
    /// Player two
    Red(PlayerKind)
}

impl Builder for Player {
    fn new(int: usize, kind: PlayerKind) -> Option<Player> {
        match int {
            1 => Some(Player::Black(kind)),
            2 => Some(Player::Red(kind)),
            _ => None
        }
    }
}

impl Turing for Player {
    fn turing_test(&self) -> PlayerKind {
        use Player::*;
        use PlayerKind::*;
        match self {
            Red(Human) | Black(Human) => Human,
            Red(Bot) | Black(Bot) => Bot
        }
    }
}

impl PartialEq for Player {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            // We don't really care about the player kind here since there can
            // only be one player 1 and so on in the game
            (Player::Black(_), Player::Black(_)) |
                (Player::Red(_), Player::Red(_)) => true,
            _ => false
        }
    }
}

impl Eq for Player {}

impl fmt::Display for Player {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Player::Black(_) => write!(f, "1"),
            Player::Red(_) => write!(f, "2")
        }
    }
}

#[derive(Add, Sub, PartialEq, Eq, Copy, Clone, Debug, Display)]
struct Index(u8);

impl Initialize for Index {
    fn new() -> Self { Index(0) }
}

impl Index {
    fn as_usize(&self) -> usize {
        let Index(i) = *self;
        i as usize
    }

    fn inc(&self) -> Self { *self + Index(1) }

    fn dec(&self) -> Self { *self - Index(1) }
}

impl str::FromStr for Index {
    type Err = num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let n = s.parse::<u8>()?;
        Ok(Index(n))
    }
}

#[derive(Copy, Clone, Debug)]
struct Movement {
    pos0: Index,
    pos1: Index
}

#[derive(Copy, Clone, Debug)]
enum Move {
    Move(Movement),
    Capture(Movement)
}

impl Initialize for Move {
    fn new() -> Self {
        Move::Move(Movement { pos0: Index::new(), pos1: Index::new() })
    }
}

impl str::FromStr for Move {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parse = |i: &str| match i.parse::<u8>() {
            Ok(p) => Ok(p),
            Err(_) => Err(Error::ParseInputError)
        };
        match &s.split(|c| c == ' ' || c == 'x' || c == '-')
                .collect::<Vec<&str>>()[..] {
            &[p0, p1] => {
                let pos0 = parse(p0)?;
                let pos1 = parse(p1)?;
                Ok(Move::Move(Movement { pos0: Index(pos0), pos1: Index(pos1) }))
            },
            _ => Err(Error::ParseInputError)
        }
    }
}

#[derive(BitOr, Add, Sub, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
/// The BitBoard struct. This is the memoery representation of the
/// checkers game state for one of the players. For a checkerboard with 64
/// squares, we denote the squares with powers of 2 up to 2^63, starting with
/// 2^0 at the lower right corner.
struct BitBoard(u64);

impl fmt::Binary for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let BitBoard(u) = self;
        write!(f, "{:b}", u)
    }
}

impl Mul for BitBoard {
    type Output = BitBoard;
    fn mul(self, rhs: Self) -> Self { BitBoard(self.0 * rhs.0) }
}

impl Div for BitBoard {
    type Output = BitBoard;
    fn div(self, rhs: Self) -> Self { BitBoard(self.0 / rhs.0) }
}

#[derive(Clone)]
/// The CheckersState struct. This is the internal representation of the
/// game state during play.
struct CheckersState {
    /// The player that has the turn
    curr_player: Player,
    /// Black player
    black_player: Player,
    /// Red player
    red_player: Player,
    /// Men of the black player.
    black_men: BitBoard,
    /// Kings of the black player
    black_kings: BitBoard,
    /// Men of the red player
    red_men: BitBoard,
    /// Kings of the red player
    red_kings: BitBoard
}

impl Game for CheckersState {
    type Move = Move;
    type Player = Player;

    fn new(players: Vec<Self::Player>) -> Self {
        use Player::*;

        let (black_player, red_player) = match &players[..] {
            &[Black(a), Red(b)] => (Black(a), Red(b)),
            &[Red(b), Black(a)] => (Black(a), Red(b)),
            _ => panic!("Invalid input")   // inaccessible branch
        };

        CheckersState {
            curr_player: black_player,
            black_player,
            red_player,
            black_men: BitBoard(11163050),
            black_kings: BitBoard(0),
            red_men: BitBoard(6172839697753047040),
            red_kings: BitBoard(0),
        }
    }

    fn available_moves(&self) -> Vec<Move> {
        let blank_spaces = BitBoard(12273903644374837845);
        let occupied = self.black_men + self.black_kings +
            self.red_men + self.red_kings;

        let adj_vacant_mvs = |shift: BitBoard| {
            let vacants = (shift | blank_spaces) - blank_spaces;
            (vacants | occupied) - occupied
        };

        let upper_avail_mvs =
            |b: BitBoard| adj_vacant_mvs(b * BitBoard(32) | b * BitBoard(128));

        let lower_avail_mvs =
            |b: BitBoard| adj_vacant_mvs(b / BitBoard(32) | b / BitBoard(128));

    }
}

fn main() {
}
