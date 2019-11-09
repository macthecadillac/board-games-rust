#![allow(clippy::unreadable_literal)]
#![allow(clippy::precedence)]
#![allow(clippy::if_same_then_else)]
#[macro_use]
extern crate clap;

#[macro_use]
extern crate derive_more;

extern crate console;
extern crate indextree;
extern crate itertools;
extern crate rand;

use std::{fmt, num, str, iter};
use std::ops::{Mul, Div, BitAnd};

use console::{Style, Term};
use itertools::Itertools;

mod mcts;
mod ui;

use mcts::{Debug, Game, Status};
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

static BLANK_SPACES: BitBoard = BitBoard(12273903644374837845);

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
        use Player::*;
        match (self, rhs) {
            // We don't really care about the player kind here since there can
            // only be one player 1 and so on in the game
            (Black(_), Black(_)) | (Red(_), Red(_)) => true,
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

#[derive(Add, Sub, PartialEq, Eq, Copy, Clone, Debug, Display, Default)]
struct Index(u8);

impl From<Index> for usize {
    fn from(item: Index) -> usize {
        let Index(i) = item;
        i as usize
    }
}

impl str::FromStr for Index {
    type Err = num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let n = s.parse::<u8>()?;
        Ok(Index(n))
    }
}

impl Index {
    // TODO: perhaps also accumulate the path taken as well
    // TODO: using an explicit loop and return a stack allocated iterator could
    // be faster
    fn _avail_captures(self,
                       captured: BitBoard,
                       opponent: BitBoard,
                       occupied: BitBoard,
                       rm_row: fn(BitBoard) -> BitBoard,
                       op: fn(BitBoard, BitBoard) -> BitBoard)
        -> Box<dyn Iterator<Item=(Index, BitBoard)>> {
        let captures = |r, x| op(rm_row(r), BitBoard(x))
            .rm_invalid_pos()
            .bitand(opponent);
        let dest = |c, x| op(rm_row(c), BitBoard(x)).rm_invalid_pos();
        let bb = POW2[usize::from(self)];
        let capture32 = captures(bb, 32);
        let capture128 = captures(bb, 128);
        let dest32 = dest(capture32, 32);
        let dest128 = dest(capture128, 128);
        let capture = (capture32 | capture128) | captured;
        let dest = (dest32 | dest128).rm_occupied(occupied);

        if dest != BitBoard(0) && capture != BitBoard(0) {
            Box::new(
                dest.iter()
                    .flat_map(move |i| 
                        i._avail_captures(capture, opponent, occupied, rm_row, op)
                         .zip(iter::repeat(capture))
                         .map(|((i, b), _)| (i, b))
                    )
            )
        } else {
            Box::new(iter::empty())
        }
    }

    fn avail_captures(self,
                      opponent_pieces: BitBoard,
                      occupied: BitBoard,
                      rm_row: fn(BitBoard) -> BitBoard,
                      op: fn(BitBoard, BitBoard) -> BitBoard)
        -> Box<dyn Iterator<Item=(Index, BitBoard)>> {
        self._avail_captures(BitBoard(0), opponent_pieces, occupied, rm_row, op)
   }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Movement(Index, Index);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Move {
    Move(Movement),
    Capture(Movement, BitBoard)
}

impl Default for Move {
    fn default() -> Self {
        Move::Move(Movement(Index::default(), Index::default()))
    }
}

impl str::FromStr for Move {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parse = |i: &str| match i.parse::<u8>() {
            Ok(p) => Ok(p),
            Err(_) => Err(Error::ParseInputError)
        };
        match s.split(|c| c == ' ' || c == 'x' || c == '-')
                .collect::<Vec<&str>>()[..] {
            [p0, p1] => {
                let pos0 = parse(p0)?;
                let pos1 = parse(p1)?;
                Ok(Move::Move(Movement(Index(pos0), Index(pos1))))
            },
            _ => Err(Error::ParseInputError)
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Move::Move(Movement(pos0, pos1)) =>
                write!(f, "move {} to {}", pos0, pos1),
            Move::Capture(Movement(pos0, pos1), captures) => {
                let c: String = captures.iter()
                    .flat_map(|Index(i)|
                        format!("{} ", i).chars().collect::<Vec<char>>()
                    )
                    .collect();
                write!(f, "move {} to {}, capturing {}", pos0, pos1, c)
            }
        }
    }
}

#[derive(BitOr, BitAnd, Add, Sub, PartialEq, Eq, PartialOrd, Ord, Copy, Clone,
         Debug, From)]
/// The BitBoard struct. This is the memoery representation of the
/// checkers game state for one of the players. For a checkerboard with 64
/// squares, we denote the squares with powers of 2 up to 2^63, starting with
/// 2^1 at the upper left corner and iterate with odd powers of 2.
struct BitBoard(u64);

// FIXME: make the fmt output human readable
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

impl BitBoard {
    fn iter(self) -> BitIterator {
        BitIterator { cursor: 0, bitboard: self }
    }

    fn rm_invalid_pos(self) -> Self {
        (self | BLANK_SPACES) - BLANK_SPACES
    }

    fn rm_occupied(self, occupied: Self) -> Self {
        (self | occupied) - occupied
    }

    fn rm_top_row(self) -> Self {
        (self | BitBoard(170)) - BitBoard(170)
    }

    fn rm_bottom_row(self) -> Self {
        (self | BitBoard(6124895493223874560)) - BitBoard(6124895493223874560)
    }

    // TODO: move to impl Index
    fn avail_mvs(self, occupied: Self,
                 rm_row: fn(BitBoard) -> BitBoard,
                 op: fn(BitBoard, BitBoard) -> BitBoard)
        -> impl Iterator<Item=Move> {
        rm_row(self)
            .iter()
            .flat_map(move |i0| {
                let bb = POW2[usize::from(i0)];
                (op(bb, BitBoard(32)) | op(bb, BitBoard(128)))
                .rm_invalid_pos()
                .rm_occupied(occupied)
                .iter()
                .map(move |i1| Move::Move(Movement(i0, i1)))
            })
    }

    fn lower_avail_mvs(self, occupied: Self) -> impl Iterator<Item=Move> {
        self.avail_mvs(occupied, BitBoard::rm_bottom_row, Mul::mul)
    }

    fn upper_avail_mvs(self, occupied: Self) -> impl Iterator<Item=Move> {
        self.avail_mvs(occupied, BitBoard::rm_top_row, Mul::mul)
    }

    fn avail_captures(self, opponent_pieces: Self, occupied: Self,
                          rm_row: fn(BitBoard) -> BitBoard,
                          op: fn(BitBoard, BitBoard) -> BitBoard)
        -> impl Iterator<Item=(Index, BitBoard)> {
        rm_row(self)
            .iter()
            .flat_map(move |indx|
                indx.avail_captures(opponent_pieces, occupied, rm_row, op)
            )
    }

    fn lower_avail_captures(self, opponent: Self, occupied: Self)
        -> impl Iterator<Item=(Index, BitBoard)> {
        self.avail_captures(opponent, occupied, BitBoard::rm_bottom_row, Mul::mul)
    }

    fn upper_avail_captures(self, opponent: Self, occupied: Self)
        -> impl Iterator<Item=(Index, BitBoard)> {
        self.avail_captures(opponent, occupied, BitBoard::rm_top_row, Div::div)
    }
}

#[derive(Clone, Copy)]
struct BitIterator {
    cursor: u8,
    bitboard: BitBoard
}

impl Iterator for BitIterator {
    type Item = Index;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cursor > 31 {
                break None
            } else if self.bitboard > POW2[usize::from(self.cursor)] {
                break None
            } else {
                let i = Index(self.cursor);
                if POW2[usize::from(self.cursor)] & self.bitboard == BitBoard(0) {
                    self.cursor += 1;
                    break Some(i)
                } else {
                    self.cursor += 1
                }
            }
        }
    }
}

#[derive(Copy, Clone)]
enum FlaggedBitBoard {
    RedMen(BitBoard),
    BlackMen(BitBoard),
    Kings(BitBoard)
}

impl FlaggedBitBoard {
    fn unwrap(self) -> BitBoard {
        use FlaggedBitBoard::*;
        match self {
            RedMen(b) | BlackMen(b) | Kings(b) => b
        }
    }

    fn avail_mvs(self, occupied: BitBoard)
        -> Box<dyn Iterator<Item=Move> + 'static> {
        use FlaggedBitBoard::*;
        match self {
            BlackMen(b) => Box::new(b.lower_avail_mvs(occupied)),
            RedMen(b) => Box::new(b.upper_avail_mvs(occupied)),
            Kings(b) => Box::new(
                b.lower_avail_mvs(occupied)
                    .chain(b.upper_avail_mvs(occupied))
            )
        }
    }

    fn avail_captures(self, opponent_pieces: BitBoard, occupied: BitBoard)
        -> impl Iterator<Item=Move> {
        fn black_captures(b: BitBoard, opp: BitBoard, occ: BitBoard)
            -> Box<dyn Iterator<Item=((Index, BitBoard), Index)> + 'static> {
            Box::new(
                b.iter()
                    .flat_map(move |indx|
                        POW2[usize::from(indx)]
                            .lower_avail_captures(opp, occ)
                            .zip(iter::repeat(indx))
                    )
            )
        }

        fn red_captures(b: BitBoard, opp: BitBoard, occ: BitBoard)
            -> Box<dyn Iterator<Item=((Index, BitBoard), Index)> + 'static> {
            Box::new(
                b.iter()
                    .flat_map(move |indx|
                        POW2[usize::from(indx)]
                            .upper_avail_captures(opp, occ)
                            .zip(iter::repeat(indx))
                    )
            )
        }

        fn king_captures(b: BitBoard, opp: BitBoard, occ: BitBoard)
            -> Box<dyn Iterator<Item=((Index, BitBoard), Index)> + 'static> {
            Box::new(
                b.iter()
                    .flat_map(move |indx|
                        POW2[usize::from(indx)]
                            .upper_avail_captures(opp, occ)
                            .chain(POW2[usize::from(indx)]
                                .lower_avail_captures(opp, occ))
                            .zip(iter::repeat(indx))
                    )
            )
        }

        use FlaggedBitBoard::*;
        match self {
            BlackMen(b) => black_captures(b, opponent_pieces, occupied),
            RedMen(b) => red_captures(b, opponent_pieces, occupied),
            Kings(b) => king_captures(b, opponent_pieces, occupied)
        }
        .map(|((i1, b), i0)| Move::Capture(Movement(i0, i1), b))
    }
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
    black_men: FlaggedBitBoard,
    /// Kings of the black player
    black_kings: FlaggedBitBoard,
    /// Men of the red player
    red_men: FlaggedBitBoard,
    /// Kings of the red player
    red_kings: FlaggedBitBoard
}

impl Interactive<Move> for CheckersState {
    fn is_valid_move(&self, mv: &Move) -> bool {
        self.available_moves().any(|x| x == *mv)
    }
}

impl fmt::Display for CheckersState {
    fn fmt(&self, _: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let int_to_padded_str = |bitboard: BitBoard| {
            let bitstring = format!("{:b}", bitboard);
            format!("{:0>64}", bitstring)
        };
        let black_men = int_to_padded_str(self.black_men.unwrap());
        let black_kings = int_to_padded_str(self.black_kings.unwrap());
        let red_men = int_to_padded_str(self.red_men.unwrap());
        let red_kings = int_to_padded_str(self.red_kings.unwrap());
        let blank_spaces = int_to_padded_str(BLANK_SPACES);
        let symbol_conv = |(a, b)| match (a, b) {
            ('1', '0') => 'm',
            ('0', '1') => 'k',
            ('0', '0') => ' ',
            _ => panic!("Check your code!")
        };
        let red = Style::new().red().bold();
        // use blue instead of black for better displah in terminals
        let blue = Style::new().blue().bold();
        let white = Style::new().on_white();
        let black = Style::new().black();

        let black_str = black_men.chars()
            .zip(black_kings.chars())
            .map(symbol_conv);
        let red_str = red_men.chars()
            .zip(red_kings.chars())
            .map(symbol_conv);
        let blank_spaces_str = blank_spaces.chars()
            .map(|c| match c {
                '0' => '+',
                '1' => '-',
                _ => panic!("impossible branch")
            });
        black_str.zip(red_str)
            .zip(blank_spaces_str)
            .chunks(8)
            .into_iter()
            .for_each(|l| {
                l.for_each(|((b, r), sp)| match (b, r, sp) {
                    ('m', ' ', '+') => print!("{}", blue.apply_to('⬤')),
                    ('k', ' ', '+') => print!("{}", blue.apply_to('♚')),
                    (' ', 'm', '+') => print!("{}", red.apply_to('⬤')),
                    (' ', 'k', '+') => print!("{}", red.apply_to('♚')),
                    (' ', ' ', '+') => print!("{}", black.apply_to(' ')),
                    (' ', ' ', '-') => print!("{}", white.apply_to(' ')),
                    _ => panic!("Check your code!")
                });
                println!()
            });
        Ok(())
    }
}

impl terminal::Terminal for CheckersState {
    type Debug = Debug;

    fn game_end_screen(&self, debug: &Debug) -> Result<(), terminal::Error> {
        let term = Term::stdout();
        let result = match debug {
            Debug::Release => term.clear_screen(),
            Debug::Debug => Ok(())
        };
        println!("\n{}\n", self);
        match result {
            Ok(_) => Ok(()),
            Err(_) => Err(terminal::Error::ClearScreenError)
        }
    }
}

// FIXME: use impl Trait over trait objects once that is stabilized for trait
// methods
impl Game for CheckersState {
    type Player = Player;
    type Move = Move;
    type Moves = Box<dyn Iterator<Item=Move> + 'static>;

    fn new(players: Vec<Player>) -> Self {
        use Player::*;
        use FlaggedBitBoard::*;

        let (black_player, red_player) = match players[..] {
            [Black(a), Red(b)] => (Black(a), Red(b)),
            [Red(b), Black(a)] => (Black(a), Red(b)),
            _ => panic!("Invalid input")   // inaccessible branch
        };

        CheckersState {
            curr_player: black_player,
            black_player,
            red_player,
            black_men: BlackMen(BitBoard(11163050)),
            black_kings: Kings(BitBoard(0)),
            red_men: RedMen(BitBoard(6172839697753047040)),
            red_kings: Kings(BitBoard(0)),
        }
    }

    fn available_moves(&self) -> Self::Moves {
        // FIXME: need to add mechanism for crowning
        let occupied = self.black_men.unwrap() + self.black_kings.unwrap() +
            self.red_men.unwrap() + self.red_kings.unwrap();
        let opponent_occupied = match self.curr_player {
            Player::Black(_) => self.red_kings.unwrap() + self.red_men.unwrap(),
            Player::Red(_) => self.black_kings.unwrap() + self.black_men.unwrap(),
        };
        let (self_men, self_kings) = match self.curr_player {
            Player::Black(_) => (self.black_men, self.black_kings),
            Player::Red(_) => (self.red_men, self.red_kings),
        };

        let moves = self_men.avail_mvs(occupied)
            .chain(self_kings.avail_mvs(occupied));
        let mut captures = self_men.avail_captures(opponent_occupied, occupied)
            .chain(self_kings.avail_captures(opponent_occupied, occupied))
            .peekable();

        if captures.peek().is_some() { Box::new(captures) }
        else { Box::new(moves) }
    }

    fn current_player(&self) -> Player { self.curr_player }

    fn next_player(&self) -> Player {
        match self.curr_player {
            Player::Black(_) => self.red_player,
            Player::Red(_) => self.black_player
        }
    }

    fn status(&self) -> Status {
        if self.black_kings.unwrap() + self.black_men.unwrap() == BitBoard(0)
            || self.red_kings.unwrap() + self.red_men.unwrap() == BitBoard(0) {
            Status::Finished
        } else {
            Status::Ongoing
        }
    }

    fn mv(&self, mv: Move) -> Self {
        let (self_men, self_kings, opponent_men, opponent_kings) =
            match self.curr_player {
            Player::Black(_) =>
                (self.black_men.unwrap(), self.black_kings.unwrap(),
                 self.red_men.unwrap(), self.red_kings.unwrap()),
            Player::Red(_) =>
                (self.red_men.unwrap(), self.red_kings.unwrap(),
                 self.black_men.unwrap(), self.black_kings.unwrap())
        };

        let rm = |x, pieces| (x | pieces) - pieces;
        let aux = |x, p0, p1| x - POW2[usize::from(p0)] + POW2[usize::from(p1)];
        let mv_pieces = |men, kings, p0, p1| (aux(men, p0, p1), aux(kings, p0, p1));
        let (self_men, self_kings, opponent_men, opponent_kings) = match mv {
            Move::Move(Movement(p0, p1)) => {
                let (self_men, self_kings) = mv_pieces(self_men, self_kings, p0, p1);
                (self_men, self_kings, opponent_men, opponent_kings)
            },
            Move::Capture(Movement(p0, p1), p) => {
                let (self_men, self_kings) = mv_pieces(self_men, self_kings, p0, p1);
                (self_men, self_kings, rm(opponent_men, p), rm(opponent_kings, p))
            }
        };

        use FlaggedBitBoard::*;
        let (red_men, red_kings, black_men, black_kings) = match self.curr_player {
            Player::Black(_) =>
                (RedMen(opponent_men), Kings(opponent_kings),
                 BlackMen(self_men), Kings(self_kings)),
            Player::Red(_) =>
                (RedMen(self_men), Kings(self_kings),
                 BlackMen(opponent_men), Kings(opponent_kings)),
        };
        CheckersState { red_men, red_kings, black_men, black_kings, ..*self }
    }

    fn winner(&self) -> Option<Player> {
        if self.black_kings.unwrap() + self.black_men.unwrap() == BitBoard(0) {
            Some(self.red_player)
        } else if self.red_kings.unwrap() + self.red_men.unwrap() == BitBoard(0) {
            Some(self.black_player)
        } else {
            None
        }
    }
}

fn main() -> Result<(), terminal::Error> {
    ui::terminal::launch_game::<CheckersState, Move, Player>("Checkers")
}
