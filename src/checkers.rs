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
extern crate unicode_segmentation;

use std::{fmt, num, str, iter};
use std::ops::{Mul, Div};

use console::{Style, Term};
use itertools::Itertools;
use unicode_segmentation::UnicodeSegmentation;

mod mcts;
mod ui;

use mcts::{Debug, Game, Status};
use ui::{Interactive, PlayerKind, Builder, Turing};
use ui::terminal;

const POW2: [BitBoard; 32] = [
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

const ROW_SUMS: [BitBoard; 8] = [
    BitBoard(170), BitBoard(21760), BitBoard(11141120), BitBoard(1426063360),
    BitBoard(730144440320), BitBoard(93458488360960),
    BitBoard(47850746040811520), BitBoard(6124895493223874560)
];

const BLANK_SPACES: BitBoard = BitBoard(12273903644374837845);
const TOP_ROW: BitBoard = BitBoard(170);
const BOTTOM_ROW: BitBoard = BitBoard(6124895493223874560);

enum Error {
    ParseInputError,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
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
    fn avail_captures(self,
                      opponent_pieces: BitBoard,
                      occupied: BitBoard,
                      direction: Direction)
        -> Box<dyn Iterator<Item=(Index, BitBoard)>> {
        // TODO: perhaps also accumulate the path taken as well
        // function output needs filtering to remove incomplete and trivial
        // moves
        fn aux(index: Index,
               captured: BitBoard,
               opponent: BitBoard,
               occupied: BitBoard,
               direction: Direction)
            -> Box<dyn Iterator<Item=(Index, BitBoard)>> {
            let capture = |r: BitBoard, x| {
                let capt = r.shift(x, direction) & opponent;
                let _dest = capt.shift(x, direction).rm(occupied);
                let dest = (_dest | r) - r;  // kings might "backtrack"
                match dest {
                    BitBoard(0) => (BitBoard(0), BitBoard(0)),
                    _ => (capt, dest)
                }
            };

            let bitboard = POW2[usize::from(index)];
            let (capture7, dest7) = capture(bitboard, 7);
            let (capture9, dest9) = capture(bitboard, 9);

            let itr = vec![(capture7, dest7), (capture9, dest9)].into_iter()
                .flat_map(move |(c, d)|
                    if c == BitBoard(0) {
                        Box::new(iter::once((index, captured)))
                    } else {
                        let capt = c | captured;
                        let occ = occupied - c;
                        let opp = opponent - c;
                        aux(d.iter().next().unwrap(), capt, opp, occ, direction)
                    }
                );
            Box::new(itr)
        }
        
        aux(self, BitBoard(0), opponent_pieces, occupied, direction)
   }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Movement(Index, Index);

#[derive(Copy, Clone, Debug, Eq)]
enum Move {
    Move(Movement),
    Capture(Movement, BitBoard)
}

impl Default for Move {
    fn default() -> Self {
        Move::Move(Movement(Index::default(), Index::default()))
    }
}

impl PartialEq for Move {
    fn eq(&self, rhs: &Self) -> bool {
        use crate::Move::*;
        match (self, rhs) {
            (Move(m1), Move(m2)) => m1 == m2,
            (Capture(m1, b1), Capture(m2, b2)) => m1 == m2 && b1 == b2,
            (Capture(m1, _), Move(m2)) | (Move(m1), Capture(m2, _)) => m1 == m2,
        }
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
                if pos0 == 0 || pos1 == 0 {
                    Err(Error::ParseInputError)
                } else {
                    Ok(Move::Move(Movement(Index(pos0 - 1), Index(pos1 - 1))))
                }
            },
            _ => Err(Error::ParseInputError)
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Move::Move(Movement(pos0, pos1)) =>
                write!(f, "move {} to {}", pos0.0 + 1, pos1.0 + 1),
            Move::Capture(Movement(pos0, pos1), captures) => {
                let c: String = captures.iter()
                    .flat_map(|Index(i)|
                        format!("{} ", i + 1).chars().collect::<Vec<char>>()
                    )
                    .collect();
                write!(f, "move {} to {}, capturing {}", pos0.0 + 1, pos1.0 + 1, c)
            }
        }
    }
}

#[derive(BitOr, BitAnd, Add, Sub, PartialEq, Eq, PartialOrd, Ord, Copy, Clone,
         Debug, From, Shl, Shr, Hash)]
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
    fn iter(self) -> BitIterator { BitIterator { cursor: 0, bitboard: self } }

    fn rm(self, bits: Self) -> Self { (self | bits) - bits }

    fn rm_invalid_pos(self) -> Self { self.rm(BLANK_SPACES) }

    fn rm_top_row(self) -> Self { self.rm(TOP_ROW) }

    fn rm_bottom_row(self) -> Self { self.rm(BOTTOM_ROW) }

    fn shift(self, binary_shift: u8, direction: Direction) -> Self {
        match direction {
            Direction::Up => self.rm_top_row() >> binary_shift,
            Direction::Down => self.rm_bottom_row() << binary_shift,
            Direction::Omni => self.rm_bottom_row() << binary_shift |
                self.rm_top_row() >> binary_shift
        }.rm_invalid_pos()
    }

    fn mv(self, p0: Index, p1: Index) -> Self {
        if self & POW2[usize::from(p0)] != BitBoard(0) {
            let origin_piece = POW2[usize::from(p0)];
            let dest_piece = POW2[usize::from(p1)];
            self.rm(origin_piece) + dest_piece
        } else {
            self
        }
    }

    fn avail_mvs(self, occupied: Self, dir: Direction)
        -> impl Iterator<Item=Move> {
        self.iter()
            .flat_map(move |i0| {
                let piece = POW2[usize::from(i0)];
                (piece.shift(7, dir) | piece.shift(9, dir))
                .rm(occupied)
                .iter()
                .map(move |i1| Move::Move(Movement(i0, i1)))
            })
    }

    fn avail_captures(self, opponent_pieces: Self, occupied: Self, dir: Direction)
        -> impl Iterator<Item=Move> {
        self.iter()
            .flat_map(move |indx|
                indx.avail_captures(opponent_pieces, occupied, dir)
                    .zip(iter::repeat(indx))
                    .filter(|((i1, _), i0)| i0 != i1)  // remove trivial moves
                    // remove incomplete moves
                    .coalesce(|x, y| {
                        let ((_, bx), _) = x;
                        let ((_, by), _) = y;
                        if bx | by == bx { Ok(x) }
                        else if bx | by == by { Ok(y) }
                        else { Err((x, y)) }
                    })
                    .map(|((i1, b), i0)| Move::Capture(Movement(i0, i1), b))
            )
    }
}

#[derive(Clone, Copy)]
enum Direction { Up, Down, Omni }

#[derive(Clone, Copy)]
struct BitIterator {
    cursor: u8,
    bitboard: BitBoard
}

impl Iterator for BitIterator {
    type Item = Index;
    fn next(&mut self) -> Option<Self::Item> {
        // TODO: test validity of the skipping logic
        // skip over empty rows
        if self.cursor % 4 == 0 {
            let nempty_rows = ROW_SUMS[self.cursor as usize / 4..].iter()
                .take_while(|&&x| (x | self.bitboard) - self.bitboard == x)
                .count();
            self.cursor += 4 * nempty_rows as u8;
        }
        loop {
            if self.cursor > 31 {
                break None
            // doesn't seem to be necessary at all because of skipping
            // } else if  POW2[usize::from(self.cursor)] > self.bitboard {
            //     break None
            } else {
                let i = Index(self.cursor as u8);
                if POW2[usize::from(self.cursor)] & self.bitboard == BitBoard(0) {
                    self.cursor += 1
                } else {
                    self.cursor += 1;
                    break Some(i)
                }
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum BitBoardVariants {
    RedMen(BitBoard),
    BlackMen(BitBoard),
    Kings(BitBoard)
}

impl BitBoardVariants {
    fn unwrap(self) -> BitBoard {
        use BitBoardVariants::*;
        match self {
            RedMen(b) | BlackMen(b) | Kings(b) => b
        }
    }

    fn mv(self, p0: Index, p1: Index) -> Self {
        use BitBoardVariants::*;
        match self {
            RedMen(x) => RedMen(x.mv(p0, p1)),
            BlackMen(x) => BlackMen(x.mv(p0, p1)),
            Kings(x) => Kings(x.mv(p0, p1)),
        }
    }

    fn rm(self, pieces: BitBoard) -> Self {
        use BitBoardVariants::*;
        match self {
            RedMen(x) => RedMen(x.rm(pieces)),
            BlackMen(x) => BlackMen(x.rm(pieces)),
            Kings(x) => Kings(x.rm(pieces)),
        }
    }

    fn avail_mvs(self, occupied: BitBoard)
        -> Box<dyn Iterator<Item=Move> + 'static> {
        use BitBoardVariants::*;
        match self {
            BlackMen(b) => Box::new(b.avail_mvs(occupied, Direction::Down)),
            RedMen(b) => Box::new(b.avail_mvs(occupied, Direction::Up)),
            Kings(b) => Box::new(
                b.avail_mvs(occupied, Direction::Up)
                 .chain(b.avail_mvs(occupied, Direction::Down))
            )
        }
    }

    fn avail_captures(self, opponent_pieces: BitBoard, occupied: BitBoard)
        -> impl Iterator<Item=Move> {
        use BitBoardVariants::*;
        match self {
            BlackMen(b) => b.avail_captures(opponent_pieces, occupied, Direction::Down),
            RedMen(b) => b.avail_captures(opponent_pieces, occupied, Direction::Up),
            Kings(b) => b.avail_captures(opponent_pieces, occupied, Direction::Omni)
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
/// The CheckersState struct. This is the internal representation of the
/// game state during play.
struct CheckersState {
    /// The player that has the turn
    curr_player: Player,
    /// Black player
    black_player: Player,
    /// Red player
    red_player: Player,
    /// Men of the current player
    curr_men: BitBoardVariants,
    /// Kings of the current player
    curr_kings: BitBoardVariants,
    /// Men of the opposite player
    oppo_men: BitBoardVariants,
    /// Kings of the opposite player
    oppo_kings: BitBoardVariants
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
                .graphemes(true)
                .rev()
                .collect::<String>()
        };

        let (black_men_fbb, black_kings_fbb, red_men_fbb, red_kings_fbb) =
            match self.curr_player {
                Player::Black(_) =>
                    (self.curr_men, self.curr_kings, self.oppo_men, self.oppo_kings),
                Player::Red(_) =>
                    (self.oppo_men, self.oppo_kings, self.curr_men, self.curr_kings)
            };

        let black_men = int_to_padded_str(black_men_fbb.unwrap());
        let black_kings = int_to_padded_str(black_kings_fbb.unwrap());
        let red_men = int_to_padded_str(red_men_fbb.unwrap());
        let red_kings = int_to_padded_str(red_kings_fbb.unwrap());
        let blank_spaces = int_to_padded_str(BLANK_SPACES);
        let symbol_conv = |(a, b)| match (a, b) {
            ('1', '0') => 'm',
            ('0', '1') => 'k',
            ('0', '0') => ' ',
            e => panic!("{:?}", e)
        };
        let red = Style::new().red();
        // use blue instead of black for better displah in terminals
        let blue = Style::new().blue();
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
                e => panic!("{:?}", e)
            });
        black_str.zip(red_str)
            .zip(blank_spaces_str)
            .chunks(8)
            .into_iter()
            .for_each(|l| {
                l.for_each(|((b, r), sp)| match (b, r, sp) {
                    ('m', ' ', '+') => print!("{}", blue.apply_to('●')),
                    ('k', ' ', '+') => print!("{}", blue.apply_to('♚')),
                    (' ', 'm', '+') => print!("{}", red.apply_to('●')),
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
        use BitBoardVariants::*;

        let (black_player, red_player) = match players[..] {
            [Black(a), Red(b)] => (Black(a), Red(b)),
            [Red(b), Black(a)] => (Black(a), Red(b)),
            _ => panic!("Invalid input")   // inaccessible branch
        };

        CheckersState {
            curr_player: black_player,
            black_player,
            red_player,
            curr_men: BlackMen(BitBoard(11163050)),
            curr_kings: Kings(BitBoard(0)),
            oppo_men: RedMen(BitBoard(6172839697753047040)),
            oppo_kings: Kings(BitBoard(0)),
        }
    }

    fn available_moves(&self) -> Self::Moves {
        let occupied = self.curr_men.unwrap() + self.curr_kings.unwrap() +
            self.oppo_men.unwrap() + self.oppo_kings.unwrap();
        let opponent_occupied = self.oppo_kings.unwrap() + self.oppo_men.unwrap();

        let moves = self.curr_men.avail_mvs(occupied)
            .chain(self.curr_kings.avail_mvs(occupied));
        let mut captures = self.curr_men.avail_captures(opponent_occupied, occupied)
            .chain(self.curr_kings.avail_captures(opponent_occupied, occupied))
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
        // if no moves left
        if self.available_moves().next().is_none() {
            Status::Finished
        } else {
            Status::Ongoing
        }
    }

    fn mv(&self, mv: Move) -> Self {
        use BitBoardVariants::*;
        // FIXME: a stop gap solution to get over the inability to parse
        // captures
        let mv = self.available_moves().find(|&m| m == mv).unwrap();
        let move_then_crown = |p0, p1|
            match (self.curr_men.mv(p0, p1), self.curr_kings.mv(p0, p1)) {
                (BlackMen(m), Kings(k)) => 
                    (BlackMen(m - (m & BOTTOM_ROW)), Kings(k + (m & BOTTOM_ROW))),
                (RedMen(m), Kings(k)) =>
                    (RedMen(m - (m & TOP_ROW)), Kings(k + (m & TOP_ROW))),
                _ => panic!("inaccessible branch")
            };

        let (oppo_men, oppo_kings, curr_men, curr_kings) = match mv {
            Move::Move(Movement(p0, p1)) => {
                let (oppo_men, oppo_kings) = move_then_crown(p0, p1);
                (oppo_men, oppo_kings, self.oppo_men, self.oppo_kings)
            },
            Move::Capture(Movement(p0, p1), p) => {
                let (oppo_men, oppo_kings) = move_then_crown(p0, p1);
                (oppo_men, oppo_kings,
                 self.oppo_men.rm(p), self.oppo_kings.rm(p))
            }
        };

        let curr_player = self.next_player();
        CheckersState { curr_men, curr_kings, oppo_men, oppo_kings, curr_player, ..*self }
    }

    fn winner(&self) -> Option<Player> {
        if self.oppo_kings.unwrap() + self.oppo_men.unwrap() == BitBoard(0) {
            Some(self.curr_player)
        } else if self.curr_kings.unwrap() + self.curr_men.unwrap() == BitBoard(0) {
            Some(self.next_player())
        // MCTS only calls this function when the status function returns
        // Status::Finished. That could mean the current player still has pieces
        // on the board but ran out of legal moves (not counting the repetition
        // rules), in which case the current player lost.
        } else if self.curr_kings.unwrap() + self.curr_men.unwrap() != BitBoard(0) {
            Some(self.next_player())
        } else {
            None
        }
    }
}

fn main() -> Result<(), terminal::Error> {
    ui::terminal::launch_game::<CheckersState, Move, Player>("Checkers")
}
