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

use console::{Style, Term};
use itertools::Itertools;
use unicode_segmentation::UnicodeSegmentation;

mod mcts;
mod tree;
mod ui;

use mcts::{Debug, Initialize, Game, Status};
use ui::{Interactive, PlayerKind, Builder, Turing};
use ui::terminal;

/// Bit-board representation of the winning configurations. The configurations
/// from smallest to largest represent configurations from lower left corner to
/// the right and up
static WINCONFIGS: [BitBoard; 69] = [
    BitBoard(15), BitBoard(30), BitBoard(60), BitBoard(120), BitBoard(1920),
    BitBoard(3840), BitBoard(7680), BitBoard(15360), BitBoard(245760),
    BitBoard(491520), BitBoard(983040), BitBoard(1966080), BitBoard(2113665),
    BitBoard(2130440), BitBoard(4227330), BitBoard(4260880), BitBoard(8454660),
    BitBoard(8521760), BitBoard(16843009), BitBoard(16909320),
    BitBoard(17043520), BitBoard(31457280), BitBoard(33686018),
    BitBoard(33818640), BitBoard(62914560), BitBoard(67372036),
    BitBoard(67637280), BitBoard(125829120), BitBoard(134744072),
    BitBoard(135274560), BitBoard(251658240), BitBoard(270549120),
    BitBoard(272696320), BitBoard(541098240), BitBoard(545392640),
    BitBoard(1082196480), BitBoard(1090785280), BitBoard(2155905152),
    BitBoard(2164392960), BitBoard(2181570560), BitBoard(4026531840),
    BitBoard(4311810304), BitBoard(4328785920), BitBoard(8053063680),
    BitBoard(8623620608), BitBoard(8657571840), BitBoard(16106127360),
    BitBoard(17247241216), BitBoard(17315143680), BitBoard(32212254720),
    BitBoard(34630287360), BitBoard(34905128960), BitBoard(69260574720),
    BitBoard(69810257920), BitBoard(138521149440), BitBoard(139620515840),
    BitBoard(275955859456), BitBoard(277042298880), BitBoard(279241031680),
    BitBoard(515396075520), BitBoard(551911718912), BitBoard(554084597760),
    BitBoard(1030792151040), BitBoard(1103823437824), BitBoard(1108169195520),
    BitBoard(2061584302080), BitBoard(2207646875648), BitBoard(2216338391040),
    BitBoard(4123168604160)
];

/// Encodings of the top-most available slot. If these are vacant then the column
/// is an available choice for the next move
static TOPROW: [BitBoard; 7] = [
    BitBoard(34359738368), BitBoard(68719476736), BitBoard(137438953472),
    BitBoard(274877906944), BitBoard(549755813888), BitBoard(1099511627776),
    BitBoard(2199023255552)
];

/// Bitboard encodings of the first 42 powers of 2 for easy access and avoidance
/// of potentially expensive calculations at run-time
static POW2: [BitBoard; 42] = [
    BitBoard(1), BitBoard(2), BitBoard(4),
    BitBoard(8), BitBoard(16), BitBoard(32), BitBoard(64), BitBoard(128),
    BitBoard(256), BitBoard(512), BitBoard(1024), BitBoard(2048),
    BitBoard(4096), BitBoard(8192), BitBoard(16384), BitBoard(32768),
    BitBoard(65536), BitBoard(131072), BitBoard(262144), BitBoard(524288),
    BitBoard(1048576), BitBoard(2097152), BitBoard(4194304), BitBoard(8388608),
    BitBoard(16777216), BitBoard(33554432), BitBoard(67108864),
    BitBoard(134217728), BitBoard(268435456), BitBoard(536870912),
    BitBoard(1073741824), BitBoard(2147483648), BitBoard(4294967296),
    BitBoard(8589934592), BitBoard(17179869184), BitBoard(34359738368),
    BitBoard(68719476736), BitBoard(137438953472), BitBoard(274877906944),
    BitBoard(549755813888), BitBoard(1099511627776), BitBoard(2199023255552)
];

#[derive(Copy, Clone)]
/// The player type
enum Player {
    /// Player one
    One(PlayerKind),
    /// Player two
    Two(PlayerKind)
}

impl Builder for Player {
    fn new(int: usize, kind: PlayerKind) -> Option<Player> {
        match int {
            1 => Some(Player::One(kind)),
            2 => Some(Player::Two(kind)),
            _ => None
        }
    }
}

impl Turing for Player {
    fn turing_test(&self) -> PlayerKind {
        use Player::*;
        use PlayerKind::*;
        match self {
            One(Human) | Two(Human) => Human,
            One(Bot) | Two(Bot) => Bot
        }
    }
}

impl PartialEq for Player {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            // We don't really care about the player kind here since there can
            // only be one player 1 and so on in the game
            (Player::One(_), Player::One(_)) |
                (Player::Two(_), Player::Two(_)) => true,
            _ => false
        }
    }
}

impl Eq for Player {}

impl fmt::Display for Player {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Player::One(_) => write!(f, "1"),
            Player::Two(_) => write!(f, "2")
        }
    }
}

#[derive(Add, Sub, PartialEq, Eq, Copy, Clone, Debug, Display)]
/// The index struct. This is the internal representation of moves.
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

#[derive(Add, AddAssign, BitOr, PartialOrd, Ord, PartialEq, Eq,
         Copy, Clone, Debug, Display)]
/// The BitBoard struct. This is the memoery representation of the
/// connect4 game state for one of the players.
struct BitBoard(u64);

impl BitBoard {
    // TODO: rewrite with try_fold when the Try trait API is stabilized
    fn check_winner(self) -> bool {
        let mut c = false;
        for &x in WINCONFIGS.iter() {
            if x | self == self { c = true; break }
            else if x > self { break }
        };
        c
    }
}

impl fmt::Binary for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let BitBoard(u) = self;
        write!(f, "{:b}", u)
    }
}

#[derive(Clone)]
/// The ConnectFourState struct. This is the internal representation of the
/// game state during play.
struct ConnectFourState {
    /// The player that has the turn
    curr_player: Player,
    /// Player One
    player_one: Player,
    /// Player Two
    player_two: Player,
    /// The pieces of the current player
    curr_side: BitBoard,
    /// The pieces of the opposing player
    other_side: BitBoard
}

impl Game for ConnectFourState {
    type Move = Index;
    type Player = Player;

    fn new(players: Vec<Player>) -> Self {
        use Player::*;

        let (player_one, player_two) = match &players[..] {
            &[One(a), Two(b)] => (One(a), Two(b)),
            &[Two(b), One(a)] => (One(a), Two(b)),
            _ => panic!("Invalid input")   // inaccessible branch
        };

        ConnectFourState {
            curr_player: player_one,
            player_one,
            player_two,
            curr_side: BitBoard(0),
            other_side: BitBoard(0)
        }
    }

    fn available_moves(&self) -> Vec<Index> {
        let full_board = self.curr_side + self.other_side;
        let (_, moves) = TOPROW.iter()
            .fold((Index(0), vec![]), |(i, mut l), &elt| {
                // The user sees a 1-based indexing scheme consistent with
                // everyday expectations whereas internally we use 0-based
                // indexing
                if full_board | elt != full_board { l.push(i.inc()) };
                (i.inc(), l)
            });
        moves
    }

    fn current_player(&self) -> Player { self.curr_player }

    fn next_player(&self) -> Player {
        match self.curr_player {
            Player::One(_) => self.player_two,
            Player::Two(_) => self.player_one
        }
    }

    fn status(&self) -> Status {
        let finished = self.curr_side.check_winner() ||
            self.other_side.check_winner() ||
            // That number is the bit representation of a full "board"
            self.curr_side + self.other_side == BitBoard(4398046511103);
        if finished { Status::Finished }
        else { Status::Ongoing }
    }

    fn mv(&self, indx: &Index) -> Self {
        let curr_player = self.next_player();
        let mut other_side = self.curr_side;
        let curr_side = self.other_side;
        let full_board = self.curr_side + self.other_side;
        // TODO: rewrite with try_fold when the Try trait API is stabilized
        // The user provides a 1-based index but we need a 0-based index
        for entry in (0..6).map(|x| POW2[indx.dec().as_usize() + 7 * x]) {
            if entry | full_board != full_board {
                other_side += entry;
                break
            }
        };
        ConnectFourState { curr_player, curr_side, other_side, ..*self }
    }

    fn winner(&self) -> Option<Player> {
        if self.curr_side.check_winner() { Some(self.curr_player) }
        // the use of next_player here is safe since connect4 is a two-player game
        else if self.other_side.check_winner() { Some(self.next_player()) }
        else { None }
    }
}

impl Interactive<Index> for ConnectFourState {
    fn is_valid_move(&self, n: &Index) -> bool {
        // The user provides a 1-based index but we need a 0-based index
        self.available_moves().contains(&n.dec())
    }
}

impl terminal::Terminal for ConnectFourState {
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

impl fmt::Display for ConnectFourState {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        println!(" 1 2 3 4 5 6 7 ");
        let int_to_padded_str = |bitboard: BitBoard| {
            let bitstring = format!("{:b}", bitboard);
            format!("{:0>42}", bitstring)
        };
        let (side_one, side_two) = match self.curr_player {
            Player::One(_) => (self.curr_side, self.other_side),
            Player::Two(_) => (self.other_side, self.curr_side)
        };
        let player_one_repr = int_to_padded_str(side_one);
        let player_two_repr = int_to_padded_str(side_two);

        let symbol_conv = |(a, b)| match (a, b) {
            ('1', '0') => 'o',
            ('0', '1') => 'x',
            ('0', '0') => ' ',
            _ => panic!("Check your code!")
        };
        let red = Style::new().red().bold();
        let blue = Style::new().blue().bold();

        player_one_repr.chars()
            .zip(player_two_repr.chars())
            .map(symbol_conv)
            .collect::<String>()
            .graphemes(true)  // needed to keep the compiler from complaining
            .interleave(vec!["|"; 42])
            .chunks(14)
            .into_iter()
            .for_each(|l| {
                l.collect::<String>()
                    .chars()
                    .rev()
                    .for_each(|c| match c {
                        'o' => print!("{}", blue.apply_to('●')),
                        'x' => print!("{}", red.apply_to('●')),
                        _ => print!("{}", c)
                    });
                println!("|");
            });
        println!(" 1 2 3 4 5 6 7 ");

        Ok(())
    }
}

fn main() -> Result<(), terminal::Error> {
    ui::terminal::launch_game::<ConnectFourState, Index, Player>("Connect Four")
}
