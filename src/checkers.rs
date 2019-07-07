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
mod ui;

use mcts::{Debug, Initialize, Game, Status};
use ui::{Interactive, PlayerKind, Builder, Turing};
use ui::terminal;
