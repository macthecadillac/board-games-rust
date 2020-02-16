pub trait Interactive<M> {
    // FIXME: Since move types will always have to be copyable, this function
    // should require the object itself instead of a reference to it
    fn is_valid_move(&self, mv: &M) -> bool;
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
/// The PlayerKind type
pub enum PlayerKind {
    /// Human player
    Human,
    /// AI player
    Bot
}

pub trait Builder {
    fn new(int: usize, kind: PlayerKind) -> Option<Self>
        where Self: Sized;
}

pub trait Turing {
    fn turing_test(&self) -> PlayerKind;
}

/// Traits and functions that helps build a CLI interface
pub mod terminal {
    use std::{io, fmt, str};
    use std::io::Write;

    use console::Term;
    use clap::{Arg, App};

    use crate::mcts;
    use crate::mcts::{Debug, Game, Status};
    use crate::repetition_guard::{Repetition, RepetitionGuard};

    use super::{PlayerKind, Turing, Builder};

    #[derive(Debug)]
    ///The Error type
    pub enum Error {
        ClearScreenError,
        ParseIntError,
    }

    /// A collection of functions that must be implemented for CLI interfaces
    pub trait Terminal {
        type Debug;

        /// Print the game-end screen.
        fn game_end_screen(&self, debug: &Self::Debug) -> Result<(), Error>;
    }

    /// Keep or clear the buffer.
    enum Buffer { Keep, Clear }

    /// Acquire input from the player and convert it into an internal
    /// representation. This function parses the user input and asks for input
    /// again if the input was invalid.
    fn acquire_input<T, M, P>(game_state: &T) -> M
        where
            M: Default + Eq + Copy + str::FromStr + std::fmt::Display,
            P: Copy + Eq + fmt::Display,
            T: Game<Move=M, Player=P> {
        let available_moves: Vec<_> = game_state.available_moves()
            .collect();
        loop {
            for (i, chunk) in available_moves[..].chunks(3).enumerate() {
                for (j, mv) in chunk.iter().enumerate() {
                    print!("{}. {}   ", 3 * i + j, mv);
                }
                println!()
            }
            print!("\nEnter your move: ");
            io::stdout().flush().unwrap();  // ensures the msg shows up
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Err(_) => println!("Invalid input."),
                Ok(_) => match input.trim().parse::<usize>() {
                    Err(_) => println!("Invalid input."),
                    Ok(m) => {
                        match available_moves.get(m) {
                            Some(&mv) => break mv,
                            None => println!("Not a valid move.")
                        }
                    }
                }
            }
        }
    }

    /// Clean up, print what needs to be printed, and exit the game
    fn exit<T, M, P>(game_state: &T, debug: &Debug) -> Result<(), Error>
        where
            M: Default + Eq + Copy + str::FromStr,
            P: Copy + Eq + fmt::Display,
            T: Game<Move=M, Player=P> + Terminal<Debug=Debug> +
                crate::Interactive<M> + fmt::Display {
        match game_state.winner() {
            None => {
                println!("This game is a draw.");
                Ok(())
            },
            Some(p) => {
                game_state.game_end_screen(&debug)?;
                println!("Player {} wins.", p);
                Ok(())
            }
        }
    }

    // FIXME: something broke such that the last two moves are no longer shown
    /// Play against the MCTS AI.  ***TODO*** For games with more than two
    /// players, have the ability to choose which side(s) are played by humans
    /// and which are played by the AI.
    fn game_loop<T, M, P>(nplayouts: usize, players: Vec<P>, nfold: Option<u8>,
                          debug: Debug) -> Result<(), Error>
        where
            M: Default + Eq + Copy + str::FromStr + fmt::Display,
            P: Turing + Copy + Eq + fmt::Display,
            T: Game<Move=M, Player=P> + Terminal<Debug=Debug> +
                crate::Interactive<M> + fmt::Display + Clone +
                Eq + std::hash::Hash
    {
        let mut game_state: T = Game::new(players);
        println!("\n{}", game_state);

        let term = Term::stdout();
        let mut buffer = Buffer::Keep;
        let mut nfold_guard = match nfold {
            Some(n) => Some(RepetitionGuard::new(n)),
            None => None
        };
        let mut automatic_draw = false;
        loop {
            if automatic_draw {
                println!("Automatic draw due to the {}-fold repetition rule",
                         nfold.unwrap());
                break
            }
            let current_player = game_state.current_player();
            match game_state.status() {
                Status::Finished => {
                    exit(&game_state, &debug)?;
                    break
                },
                Status::Ongoing => {
                    if let (&Buffer::Clear, &Debug::Release) = (&buffer, &debug) {
                        match term.clear_screen() {
                            Ok(_) => Ok(()),
                            Err(_) => Err(Error::ClearScreenError)
                        }?;
                        println!("\n{}", game_state);
                    };
                    match current_player.turing_test() {
                        PlayerKind::Human => {
                            let n = acquire_input(&game_state);
                            game_state = game_state.mv(n);
                            if let (Some(g), Some(n)) = (&mut nfold_guard, nfold) {
                                match g.inc(game_state.clone()) {
                                    Repetition::Acceptable => println!(
                                        "Repeated move. Repeating over {} times will \
                                        result in an automatic draw.", n
                                    ),
                                    Repetition::Excessive => {
                                        automatic_draw = true;
                                    }
                                }
                            };
                            println!("\n{}", game_state);
                            // if the current player has another turn
                            if game_state.current_player() == current_player {
                                buffer = Buffer::Keep
                            } else {
                                buffer = Buffer::Clear
                            }
                        },
                        PlayerKind::Bot => {
                            let ai_move = mcts::most_favored_move(nplayouts,
                                                                  &game_state,
                                                                  &nfold_guard,
                                                                  &debug);
                            game_state = game_state.mv(ai_move);
                            println!("\nPlayer {}'s move: {}\n", current_player,
                                     ai_move);
                            println!("{}", game_state);
                            if let Some(g) = &mut nfold_guard {
                                match g.inc(game_state.clone()) {
                                    Repetition::Acceptable => (),
                                    Repetition::Excessive => {
                                        automatic_draw = true;
                                    }
                                }
                            };
                            buffer = Buffer::Keep;
                        }
                    }
                }
            }
        };
        Ok(())
    }

    /// The entry point into a terminal-based game. See the other files in the
    /// repository for example usage.
    pub fn launch_game<T, M, P>(name: &str, nfold: Option<u8>) -> Result<(), Error>
        where
            M: Default + Eq + Copy + fmt::Display + str::FromStr,
            P: Builder + Turing + Copy + Eq + fmt::Display,
            T: Game<Move=M, Player=P> + Terminal<Debug=Debug> +
                crate::Interactive<M> + Clone + fmt::Display +
                Eq + std::hash::Hash {
        let args = App::new(name)
            .author(crate_authors!())
            .about("A board game with an optional Monte-Carlo based AI")
            .arg(Arg::with_name("nplayouts")
                 .short("n")
                 .long("nplayouts")
                 .takes_value(true)
                 .help("The number of playouts to be performed with the \
                        Monte Carlo AI. The higher the number, the stronger \
                        the game play. The default is 3000."))
            .arg(Arg::with_name("debug")
                 .short("d")
                 .long("debug")
                 .help("Turn on debug mode"))
            .get_matches();

        let parse_int = |n: &str| match n.parse::<usize>() {
            Ok(u) => Ok(u),
            Err(_) => {
                println!("Invalid argument. See help.");
                Err(Error::ParseIntError)
            }
        };

        let clear_screen = || {
            let term = Term::stdout();
            match term.clear_screen() {
                Ok(()) => Ok(()),
                Err(_) => Err(Error::ClearScreenError)
            }
        };

        clear_screen()?;
        println!("\nGame setup:");
        let players: Vec<P> = (1..).map(|n| {
                // the concrete PlayerKind here is unimportant: we only care
                // about whether the iterator should end
                let f: Option<P> = Builder::new(n, PlayerKind::Human);
                match f {
                    None => None,
                    Some(_) => loop {
                        print!("    Player {}  (1. Human, 2. Bot)?: ", n);
                        io::stdout().flush().unwrap();  // ensures the msg shows up
                        let mut input = String::new();
                        match io::stdin().read_line(&mut input) {
                            Err(_) => println!("Invalid input."),
                            Ok(_) => match input.trim() {
                                "1" => break Builder::new(n, PlayerKind::Human),
                                "2" => break Builder::new(n, PlayerKind::Bot),
                                _ => println!("Invalid input.")
                            }
                        }
                    }
                }
            })
            .take_while(|&x| x.is_some())
            .map(|x| x.unwrap())
            .collect();

        let n = args.value_of("nplayouts").unwrap_or("3000");
        let nplayouts = parse_int(n)?;
        let debug = if args.is_present("debug") { Debug::Debug }
                    else { Debug::Release };
        clear_screen()?;
        game_loop::<T, M, P>(nplayouts, players, nfold, debug)
    }
}
