pub trait Interactive<I> {
    fn is_valid_move(&self, mv: &I) -> bool;
}

#[derive(Copy, Clone, PartialEq, Eq)]
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

    use crate::mcts_core;
    use crate::mcts_core::{Debug, Game, Initialize, Status};

    use super::{PlayerKind, Turing, Builder};

    #[derive(Debug)]
    ///The Error type
    pub enum Error {
        ClearScreenError,
        ParseIntError,
    }

    /// A collection of functions that must be implemented for CLI interfaces
    pub trait Terminal<D> {
        /// Print the game-end screen.
        fn game_end_screen(&self, debug: &D) -> Result<(), Error>;
    }

    /// Keep or clear the buffer.
    enum Buffer { Keep, Clear }

    /// Acquire input from the player and convert it into an internal
    /// representation. This function parses the user input and asks for input
    /// again if the input was invalid.
    fn acquire_input<T, I, P>(game_state: &T) -> I
        where
            I: Initialize + Eq + Copy + str::FromStr,
            P: Copy + Eq + fmt::Display,
            T: Game<I, P> {
        loop {
            print!("\nEnter your move: ");
            io::stdout().flush().unwrap();  // ensures the msg shows up
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Err(_) => println!("Invalid input."),
                Ok(_) => match input.trim().parse::<I>() {
                    Err(_) => println!("Invalid input."),
                    Ok(m) => {
                        if game_state.available_moves()[..].contains(&m) {
                            break m
                        } else {
                            println!("Not a valid move.");
                        }
                    }
                }
            }
        }
    }

    /// Clean up, print what needs to be printed, and exit the game
    fn exit<T, I, P>(game_state: &T, debug: &Debug) -> Result<(), Error>
        where
            I: Initialize + Eq + Copy + str::FromStr,
            P: Copy + Eq + fmt::Display,
            T: Game<I, P> + Terminal<Debug> + crate::Interactive<I> + fmt::Display {
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

    /// Play against the MCTS AI.  ***TODO*** For games with more than two
    /// players, have the ability to choose which side(s) are played by humans
    /// and which are played by the AI.
    fn game_loop<T, I, P>(nplayouts: usize, players: Vec<P>,
                          debug: Debug) -> Result<(), Error>
        where
            I: Initialize + Eq + Copy + str::FromStr + fmt::Display,
            P: Turing + Copy + Eq + fmt::Display,
            T: Game<I, P> + Terminal<Debug> + crate::Interactive<I> +
                fmt::Display + Clone
    {
        let mut game_state: T = Game::new(players);
        println!("\n{}", game_state);

        let term = Term::stdout();
        let mut buffer = Buffer::Keep;
        loop {
            let current_player = game_state.current_player();
            match game_state.status() {
                Status::Finished => {
                    exit(&game_state, &debug)?;
                    break
                },
                Status::Ongoing => {
                    match (&buffer, &debug) {
                        (&Buffer::Clear, &Debug::Release) => {
                            match term.clear_screen() {
                                Ok(_) => Ok(()),
                                Err(_) => Err(Error::ClearScreenError)
                            }?;
                            println!("\n{}", game_state);
                        },
                        _ => ()
                    };
                    match current_player.turing_test() {
                        PlayerKind::Human => {
                            let n = acquire_input(&game_state);
                            game_state = game_state.mv(&n);
                            println!("\n{}", game_state);
                            // if the current player has another turn
                            if game_state.current_player() == current_player {
                                buffer = Buffer::Keep
                            } else {
                                buffer = Buffer::Clear
                            }
                        },
                        PlayerKind::Bot => {
                            let ai_move = mcts_core::most_favored_move(nplayouts,
                                                                       &game_state,
                                                                       &debug);
                            game_state = game_state.mv(&ai_move);
                            println!("\nPlayer {}'s move: {}\n", current_player, ai_move);
                            println!("{}", game_state);
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
    pub fn launch_game<T, I, P>(name: &str) -> Result<(), Error>
        where
            I: Initialize + Eq + Copy + fmt::Display + str::FromStr,
            P: Builder + Turing + Copy + Eq + fmt::Display,
            T: Game<I, P> + Terminal<Debug> + crate::Interactive<I> + Clone + fmt::Display {
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
        game_loop::<T, I, P>(nplayouts, players, debug)
    }
}
