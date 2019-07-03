pub trait Interactive<I> {
    fn is_valid_move(&self, mv: &I) -> bool;
}

pub mod terminal {
    use std::{io, fmt, str};
    use std::io::Write;

    use console::Term;
    use clap::{Arg, App};

    use crate::mcts_core;
    use crate::mcts_core::{Debug, Game, Next, Initialize, Status};

    #[derive(Debug)]
    pub enum Error {
        ClearScreenError,
        ParseIntError,
    }

    pub trait Terminal<D> {
        fn game_end_screen(&self, debug: &D) -> Result<(), Error>;
    }

    pub trait FromInt {
        fn from_int(int: &usize) -> Result<Box<Self>, Error>;
    }

    enum Buffer { Keep, Clear }

    fn acquire_input<T, I, P>(game_state: &T) -> I
        where
            I: Initialize + Eq + Copy + str::FromStr,
            P: Next + Copy + Eq + fmt::Display,
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

    /// TODO: Rename it to something better
    fn exit<T, I, P>(game_state: &T, debug: &Debug) -> Result<(), Error>
        where
            I: Initialize + Eq + Copy + str::FromStr,
            P: Next + Copy + Eq + fmt::Display,
            T: Game<I, P> + Terminal<Debug> + crate::Interactive<I> + fmt::Display {
        match game_state.winner() {
            None => {
                println!("The game is a draw.");
                Ok(())
            },
            Some(p) => {
                game_state.game_end_screen(&debug)?;
                println!("Player {} wins.", p);
                Ok(())
            }
        }
    }

    fn two_player_game<T, I, P>(debug: Debug) -> Result<(), Error>
        where
            I: Initialize + Eq + Copy + str::FromStr,
            P: Next + Copy + Eq + fmt::Display,
            T: Game<I, P> + Terminal<Debug> + crate::Interactive<I> + fmt::Display {
        let mut game_state: T = Game::init();
        let term = Term::stdout();
        loop {
            match game_state.status() {
                Status::Finished => {
                    exit(&game_state, &debug)?;
                    break
                },
                Status::Ongoing => {
                    println!("Current player: {}\n", game_state.curr_player());
                    println!("{}", game_state);
                    let n = acquire_input(&game_state);
                    match term.clear_screen() {
                        Ok(_) => Ok(()),
                        Err(_) => Err(Error::ClearScreenError)
                    }?;
                    game_state = game_state.mv(&n);
                }
            }
        };
        Ok(())
    }

    fn play_vs_ai<T, I, P>(nplayouts: usize, human_side: P, debug: Debug)
                           -> Result<(), Error>
        where
            I: Initialize + Eq + Copy + str::FromStr + fmt::Display,
            P: Next + Copy + Eq + fmt::Display,
            T: Game<I, P> + Terminal<Debug> + crate::Interactive<I> +
                fmt::Display + Clone
    {
        let mut game_state: T = Game::init();
        println!("\n{}", game_state);

        let term = Term::stdout();
        let mut buffer = Buffer::Keep;
        loop {
            let curr_side = game_state.curr_player();
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
                    if human_side == curr_side {
                        let n = acquire_input(&game_state);
                        game_state = game_state.mv(&n);
                        println!("\n{}", game_state);
                        if game_state.curr_player() == curr_side {
                            buffer = Buffer::Keep
                        } else {
                            buffer = Buffer::Clear
                        }
                    } else {
                        let ai_move = mcts_core::most_favored_move(nplayouts,
                                                                   &game_state,
                                                                   &debug);
                        game_state = game_state.mv(&ai_move);
                        println!("\nCOMPUTER MOVE: {}\n", ai_move);
                        println!("{}", game_state);
                        buffer = Buffer::Keep;
                    }
                }
            }
        };
        Ok(())
    }

    pub fn launch_game<T, I, P>(name: &str) -> Result<(), Error>
        where
            I: Initialize + Eq + Copy + fmt::Display + str::FromStr,
            P: Next + Copy + Eq + fmt::Display + FromInt,
            T: Game<I, P> + Terminal<Debug> + crate::Interactive<I> + Clone + fmt::Display {
        let args = App::new(name)
            .version(crate_version!())
            .author(crate_authors!())
            .about("A curation of board games with an optional Monte-Carlo \
                    based AI")
            .arg(Arg::with_name("ai")
                 .short("a")
                 .long("ai")
                 .help("Play against the computer."))
            .arg(Arg::with_name("side")
                 .short("s")
                 .long("side")
                 .takes_value(true)
                 .help("In a multiplayer game, the side of the human player. \
                        If the human player is the first player, enter 1, \
                        and so on."))
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

        let ai = args.is_present("ai");
        let n = args.value_of("nplayouts").unwrap_or("3000");
        let nplayouts = parse_int(n)?;
        let debug = if args.is_present("debug") { Debug::Debug }
                    else { Debug::Release };
        if ai {
            let h = parse_int(args.value_of("side").unwrap_or("1"))?;
            let side = match P::from_int(&h) {
                Ok(u) => Ok(*u),
                Err(_) => {
                    println!("Invalid argument. See help.");
                    Err(Error::ParseIntError)
                }
            }?;
            clear_screen()?;
            play_vs_ai::<T, I, P>(nplayouts, side, debug)
        } else {
            clear_screen()?;
            two_player_game::<T, I, P>(debug)
        }
    }
}
