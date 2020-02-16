// TODO: have the option to clear the guard when the repeated moves are not
// consecutive
use std::collections::HashMap;

/// Implementation of n-fold repetition rules
#[derive(Clone)]
pub struct RepetitionGuard<T> where T: std::hash::Hash + Eq {
    max: u8,
    counters: HashMap<T, u8>
}

pub enum Repetition {
    Acceptable,
    Excessive
}

impl<T> RepetitionGuard<T> where T: std::hash::Hash + Eq {
    pub fn new(max: u8) -> Self {
        RepetitionGuard { max, counters: HashMap::new() }
    }

    pub fn inc(&mut self, key: T) -> Repetition {
        let count = match self.counters.get(&key) {
            Some(&c) => c + 1,
            None => 1
        };
        if count > self.max {
            Repetition::Excessive
        } else {
            self.counters.insert(key, count);
            Repetition::Acceptable
        }
    }

    pub fn query(&self, key: &T) -> Repetition {
        let count = match self.counters.get(key) {
            Some(&c) => c,
            None => 0
        };
        if count > self.max {
            Repetition::Excessive
        } else {
            Repetition::Acceptable
        }
    }
}

