use std::ops::AddAssign;
use std::cmp::Ordering;
use std::fmt;

use super::Outcome;

#[derive(Clone, Debug)]
pub struct Score {
    q: f32,
    u: f32,
    win: usize,
    loss: usize,
    draw: usize,
    total: usize,
    ptot: usize
}

impl Score {
    pub fn new(ptot: usize) -> Self {
        Score { q: 1., u: 1., win: 0, loss: 0, draw: 0, total: 0, ptot }
    }

    pub fn total(&self) -> usize { self.total }

    pub fn update(&mut self, outcome: Outcome) {
        let delta = match outcome {
            Outcome::Win => Score {
                q: 1., u: 0., win: 1, loss: 0, draw: 0, total: 1, ptot: 0
            },
            Outcome::Loss => Score {
                q: 0., u: 0., win: 0, loss: 1, draw: 0, total: 1, ptot: 0
            },
            Outcome::Draw => Score {
                q: 0.5, u: 0., win: 0, loss: 0, draw: 1, total: 1, ptot: 0
            }
        };
        *self += delta;
    }

    /// Tests float congruence
    pub fn feq(&self, rhs: &Self) -> bool {
        (self.q - rhs.q).abs() < 1e-5 && (self.u - rhs.u).abs() < 1e-5
    }

    fn score(q: f32, a: &Score) -> f32 { (q + 1.) * a.u.sqrt() }

    pub fn score_self(&self) -> f32 { Score::score(self.q, self) }

    pub fn score_other(&self) -> f32 { Score::score(1. - self.q, self) }

    pub fn gt_other(&self, rhs: &Score) -> bool {
        if self.feq(rhs) { false }
        else { self.score_other() > rhs.score_other() }
    }

    pub fn gt_self(&self, rhs: &Score) -> bool {
        if self.feq(rhs) { false }
        else { self.score_self() > rhs.score_self() }
    }
}

impl AddAssign for Score {
    fn add_assign(&mut self, rhs: Score) {
        let u = 1. / (self.total + rhs.total) as f32;
        let win = self.win + rhs.win;
        let total = self.total + rhs.total;
        let ptot = self.ptot + 1;
        let loss = self.loss + rhs.loss;
        let draw = self.draw + rhs.draw;
        let q = 0.5 * (2 * win + draw) as f32 / total as f32;
        *self = Score { q, u, win, loss, draw, total, ptot };
    }
}

impl PartialEq for Score {
    fn eq(&self, rhs: &Self) -> bool { self.total == rhs.total }
}

impl Eq for Score {}

impl Ord for Score {
    fn cmp(&self, rhs: &Self) -> Ordering { self.total.cmp(&rhs.total) }
}

impl PartialOrd for Score {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

impl fmt::Display for Score {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
           "(q: {:0<12}, u: {:0<12}, win: {}, draw: {}, loss: {}, total: {}, \
            ptot: {})\tscore = {}",
            self.q, self.u, self.win, self.draw, self.loss, self.total,
            self.ptot, self.score_self()
        )
    }
}
