use core::iter::{self, repeat};

pub trait ParityMatrix {
    fn row(&self, m: usize) -> Vec<bool>;
}

#[derive(Debug, Clone)]
pub struct Reconstructor<Parity, const BLOCKSIZE: usize> {
    parity: Parity,
    matrix: Vec<Vec<bool>>,
    blocks: Vec<[u8; BLOCKSIZE]>,
}

impl<Parity: ParityMatrix, const BLOCKSIZE: usize> Reconstructor<Parity, BLOCKSIZE> {
    pub fn new(parity: Parity) -> Self {
        let n = parity.row(0).len();
        Reconstructor {
            parity,
            matrix: repeat(repeat(false).take(n).collect()).take(n).collect(),
            blocks: repeat([0;BLOCKSIZE]).take(n).collect(),
        }
    }

    // Returns true when we have sufficient data for reconstruction
    pub fn handle_block(&mut self, index: usize, mut data: [u8; BLOCKSIZE]) -> bool {
        let mut row = self.parity.row(index);
        debug_assert_eq!(row.len(), self.matrix.len());
        let mut working_head = row.len() - 1;
        loop {
            if row[working_head] && self.matrix[working_head][working_head] {
                // eliminate
                for (i, el) in data.iter_mut().enumerate() {
                    *el ^= self.blocks[working_head][i];
                }
                for (i, el) in row.iter_mut().enumerate() {
                    *el = *el != self.matrix[working_head][i];
                }
            } else if row [working_head] {
                // new row, store it
                self.matrix[working_head] = row;
                self.blocks[working_head] = data;
                break;
            }

            if working_head == 0 {
                break;
            } else {
                working_head -= 1;
            }
        }
        
        // Check whether we have sufficient data
        for i in 0..self.matrix.len() {
            if !self.matrix[i][i] {
                return false;
            }
        }
        true
    }

    pub fn output(mut self) -> impl Iterator<Item = [u8; BLOCKSIZE]> {
        let mut i = 0;
        iter::from_fn(move || {
            if i < self.blocks.len() {
                let mut output = self.blocks[i];
                for j in 0..i {
                    if self.matrix[i][j] {
                        for (k, el) in output.iter_mut().enumerate() {
                            *el ^= self.blocks[j][k];
                        }
                    }
                }
                self.blocks[i] = output;
                i += 1;
                Some(output)
            } else {
                None
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestParity<const N: usize>;

    impl<const N: usize> ParityMatrix for TestParity<N> {
        fn row(&self, m: usize) -> Vec<bool> {
            if m < N {
                let mut out: Vec<_> = repeat(false).take(N).collect();
                out[m] = true;
                out
            } else {
                let mut out = Vec::with_capacity(N);
                for i in 0..N {
                    out.push((m - N) & 1_usize.checked_shl(i as _).unwrap_or(0) != 0);
                }
                out
            }
        }
    }

    #[test]
    fn simple_reconstruction_test() {
        let mut rec = Reconstructor::new(TestParity::<4>);
        assert_eq!(rec.handle_block(0, [1]), false);
        assert_eq!(rec.handle_block(2, [3]), false);
        assert_eq!(rec.handle_block(9, [2]), false);
        assert_eq!(rec.handle_block(10, [1]), false);
        assert_eq!(rec.handle_block(14, [6]), true);

        let out: Vec<[u8;1]> = rec.output().collect();
        assert_eq!(out, [[1],[2],[3],[4]]);
    }

    #[test]
    fn nontrivial_relation() {
        let mut rec = Reconstructor::new(TestParity::<4>);
        assert_eq!(rec.handle_block(0, [1]), false);
        assert_eq!(rec.handle_block(10, [1]), false);
        assert_eq!(rec.handle_block(14, [6]), false);
        assert_eq!(rec.handle_block(16, [7]), false);
        assert_eq!(rec.handle_block(19, [4]), true);

        let out: Vec<[u8;1]> = rec.output().collect();
        assert_eq!(out, [[1], [2], [3], [4]]);
    }
}
