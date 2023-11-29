use crate::compiler::{ByteCode, Closure};
use oneparse::position::Located;

pub trait MoveOptimize {
    fn optimize_move(&mut self);
}

impl MoveOptimize for Closure {
    fn optimize_move(&mut self) {
        let mut idx = 0;
        while let Some(pair) = self
            .code
            .get(idx)
            .cloned()
            .zip(self.code.get(idx + 1).cloned())
        {
            match pair {
                (
                    Located {
                        value:
                            ByteCode::Move {
                                dst: dst1,
                                src: src1,
                            },
                        pos: _,
                    },
                    Located {
                        value:
                            ByteCode::Move {
                                dst: dst2,
                                src: src2,
                            },
                        pos,
                    },
                ) if src2 == dst1.into() => {
                    self.code.remove(idx);
                    *self.code.get_mut(idx).unwrap() = Located::new(
                        ByteCode::Move {
                            dst: dst2,
                            src: src1,
                        },
                        pos,
                    );
                }
                (_, _) => {
                    idx += 2;
                }
            }
        }
    }
}
