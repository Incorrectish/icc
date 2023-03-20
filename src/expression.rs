use crate::assembly::Asm;

#[derive(PartialEq, Eq, Debug)]
pub enum ExpType {
    Type(Asm, &'static str),
    Constant(Constant),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Constant {
    Int(i32),
}

