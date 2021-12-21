use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
pub enum Bytecode {
  Add,
  Subtract,
  Multiply,
  Divide,
  Negate,
  LessThan,
  LessThanOrEquals,
  Equals,
  GreaterThanOrEquals,
  GreaterThan,
  NotEquals,
  StoreLocal(i32),
  GetLocal(i32),
  BranchIfFalse(i32),
  Goto(i32),
  PushInt(i32),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
  pub bytecodes: Vec<Bytecode>,
}
