use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
pub enum Bytecode {
  Add,
  Subtract,
  Negate,
  BranchIfFalse(i32),
  Goto(i32),
  PushInt(i32),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
  pub bytecodes: Vec<Bytecode>,
}
