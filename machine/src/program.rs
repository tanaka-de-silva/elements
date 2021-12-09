use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
pub enum Bytecode {
  Add,
  PushInt(i32),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
  pub bytecodes: Vec<Bytecode>,
}
