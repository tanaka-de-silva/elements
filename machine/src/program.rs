use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub enum NumericType {
  IntType,
  LongType,
  DoubleType
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
pub enum Bytecode {
  Add(NumericType),
  Subtract(NumericType),
  Multiply(NumericType),
  Divide(NumericType),
  Negate(NumericType),
  LessThan(NumericType),
  LessThanOrEquals(NumericType),
  Equals(NumericType),
  GreaterThanOrEquals(NumericType),
  GreaterThan(NumericType),
  NotEquals(NumericType),
  And,
  Or,
  StoreLocal(i32),
  GetLocal(i32),
  BranchIfFalse(i32),
  Goto(i32),
  PushInt(i32),
  PushLong(i64),
  PushDouble(f64),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
  pub bytecodes: Vec<Bytecode>,
}
