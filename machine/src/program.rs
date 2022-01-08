use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub enum NumericType {
  IntType,
  LongType,
  DoubleType
}

type LocalVarIndex = i32;

type PCOffset = i32;

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
  StoreLocalInt(LocalVarIndex),
  StoreLocalLong(LocalVarIndex),
  StoreLocalDouble(LocalVarIndex),
  GetLocalInt(LocalVarIndex),
  GetLocalLong(LocalVarIndex),
  GetLocalDouble(LocalVarIndex),
  BranchIfFalse(PCOffset),
  Goto(PCOffset),
  PushInt(i32),
  PushLong(i64),
  PushDouble(f64),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
  pub bytecodes: Vec<Bytecode>,
}
