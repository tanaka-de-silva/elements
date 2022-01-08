use super::program::{Bytecode, NumericType};
use super::stack::Stack;
use super::values::VmValue;
use std::collections::HashMap;

pub fn evalute(bytecodes: &Vec<Bytecode>) -> Stack {
  let mut stack: Stack = Stack::new();
  let mut locals: HashMap<i32, VmValue> = HashMap::new();
  let mut program_counter = 0;
  loop {
    match bytecodes.get(program_counter as usize) {
      None => break,
      Some(Bytecode::Add(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs + rhs;
        stack.push_int(result);
      }
      Some(Bytecode::Add(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs + rhs;
        stack.push_long(result);
      }
      Some(Bytecode::Add(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs + rhs;
        stack.push_double(result);
      }
      Some(Bytecode::Subtract(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs - rhs;
        stack.push_int(result)
      }
      Some(Bytecode::Subtract(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs - rhs;
        stack.push_long(result);
      }
      Some(Bytecode::Subtract(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs - rhs;
        stack.push_double(result);
      }
      Some(Bytecode::Multiply(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs * rhs;
        stack.push_int(result)
      }
      Some(Bytecode::Multiply(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs * rhs;
        stack.push_long(result);
      }
      Some(Bytecode::Multiply(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs * rhs;
        stack.push_double(result);
      }
      Some(Bytecode::Divide(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs / rhs;
        stack.push_int(result)
      }
      Some(Bytecode::Divide(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs / rhs;
        stack.push_long(result);
      }
      Some(Bytecode::Divide(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs / rhs;
        stack.push_double(result);
      }
      Some(Bytecode::Negate(NumericType::IntType)) => {
        let value = stack.pop_int();
        let result = -value;
        stack.push_int(result)
      }
      Some(Bytecode::Negate(NumericType::LongType)) => {
        let value = stack.pop_long();
        let result = -value;
        stack.push_long(result)
      }
      Some(Bytecode::Negate(NumericType::DoubleType)) => {
        let value = stack.pop_double();
        let result = -value;
        stack.push_double(result)
      }
      Some(Bytecode::Equals(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs == rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::Equals(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs == rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::Equals(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs == rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::NotEquals(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs != rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::NotEquals(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs != rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::NotEquals(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs != rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::LessThan(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs < rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::LessThan(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs < rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::LessThan(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs < rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::LessThanOrEquals(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs <= rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::LessThanOrEquals(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs <= rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::LessThanOrEquals(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs <= rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::GreaterThan(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs > rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::GreaterThan(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs > rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::GreaterThan(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs > rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::GreaterThanOrEquals(NumericType::IntType)) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs >= rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::GreaterThanOrEquals(NumericType::LongType)) => {
        let rhs = stack.pop_long();
        let lhs = stack.pop_long();
        let result = lhs >= rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::GreaterThanOrEquals(NumericType::DoubleType)) => {
        let rhs = stack.pop_double();
        let lhs = stack.pop_double();
        let result = lhs >= rhs;
        stack.push_bool(result);
      }
      Some(Bytecode::And) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs != 0 && rhs != 0;
        stack.push_bool(result);
      }
      Some(Bytecode::Or) => {
        let rhs = stack.pop_int();
        let lhs = stack.pop_int();
        let result = lhs != 0 || rhs != 0;
        stack.push_bool(result);
      }
      Some(Bytecode::Goto(i)) => {
        program_counter += i;
      }
      Some(Bytecode::GetLocalInt(i)) => {
        match locals.get(i).unwrap() {
          VmValue::IntValue(v) => stack.push_int(*v),
          _ => unreachable!(),
        };
      }
      Some(Bytecode::GetLocalLong(i)) => {
        match locals.get(i).unwrap() {
          VmValue::LongValue(v) => stack.push_long(*v),
          _ => unreachable!(),
        };
      }
      Some(Bytecode::GetLocalDouble(i)) => {
        match locals.get(i).unwrap() {
          VmValue::DoubleValue(v) => stack.push_double(*v),
          _ => unreachable!(),
        };
      }
      Some(Bytecode::StoreLocalInt(i)) => {
        let value = stack.pop_int();
        locals.insert(*i, VmValue::IntValue(value));
      }
      Some(Bytecode::StoreLocalLong(i)) => {
        let value = stack.pop_long();
        locals.insert(*i, VmValue::LongValue(value));
      }
      Some(Bytecode::StoreLocalDouble(i)) => {
        let value = stack.pop_double();
        locals.insert(*i, VmValue::DoubleValue(value));
      }
      Some(Bytecode::BranchIfFalse(i)) => {
        let value = stack.pop_bool();
        if !value {
          program_counter += i;
        }
      }
      Some(Bytecode::PushInt(x)) => stack.push_int(*x),
      Some(Bytecode::PushLong(x)) => stack.push_long(*x),
      Some(Bytecode::PushDouble(x)) => stack.push_double(*x),
    };
    program_counter += 1;
  }
  stack
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn can_add_two_numbers() {
    let bytecodes = vec![
      Bytecode::PushInt(1),
      Bytecode::PushInt(2),
      Bytecode::Add(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 3);
  }

  #[test]
  fn can_subtract_a_number_from_another() {
    let bytecodes = vec![
      Bytecode::PushInt(2),
      Bytecode::PushInt(1),
      Bytecode::Subtract(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 1);
  }

  #[test]
  fn can_multiply_a_number_by_another() {
    let bytecodes = vec![
      Bytecode::PushInt(2),
      Bytecode::PushInt(3),
      Bytecode::Multiply(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 6);
  }

  #[test]
  fn can_divide_a_number_by_another() {
    let bytecodes = vec![
      Bytecode::PushInt(4),
      Bytecode::PushInt(2),
      Bytecode::Divide(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 2);
  }

  #[test]
  fn can_find_the_conjunction_of_two_booleans() {
    let bytecodes = vec![Bytecode::PushInt(1), Bytecode::PushInt(1), Bytecode::And];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 1);
  }

  #[test]
  fn can_find_the_disjunction_of_two_booleans() {
    let bytecodes = vec![Bytecode::PushInt(1), Bytecode::PushInt(0), Bytecode::Or];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 1);
  }

  #[test]
  fn can_negate_a_number() {
    let bytecodes = vec![Bytecode::PushInt(1), Bytecode::Negate(NumericType::IntType)];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, -1);
  }

  #[test]
  fn can_check_equality() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(0),
        Bytecode::PushInt(1),
        Bytecode::Equals(NumericType::IntType)
      ])
      .pop_int(),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(1),
        Bytecode::PushInt(1),
        Bytecode::Equals(NumericType::IntType)
      ])
      .pop_int(),
      1
    );
  }

  #[test]
  fn can_check_inequality() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(3),
        Bytecode::NotEquals(NumericType::IntType),
      ])
      .pop_int(),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(2),
        Bytecode::NotEquals(NumericType::IntType),
      ])
      .pop_int(),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_less_than_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(1),
        Bytecode::LessThan(NumericType::IntType),
      ])
      .pop_int(),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(1),
        Bytecode::PushInt(2),
        Bytecode::LessThan(NumericType::IntType),
      ])
      .pop_int(),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_less_than_or_equal_to_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(1),
        Bytecode::LessThanOrEquals(NumericType::IntType),
      ])
      .pop_int(),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(1),
        Bytecode::PushInt(2),
        Bytecode::LessThanOrEquals(NumericType::IntType),
      ])
      .pop_int(),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_greater_than_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(3),
        Bytecode::GreaterThan(NumericType::IntType),
      ])
      .pop_int(),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(2),
        Bytecode::GreaterThan(NumericType::IntType),
      ])
      .pop_int(),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_greater_or_than_equal_to_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(3),
        Bytecode::GreaterThanOrEquals(NumericType::IntType),
      ])
      .pop_int(),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(2),
        Bytecode::GreaterThanOrEquals(NumericType::IntType),
      ])
      .pop_int(),
      1
    );
  }

  #[test]
  fn can_goto_an_instruction_offset() {
    let bytecodes = vec![
      Bytecode::Goto(2),
      Bytecode::PushInt(1),
      Bytecode::Negate(NumericType::IntType),
      Bytecode::PushInt(2),
      Bytecode::PushInt(2),
      Bytecode::Add(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 4);
  }

  #[test]
  fn can_branch_to_an_instruction_offset() {
    let bytecodes = vec![
      Bytecode::PushInt(0),
      Bytecode::BranchIfFalse(2),
      Bytecode::PushInt(1),
      Bytecode::Negate(NumericType::IntType),
      Bytecode::PushInt(2),
      Bytecode::PushInt(2),
      Bytecode::Add(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 4);
  }

  #[test]
  fn continues_when_branch_condition_is_not_satisfied() {
    let bytecodes = vec![
      Bytecode::PushInt(7),
      Bytecode::PushInt(1),
      Bytecode::BranchIfFalse(2),
      Bytecode::PushInt(1),
      Bytecode::Negate(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, -1);
  }

  #[test]
  fn can_store_and_get_local_values() {
    let bytecodes = vec![
      Bytecode::PushInt(1),
      Bytecode::StoreLocalInt(0),
      Bytecode::PushInt(3),
      Bytecode::StoreLocalInt(1),
      Bytecode::GetLocalInt(0),
      Bytecode::GetLocalInt(0),
      Bytecode::GetLocalInt(1),
      Bytecode::Add(NumericType::IntType),
      Bytecode::Add(NumericType::IntType),
    ];
    let result = evalute(&bytecodes).pop_int();
    assert_eq!(result, 5);
  }
}
