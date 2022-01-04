use super::program::Bytecode;
use super::stack::Stack;
use super::values::VmValue;
use std::collections::HashMap;

pub fn evalute(bytecodes: &Vec<Bytecode>) -> VmValue {
  let mut stack: Stack = Stack::new();
  let mut locals: HashMap<i32, VmValue> = HashMap::new();
  let mut program_counter = 0;
  loop {
    match bytecodes.get(program_counter as usize) {
      None => return stack.top(),
      Some(Bytecode::Add) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs + rhs;
        stack.push(result);
      }
      Some(Bytecode::Subtract) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs - rhs;
        stack.push(result)
      }
      Some(Bytecode::Multiply) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs * rhs;
        stack.push(result)
      }
      Some(Bytecode::Divide) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs / rhs;
        stack.push(result)
      }
      Some(Bytecode::Negate) => {
        let value = stack.pop().unwrap();
        let result = -value;
        stack.push(result)
      }
      Some(Bytecode::Equals) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs == rhs;
        stack.push(result as i32);
      }
      Some(Bytecode::NotEquals) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs != rhs;
        stack.push(result as i32);
      }
      Some(Bytecode::LessThan) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs < rhs;
        stack.push(result as i32);
      }
      Some(Bytecode::LessThanOrEquals) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs <= rhs;
        stack.push(result as i32);
      }
      Some(Bytecode::GreaterThan) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs > rhs;
        stack.push(result as i32);
      }
      Some(Bytecode::GreaterThanOrEquals) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs >= rhs;
        stack.push(result as i32);
      }
      Some(Bytecode::And) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs != 0 && rhs != 0;
        stack.push(result as i32);
      }
      Some(Bytecode::Or) => {
        let rhs = stack.pop().unwrap();
        let lhs = stack.pop().unwrap();
        let result = lhs != 0 || rhs != 0;
        stack.push(result as i32);
      }
      Some(Bytecode::Goto(i)) => {
        program_counter += i;
      }
      Some(Bytecode::GetLocal(i)) => {
        let value = locals.get(i).unwrap();
        stack.push(*value)
      }
      Some(Bytecode::StoreLocal(i)) => {
        let value = stack.pop().unwrap();
        locals.insert(*i, value);
      }
      Some(Bytecode::BranchIfFalse(i)) => match stack.pop() {
        Some(x) => {
          if x == 0 {
            program_counter += i;
          }
        }
        _ => unreachable!(),
      },
      Some(Bytecode::PushInt(x)) => stack.push(*x),
    };
    program_counter += 1;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn can_add_two_numbers() {
    let bytecodes = vec![Bytecode::PushInt(1), Bytecode::PushInt(2), Bytecode::Add];
    let result = evalute(&bytecodes);
    assert_eq!(result, 3);
  }

  #[test]
  fn can_subtract_a_number_from_another() {
    let bytecodes = vec![
      Bytecode::PushInt(2),
      Bytecode::PushInt(1),
      Bytecode::Subtract,
    ];
    let result = evalute(&bytecodes);
    assert_eq!(result, 1);
  }

  #[test]
  fn can_multiply_a_number_by_another() {
    let bytecodes = vec![
      Bytecode::PushInt(2),
      Bytecode::PushInt(3),
      Bytecode::Multiply,
    ];
    let result = evalute(&bytecodes);
    assert_eq!(result, 6);
  }

  #[test]
  fn can_divide_a_number_by_another() {
    let bytecodes = vec![Bytecode::PushInt(4), Bytecode::PushInt(2), Bytecode::Divide];
    let result = evalute(&bytecodes);
    assert_eq!(result, 2);
  }

  #[test]
  fn can_find_the_conjunction_of_two_booleans() {
    let bytecodes = vec![Bytecode::PushInt(1), Bytecode::PushInt(1), Bytecode::And];
    let result = evalute(&bytecodes);
    assert_eq!(result, 1);
  }

  #[test]
  fn can_find_the_disjunction_of_two_booleans() {
    let bytecodes = vec![Bytecode::PushInt(1), Bytecode::PushInt(0), Bytecode::Or];
    let result = evalute(&bytecodes);
    assert_eq!(result, 1);
  }

  #[test]
  fn can_negate_a_number() {
    let bytecodes = vec![Bytecode::PushInt(1), Bytecode::Negate];
    let result = evalute(&bytecodes);
    assert_eq!(result, -1);
  }

  #[test]
  fn can_check_equality() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(0),
        Bytecode::PushInt(1),
        Bytecode::Equals
      ]),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(1),
        Bytecode::PushInt(1),
        Bytecode::Equals
      ]),
      1
    );
  }

  #[test]
  fn can_check_inequality() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(3),
        Bytecode::NotEquals,
      ]),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(2),
        Bytecode::NotEquals,
      ]),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_less_than_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(1),
        Bytecode::LessThan,
      ]),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(1),
        Bytecode::PushInt(2),
        Bytecode::LessThan,
      ]),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_less_than_or_equal_to_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(1),
        Bytecode::LessThanOrEquals,
      ]),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(1),
        Bytecode::PushInt(2),
        Bytecode::LessThanOrEquals,
      ]),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_greater_than_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(3),
        Bytecode::GreaterThan,
      ]),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(2),
        Bytecode::GreaterThan,
      ]),
      1
    );
  }

  #[test]
  fn can_check_if_a_value_is_greater_or_than_equal_to_another() {
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(2),
        Bytecode::PushInt(3),
        Bytecode::GreaterThanOrEquals,
      ]),
      0
    );
    assert_eq!(
      evalute(&vec![
        Bytecode::PushInt(3),
        Bytecode::PushInt(2),
        Bytecode::GreaterThanOrEquals,
      ]),
      1
    );
  }

  #[test]
  fn can_goto_an_instruction_offset() {
    let bytecodes = vec![
      Bytecode::Goto(2),
      Bytecode::PushInt(1),
      Bytecode::Negate,
      Bytecode::PushInt(2),
      Bytecode::PushInt(2),
      Bytecode::Add,
    ];
    let result = evalute(&bytecodes);
    assert_eq!(result, 4);
  }

  #[test]
  fn can_branch_to_an_instruction_offset() {
    let bytecodes = vec![
      Bytecode::PushInt(0),
      Bytecode::BranchIfFalse(2),
      Bytecode::PushInt(1),
      Bytecode::Negate,
      Bytecode::PushInt(2),
      Bytecode::PushInt(2),
      Bytecode::Add,
    ];
    let result = evalute(&bytecodes);
    assert_eq!(result, 4);
  }

  #[test]
  fn continues_when_branch_condition_is_not_satisfied() {
    let bytecodes = vec![
      Bytecode::PushInt(7),
      Bytecode::PushInt(1),
      Bytecode::BranchIfFalse(2),
      Bytecode::PushInt(1),
      Bytecode::Negate,
    ];
    let result = evalute(&bytecodes);
    assert_eq!(result, -1);
  }

  #[test]
  fn can_store_and_get_local_values() {
    let bytecodes = vec![
      Bytecode::PushInt(1),
      Bytecode::StoreLocal(0),
      Bytecode::PushInt(3),
      Bytecode::StoreLocal(1),
      Bytecode::GetLocal(0),
      Bytecode::GetLocal(0),
      Bytecode::GetLocal(1),
      Bytecode::Add,
      Bytecode::Add,
    ];
    let result = evalute(&bytecodes);
    assert_eq!(result, 5);
  }
}
