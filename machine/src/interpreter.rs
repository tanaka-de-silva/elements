use super::program::Bytecode;
use super::stack::Stack;
use super::values::VmValue;

pub fn evalute(bytecodes: &Vec<Bytecode>) -> VmValue {
  let mut stack: Stack = Stack::new();
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
}
