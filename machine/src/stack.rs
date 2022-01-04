use super::values::VmValue;

pub struct Stack {
    values: Vec<VmValue>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack { values: Vec::new() }
    }

    pub fn top(&self) -> VmValue {
        *self.values.last().unwrap()
    }

    pub fn pop(&mut self) -> Option<VmValue> {
        self.values.pop()
    }

    pub fn push(&mut self, value: VmValue) -> () {
        self.values.push(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_push_and_pop_values() {
        let mut stack = Stack::new();
        stack.push(1);
        stack.push(2);
        let first_pop = stack.pop();
        assert_eq!(first_pop, Option::Some(2));
        let top = stack.top();
        assert_eq!(top, 1);
    }
}
