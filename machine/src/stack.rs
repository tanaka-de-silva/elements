use std::convert::TryInto;

pub struct Stack {
    values: Vec<u8>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack { values: Vec::new() }
    }

    pub fn pop_int(&mut self) -> i32 {
        let final_length = self.values.len().saturating_sub(std::mem::size_of::<i32>());
        let tail = self.values.split_off(final_length);
        self.values.truncate(final_length);
        return i32::from_le_bytes(tail.try_into().unwrap());
    }

    pub fn pop_bool(&mut self) -> bool {
        return self.pop_int() != 0;
    }

    pub fn pop_long(&mut self) -> i64 {
        let final_length = self.values.len().saturating_sub(std::mem::size_of::<i64>());
        let tail = self.values.split_off(final_length);
        self.values.truncate(final_length);
        return i64::from_le_bytes(tail.try_into().unwrap());
    }

    pub fn pop_double(&mut self) -> f64 {
        let final_length = self.values.len().saturating_sub(std::mem::size_of::<f64>());
        let tail = self.values.split_off(final_length);
        self.values.truncate(final_length);
        return f64::from_le_bytes(tail.try_into().unwrap());
    }

    pub fn push_bool(&mut self, value: bool) -> () {
        self.push_int(value as i32);
    }

    pub fn push_int(&mut self, value: i32) -> () {
        let mut int_bytes = value.to_le_bytes().to_vec();
        self.values.append(&mut int_bytes);
    }

    pub fn push_long(&mut self, value: i64) -> () {
        let mut int_bytes = value.to_le_bytes().to_vec();
        self.values.append(&mut int_bytes);
    }

    pub fn push_double(&mut self, value: f64) -> () {
        let mut int_bytes = value.to_le_bytes().to_vec();
        self.values.append(&mut int_bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_push_and_pop_values() {
        let mut stack = Stack::new();
        stack.push_int(1);
        stack.push_int(2);
        let first_pop = stack.pop_int();
        let second_pop = stack.pop_int();
        assert_eq!(first_pop, 2);
        assert_eq!(second_pop, 1);
    }
}
