use std::convert::TryInto;
use super::program::{LocalVarIndex};

pub struct Locals {
    values: Vec<u8>,
}

impl Locals {
    pub fn new() -> Locals {
        Locals { values: Vec::}
    }

    pub fn read_int(&mut self, index: LocalVarIndex) -> i32 {
        let final_length = self.values.len().saturating_sub(std::mem::size_of::<i32>());
        let tail = self.values.split_off(final_length);
        self.values.truncate(final_length);
        return i32::from_le_bytes(tail.try_into().unwrap());
    }

    pub fn read_bool(&mut self, index: LocalVarIndex) -> bool {
        return self.read_int() != 0;
    }

    pub fn read_long(&mut self, index: LocalVarIndex) -> i64 {
        let final_length = self.values.len().saturating_sub(std::mem::size_of::<i64>());
        let tail = self.values.split_off(final_length);
        self.values.truncate(final_length);
        return i64::from_le_bytes(tail.try_into().unwrap());
    }

    pub fn read_double(&mut self, index: LocalVarIndex) -> f64 {
        let final_length = self.values.len().saturating_sub(std::mem::size_of::<f64>());
        let tail = self.values.split_off(final_length);
        self.values.truncate(final_length);
        return f64::from_le_bytes(tail.try_into().unwrap());
    }

    pub fn write_int(&mut self, index: LocalVarIndex, value: i32) -> () {
        let mut int_bytes = value.to_le_bytes().to_vec();
        self.values.append(&mut int_bytes);
    }

    pub fn write_bool(&mut self, index: LocalVarIndex, value: bool) -> () {
        self.write_int(value as i32);
    }

    pub fn write_long(&mut self, index: LocalVarIndex, value: i64) -> () {
        let mut int_bytes = value.to_le_bytes().to_vec();
        self.values.append(&mut int_bytes);
    }

    pub fn write_double(&mut self, index: LocalVarIndex, value: f64) -> () {
        let mut int_bytes = value.to_le_bytes().to_vec();
        self.values.append(&mut int_bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_write_and_read_values() {
        let mut locals = Locals::new();
        locals.write_int(1);
        locals.write_int(2);
        let first_pop = locals.read_int();
        let second_pop = locals.read_int();
        assert_eq!(first_pop, 2);
        assert_eq!(second_pop, 1);
    }
}
