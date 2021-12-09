use std::env;
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::Path;
mod interpreter;
mod values;
mod stack;
mod program;

fn read_file_as_string(path: &Path) -> Result<String, io::Error> {
    let mut file = File::open(path)?;
    let mut string_buffer = String::new();
    file.read_to_string(&mut string_buffer)?;
    return Ok(string_buffer);
}

fn run_program(path: &Path) -> () {
    match read_file_as_string(&path) {
        Err(cause) => panic!("couldn't read {}: {}", path.display(), cause),
        Ok(contents) => {
            let decoded: program::Program = serde_json::from_str(&contents).unwrap();
            let result = interpreter::evalute(&decoded.bytecodes);
            println!("{:?}", decoded);
            println!("{:?}", result);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = Path::new(&args[1]);
    println!("{:?}", file_path);
    run_program(&file_path)
}
