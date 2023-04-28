mod eval;
mod read;
mod print;
mod object;
mod repl;
mod stdlib;

fn main() {
    println!("athir (c) 2023 Athir LLC");

    repl::repl();

}