
pub mod lexer;
pub mod parser;

// pub struct Reader<T> {
//     parser: Parser<T>,
// }

// impl<T> Reader<T> 
// where T: Iterator<Item = Token>
// {
//     pub fn new(lexer: T) -> Self {
//         Self {
//             parser: Parser::new(lexer),
//         }
//     }

//     pub fn read(&mut self) -> Result<Box<Node>, Error> {
//         self.parser.parse()
//     }
// }
