//! Source is a wrapper around an iterator of strings that implements the Iterator trait.
//! 
//! Examples:
//! 
//! ```
//! // Create a source from stdin, e.g. for a REPL
//! let source = Source::new(std::io::stdin().lines().map(|line| line.unwrap()));
//! 
//! // Create a source from a vector of strings, e.g. for testing
//! let source = Source::new(vec!["foo", "bar", "baz"].iter().map(|s| s.to_string()));
//! ```
pub struct Source<T: Iterator<Item=String>> {
    inner: T,
}

impl<T: Iterator<Item=String>> Source<T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner,
        }
    }
}

impl <T: Iterator<Item=String>> Iterator for Source<T> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}


