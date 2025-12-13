// use std::cell::Cell;

// use crate::stream::Stream;

// // Abandoning this ig. Makes the tokenizer unnecessarily difficult.
// // the tokenizer will instead operate on a string slice representing the contents of a file
// pub struct FileCharStream {
//     content: String,
//     current_index: Cell<usize>,
// }

// impl Stream<char> for FileCharStream {
//     fn new(value: String) -> Self {
//         FileCharStream {
//             content: value.to_owned(),
//             current_index: Cell::new(0 as usize),
//         }
//     }

//     fn peek(&self) -> Option<char> {
//         self.content.chars().nth(self.current_index.get() + 1)
//     }
//     fn next(&mut self) -> Option<char> {
//         self.current_index.set(self.current_index.get() + 1);
//         self.content.chars().nth(self.current_index.get())
//     }

//     fn is_end(&self) -> bool {
//         self.content.chars().nth(self.current_index.get()).is_none()
//     }
// }
