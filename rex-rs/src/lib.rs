use std::any::Any;
use std::iter::Map;

pub struct Config<R, T> {
    dom_el: Box<fn(String, attributes: Map<String, Box<for<'a> fn(&'a str) -> Box<dyn Any>>>, children: Vec<fn() -> R>) -> R>,
    dom_text: Box<fn(String) -> R>,
    el: Box<fn(String, attributes: Map<String, Box<fn() -> Box<dyn Any>>>, children: Vec<fn() -> T>) -> T>,
    text: Box<fn(text: String) -> T>
}

pub fn add(left: usize, right: usize) -> usize {
    left + right

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
