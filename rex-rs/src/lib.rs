use std::any::Any;
use std::collections::HashMap;
use std::iter::Map;

pub trait Node {}
pub trait Object {}

impl<'a, T> Object for &'a T {}
impl<T> Object for Vec<T> {}

pub type BoxedObject<'props> = Box<dyn Object + 'props>;
pub type BoxedObjectFn<'props> = Box<dyn Fn() -> BoxedObject<'props> + 'props>;
pub type BoxedNodeFn<'props, NODE> = Box<dyn Fn() -> Vec<NODE> + 'props>;

pub struct Config<DOM_NODE, NODE> {
    dom_el: for<'props> fn(&str, attributes: HashMap<String, BoxedObjectFn<'props>>, children: Vec<BoxedNodeFn<'props, DOM_NODE>>) -> DOM_NODE,
    dom_text: Box<fn(&str) -> DOM_NODE>,
    el: Box<for<'props> fn(&str, attributes: HashMap<String, BoxedObjectFn<'props>>, children: Vec<BoxedNodeFn<'props, NODE>>) -> NODE>,
    text: Box<fn(&str) -> NODE>,
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use structx::*;
    use super::*;

    use structx::*;

    use structx::*;

    use structx::*;

    use structx::*;

    pub type Props<T: ToString + Clone + Copy> = Structx! { products: Vec<Structx!{ name: T }> };

    pub fn render<'props, T: ToString + Copy + Clone, DOM_NODE, NODE: Object>(props: &'props Props<T>, config: &Config<DOM_NODE, NODE>) -> DOM_NODE {
        (config.dom_el)("test", HashMap::from([("name".to_string(), Box::new(|| { Box::new((config.el)("div", HashMap::from([]), vec![])) as BoxedObject<'props> }) as BoxedObjectFn<'props>)]), vec![Box::new(|| { Box::new(([props.products]).iter().flat_map(|[product]| { ([props.products]).iter().flat_map(|[product1]| { [([product1]).name] }).collect::<Vec<_>>() }).collect::<Vec<_>>()) as BoxedObject<'props> }) as BoxedObjectFn<'props>]) }

    #[test]
    fn it_works() {
        let result = add(2, 2);
        let x = structx! { products: vec![structx! { name: 0 }] };

        fn test<T>(x: T) -> Structx! { name: T } {
            structx! {
                name: x
            }
        }

        pub fn rr<T: ToString, DOM_NODE, NODE>(props: &Props<T>, config: &Config<DOM_NODE, NODE>) -> DOM_NODE {
            todo!()
        }

        assert_eq!(result, 4);
    }
}
