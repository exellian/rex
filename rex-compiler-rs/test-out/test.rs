use rex::{Config, BoxedObject, BoxedObjectFn, BoxedNodeFn};
use structx::*;

pub type Props<T: ToString> = Structx! { products: Vec<Structx!{ name: T }> };

pub fn render<'props, T: ToString, DOM_NODE, NODE>(props: &'props Props<T>, config: &rex::Config<DOM_NODE, NODE>) -> DOM_NODE {
    (config.dom_el)("test", HashMap::from([("name".to_string(), Box::new(|| { Box::new((config.el)("div", HashMap::from([]), vec![])) as BoxedObject<'props> }) as BoxedObjectFn<'props>)]), vec![Box::new(|| { Box::new(([props.products]).iter().flat_map(|[product]| { ([props.products]).iter().flat_map(|[product1]| { [([product1]).name] }).collect::<Vec<_>>() }).collect::<Vec<_>>()) as BoxedObject<'props> }) as BoxedObjectFn<'props>]) }