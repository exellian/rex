use std::collections::HashMap;

pub trait Node: Render {}
pub trait Flatten {
    type Node;
    fn flatten(self) -> Vec<Self::Node>;
}
pub trait Render {
    fn render(&self) -> String;
}
impl<NODE> Render for dyn Flatten<Node = NODE> {
    fn render(&self) -> String {
        todo!()
    }
}
impl<'a, T> Render for &'a T
where
    T: Render,
{
    fn render(&self) -> String {
        Render::render(*self)
    }
}
impl<T> Render for Vec<T>
where
    T: Render,
{
    fn render(&self) -> String {
        todo!()
    }
}
impl<'a> Render for &'a str {
    fn render(&self) -> String {
        self.to_string()
    }
}
impl Render for String {
    fn render(&self) -> String {
        self.clone()
    }
}

impl<T> Flatten for T
where
    T: Node,
{
    type Node = T;

    #[inline]
    fn flatten(self) -> Vec<Self::Node> {
        vec![self]
    }
}

impl<T, NODE> Flatten for Vec<T>
where
    T: Flatten<Node = NODE>,
{
    type Node = NODE;
    fn flatten(self) -> Vec<NODE> {
        self.into_iter()
            .flat_map(|x| x.flatten())
            .collect::<Vec<_>>()
    }
}

pub type AttributeValue<'props> = Box<dyn Render + 'props>;

pub type AttributeFn<'props> = Box<dyn FnOnce() -> AttributeValue<'props> + 'props>;

pub type ChildValue<'props, NODE> = Box<dyn Flatten<Node = NODE> + 'props>;
/**
 * Always returns a vector of nodes because we need to support higher order for loops
 * which would result in Array<Array<NODE>> types which need to be flattened to Array<Node>
 * But can't be transformed into just NODE
 */
pub type ChildFn<'props, NODE> = Box<dyn FnOnce() -> ChildValue<'props, NODE> + 'props>;

pub struct Config<NODE> {
    el: for<'props> fn(
        &str,
        attributes: HashMap<String, AttributeFn<'props>>,
        children: Vec<ChildFn<'props, NODE>>,
    ) -> NODE,
    text: fn(&str) -> NODE,
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use structx::*;

    pub type Props<T: Render, T1: Render> = Structx! {

        items: Vec<Structx!{ age: T }>,
        products: Vec<Structx!{ name: T1 }>
    };
    pub fn render<'props, T: Render, T1: Render, NODE: Node>(
        props: &'props Props<T, T1>,
        config: &'props Config<NODE>,
    ) -> impl Flatten<Node = NODE> {
        (config.el)(
            "test",
            HashMap::from([(
                "name".to_string(),
                Box::new(|| Box::new("Hallo Welt") as AttributeValue<'props>)
                    as AttributeFn<'props>,
            )]),
            vec![
                Box::new(|| {
                    Box::new((config.el)(
                        "div",
                        HashMap::from([(
                            "class".to_string(),
                            Box::new(|| Box::new("well-done") as AttributeValue<'props>)
                                as AttributeFn<'props>,
                        )]),
                        vec![Box::new(|| {
                            Box::new((config.el)(
                                "a",
                                HashMap::from([(
                                    "href".to_string(),
                                    Box::new(|| {
                                        Box::new("https://localhost:8000/post")
                                            as AttributeValue<'props>
                                    }) as AttributeFn<'props>,
                                )]),
                                vec![],
                            )) as ChildValue<'props, NODE>
                        }) as ChildFn<'props, NODE>],
                    )) as ChildValue<'props, NODE>
                }) as ChildFn<'props, NODE>,
                Box::new(|| {
                    Box::new((config.text)(
                        &((&props.products)
                            .into_iter()
                            .map(|product| {
                                (&props.products)
                                    .into_iter()
                                    .map(|product1| &(&product1).name)
                                    .collect::<Vec<_>>()
                            })
                            .collect::<Vec<_>>())
                        .render(),
                    )) as ChildValue<'props, NODE>
                }) as ChildFn<'props, NODE>,
                Box::new(|| {
                    Box::new((config.el)(
                        "ul",
                        HashMap::from([]),
                        vec![Box::new(|| {
                            Box::new(
                                (&props.items)
                                    .into_iter()
                                    .map(|x| {
                                        (config.el)(
                                            "li",
                                            HashMap::from([]),
                                            vec![Box::new(|| {
                                                Box::new((config.text)(&(&(&x).age).render()))
                                                    as ChildValue<'props, NODE>
                                            })
                                                as ChildFn<'props, NODE>],
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                            ) as ChildValue<'props, NODE>
                        }) as ChildFn<'props, NODE>],
                    )) as ChildValue<'props, NODE>
                }) as ChildFn<'props, NODE>,
            ],
        )
    }

    #[test]
    fn it_works() {
        let test: Props<usize, usize> =
            structx! { items: vec![], products: Vec::from([structx!{ name: 0 }]) };
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
