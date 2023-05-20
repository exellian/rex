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
        self.iter().map(|x| x.render()).collect::<Vec<_>>().join("")
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
impl Render for usize {
    fn render(&self) -> String {
        format!("{}", self)
    }
}

impl<T> Flatten for T
where
    T: Node,
{
    type Node = T;

    fn flatten(self) -> Vec<Self::Node> {
        vec![self]
    }
}

impl<T> Flatten for Vec<T>
where
    T: Flatten,
{
    type Node = T::Node;
    fn flatten(self) -> Vec<Self::Node> {
        self.into_iter()
            .flat_map(|x| x.flatten())
            .collect::<Vec<_>>()
    }
}

pub trait Config {
    type Node: Node;

    fn el<'props>(
        tag_name: &str,
        attributes: impl FnOnce() -> HashMap<String, String> + 'props,
        children: impl FnOnce() -> Vec<Self::Node> + 'props,
        in_attr: bool,
    ) -> Vec<Self::Node>;

    fn text(text: impl Render, in_attr: bool) -> Vec<Self::Node>;

    fn attr(input: impl Render) -> String;
}

#[cfg(test)]
mod tests {
    //use rex::{AttributeFn, AttributeValue, ChildFn, ChildValue, Config};

    use crate::{Config, Flatten, Node, Render};
    use std::collections::HashMap;

    use structx::*;
    pub type Props<T, T1> =
        Structx! { items: Vec<Structx!{ age: T }>,products: Vec<Structx!{ name: T1 }> };
    pub fn render<T: Render, T1: Render, C: Config>(props: &Props<T, T1>) -> Vec<C::Node> {
        C::el(
            "test",
            || {
                HashMap::from([
                    ("name".to_string(), C::attr("Hallo Welt")),
                    (
                        "test".to_string(),
                        C::attr(C::el(
                            "div",
                            || HashMap::from([]),
                            || {
                                vec![
                                    (C::text("Hallo", true)).flatten(),
                                    (C::text("hafksjdlfj", true)).flatten(),
                                ]
                                .flatten()
                            },
                            true,
                        )),
                    ),
                ])
            },
            || {
                vec![
                    (C::el(
                        "div",
                        || HashMap::from([("class".to_string(), C::attr("well-done"))]),
                        || {
                            vec![(C::el(
                                "a",
                                || {
                                    HashMap::from([(
                                        "href".to_string(),
                                        C::attr("https://localhost:8000/post"),
                                    )])
                                },
                                || vec![],
                                false,
                            ))
                            .flatten()]
                            .flatten()
                        },
                        false,
                    ))
                    .flatten(),
                    C::text(&(3243 + 32324), false),
                    C::text(
                        (props.products)
                            .iter()
                            .map(|_| {
                                (props.products)
                                    .iter()
                                    .map(|product1| &(product1).name)
                                    .collect::<Vec<_>>()
                            })
                            .collect::<Vec<_>>(),
                        false,
                    ),
                    (C::el(
                        "ul",
                        || HashMap::from([]),
                        || {
                            vec![((props.items)
                                .iter()
                                .map(|x| {
                                    C::el(
                                        "li",
                                        || HashMap::from([]),
                                        || vec![C::text(&(x).age, false)].flatten(),
                                        false,
                                    )
                                })
                                .collect::<Vec<_>>())
                            .flatten()]
                            .flatten()
                        },
                        false,
                    ))
                    .flatten(),
                ]
                .flatten()
            },
            false,
        )
    }

    #[test]
    fn render_test() {
        struct HtmlConfig;
        impl Node for String {}

        impl Config for HtmlConfig {
            type Node = String;

            fn el<'props>(
                tag_name: &str,
                attributes: impl FnOnce() -> HashMap<String, String> + 'props,
                children: impl FnOnce() -> Vec<Self::Node> + 'props,
                in_attr: bool,
            ) -> Vec<Self::Node> {
                let attrs = attributes();
                let mut attr_str = String::new();
                let mut index = 0;
                for (key, val) in attrs {
                    if index == 0 {
                        attr_str.push(' ');
                    }
                    attr_str.push_str(&*format!("{}=\"{}\"", key, val.render()));
                    index += 1;
                }
                let open = format!("<{}{}>", tag_name, attr_str);
                let body = children().join("");
                let close = format!("</{}>", tag_name);
                vec![format!("{}{}{}", open, body, close)]
            }

            fn text(text: impl Render, in_attr: bool) -> Vec<Self::Node> {
                let res = text.render();
                vec![format!("{}", res)]
            }

            fn attr(input: impl Render) -> String {
                let res = input.render();
                res.replace("\"", "&quot;")
            }
        }

        let props: Props<usize, &str> = structx! {
            items: vec![
                structx! { age: 10 },
                structx! { age: 11 },
                structx! { age: 12 },
                structx! { age: 13 },
                structx! { age: 14 }
            ],
            products: vec![
                structx! { name: "lel" }
            ],
        };

        let res = render::<_, _, HtmlConfig>(&props).join("");
        println!("{}", res);
    }
}
