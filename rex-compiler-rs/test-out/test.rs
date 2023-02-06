struct A0 {
    name: T,
}

struct Props {
    products: Vec<A0>,
}

fn render(props: &Props, config: &rex::Config) { config.dom_el("test", HashMap::from([(name, || { config.el("div", HashMap::from([]), vec![]) })]), vec![|| { (props.products).map(|product| { (props.products).map(|product1| { (product1).name }) }) }]) }