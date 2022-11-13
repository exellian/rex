export function el(name: string, attributes: { [key: string]: any }, children: any[]): Node {
    let element = document.createElement(name)
    for (const key in attributes) {
        element.setAttribute(key, attributes[key]().toString())
    }
    for (const f_child of children.flat(Infinity)) {
        let child = f_child()
        if (child instanceof Node) {
            element.appendChild(child)
        } else {
            element.appendChild(text(child.toString()))
        }
    }
    return element
}
export function text(value: string): Node {
    return document.createTextNode(value)
}