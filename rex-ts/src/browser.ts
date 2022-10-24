export function createElement(name: string, attributes: { [key: string]: any }, children: any[]): Node {
    let element = document.createElement(name)
    for (const key in attributes) {
        element.setAttribute(key, attributes[key].toString())
    }
    console.log(children)
    children.flatMap(value => {

    })
    for (const child of children.flat(Infinity)) {
        console.log(child)
        if (child instanceof Node) {
            element.appendChild(child)
        } else {
            element.appendChild(createTextNode(child.toString()))
        }
    }
    return element
}
export function createTextNode(value: string): Node {
    return document.createTextNode(value)
}