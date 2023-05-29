import {Config, render, Render} from "./index";

export class BrowserConfig implements Config<Node> {
    attr(input: Render): string {
        return render(input);
    }

    el<T>(
        tagName: string,
        attributes: Map<string, () => string>,
        id: () => ([string, T | null] | null),
        children: Array<() => Node[]>,
        inAttr: boolean
    ): Node[] {
        let element= document.createElement(tagName)
        for (const [key, val] of attributes) {
            element.setAttribute(key, val())
        }
        for (const childFn of children) {
            let childs = childFn()
            childs.forEach(element.appendChild)
        }
        return [element];
    }

    text(text: Render, inAttr: boolean): Node[] {
        let element = document.createTextNode(render(text))
        return [element]
    }

}