import { el as browserEl, text as browserText } from './browser'

export interface IComponent {
    onInit(): void
    onMounted(): void
    onDrop(): void
}

export interface IComponentView {
    render(): Node
}

export interface Config<R, T> {
    domEl: (name: string, attributes: { [key: string]: () => any }, children: (() => R)[]) => R
    domText: (text: string) => R
    el: (name: string, attributes: { [key: string]: () => any }, children: (() => T)[]) => T
    text: (text: string) => T
}

export type View = (props: { [key: string]: any }, config: Config<any, any>) => any

export interface ComponentConstructor {
    _NAME: string
    _VIEW: View
    new(props: any): IComponent | IComponentView
}

enum HydrateMessage {
    NodeTypeMismatch = "",
    TagNameMismatch = "",
    AttributeMismatch = "",
    AttributeValueMismatch = "",
    ChildNotExist = "",
    TextNotEqual = "",
}

enum Message {
    RootNotFound = "",
    ComponentAlreadyExist = "",
}

export class Reactor {

    private readonly _root: ComponentConstructor
    private readonly _components: Map<string, ComponentConstructor>

    constructor(root: ComponentConstructor) {
        this._root = root
        this._components = new Map<string, ComponentConstructor>()
    }

    public mount(cc: ComponentConstructor): void {
        if (this._components.has(cc._NAME)) {
            throw new Error(Message.ComponentAlreadyExist)
        }
        this._components.set(cc._NAME, cc)
    }

    private static isNonStd(name: string): boolean {
        return false
    }

    private _hydrate(el: Node, cc: ComponentConstructor): void {
        let self = this
        let component = el
        //
        cc._VIEW({}, {
            domEl(name: string, attributes: { [p: string]: () => any }, children: (() => void)[]): void {
                if (!(el instanceof HTMLElement)) {
                    throw new Error(HydrateMessage.NodeTypeMismatch)
                }
                if (el.tagName !== name) {
                    throw new Error(HydrateMessage.TagNameMismatch)
                }
                if (component == el) {
                    let props: { [name: string]: any } = {}
                    for (const name in attributes) {
                        if (!el.hasAttribute(name)) {
                            throw new Error(HydrateMessage.AttributeMismatch)
                        }
                        props[name] = attributes[name]()
                    }
                    (component as any)["component"] = new cc(props)
                } else {
                    for (const name in attributes) {
                        if (!el.hasAttribute(name)) {
                            throw new Error(HydrateMessage.AttributeMismatch)
                        }
                        let value = attributes[name]()
                        if (el.getAttribute(name) !== value) {
                            throw new Error(HydrateMessage.AttributeValueMismatch)
                        }
                    }
                }

                if (self._components.has(name)) {
                    self._hydrate(el, self._components.get(name)!)
                } else {
                    if (Reactor.isNonStd(name)) {
                        console.warn(`Non standard tag-name: ${name}! Did you forget to mount a component? Please don't use non std tags!`)
                    }
                    let node = el
                    for (let child_index = 0;child_index < children.length;child_index++) {
                        if (child_index >= node.childNodes.length) {
                            throw new Error(HydrateMessage.ChildNotExist)
                        }
                        // el is the current cursor. Change it to the child that should be inspected.
                        el = node.childNodes.item(child_index)
                        children[child_index]()
                    }
                }
            },
            domText(text: string): void {
                if (!(el instanceof Text)) {
                    throw new Error(HydrateMessage.NodeTypeMismatch)
                }
                if (el.textContent != text) {
                    throw new Error(HydrateMessage.TextNotEqual)
                }
            },
            el: browserEl,
            text: browserText
        })

    }

    public hydrate(element: HTMLElement): void {
        if (element.children.length != 1) {
            throw new Error(Message.RootNotFound)
        }
        let root = element.childNodes.item(0)
        this._hydrate(root, this._root)
    }

}
