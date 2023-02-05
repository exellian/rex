import { el as browserEl, text as browserText } from './browser'

import { Component } from "./decorators"
export { Component }

export interface INode {
    onInit(): void
    onMounted(): void
    onDrop(): void
}

export interface Config<R, T> {
    domEl: (name: string, attributes: { [key: string]: () => any }, children: (() => R)[]) => R
    domText: (text: string) => R
    el: (name: string, attributes: { [key: string]: () => any }, children: (() => T)[]) => T
    text: (text: string) => T
}

export type View = <R, T>(props: { [key: string]: () => any }, config: Config<R, T>) => R

interface IComponent extends Type<any> {
    _NAME: string
    _VIEW: View
}
export declare interface Type<T> extends Function {
    new (...args: any[]): T;
}

enum HydrateMessage {
    NodeTypeMismatch = "",
    TagNameMismatch = "",
    AttributeMismatch = "",
    AttributeValueMismatch = "",
    ChildNotExist = "",
    TextNotEqual = "",
}

export function createRoot<T>(root: T, dependencies?: Array<Type<any>>): Root {
    // TODO type check root and type check dependencies
    let comps = new Map<string, IComponent>()
    if (dependencies) {
        for (let c of dependencies) {
            if (comps.has(c.constructor.name)) {
                throw new Error(Message.ComponentAlreadyExist)
            }
            comps.set(c.constructor.name, c as IComponent)
        }
    }
    return new Root(root as INode, comps)
}

enum Message {
    RootNotFound = "",
    ComponentAlreadyExist = "",
}

export class Root {

    private readonly _root: INode
    private readonly _components: Map<string, IComponent>

    constructor(root: INode, components: Map<string, IComponent>) {
        this._root = root
        this._components = components
    }

    private static isNonStd(name: string): boolean {
        return false
    }

    private _hydrate(el: Node, cc: IComponent): void {
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
                        let value: any = attributes[name]()
                        if (el.getAttribute(name) !== value) {
                            throw new Error(HydrateMessage.AttributeValueMismatch)
                        }
                    }
                }

                if (self._components.has(name)) {
                    self._hydrate(el, self._components.get(name)!)
                } else {
                    if (Root.isNonStd(name)) {
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
        this._hydrate(root, this._root.constructor as IComponent)
    }

}
