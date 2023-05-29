import { Component, onclick } from "./decorators"
import {BrowserConfig} from "./browser";
export { Component, onclick }

type Props = { [key: string]: Props | Render | Array<Props> }


export interface INode {
    onInit(): void
    onMounted(): void
    onDrop(): void
}

export interface IRender {
    render(): string;
}


export type Render = string | IRender;

export function render(value: Render): string {
    return "hallo"
}

export interface Config<N> {
    el<T>(
        tagName: string,
        attributes: Map<string, () => string>,
        id: () => ([string, T | null] | null),
        children: Array<() => N[]>,
        inAttr: boolean
    ): N[];

    text(text: Render, inAttr: boolean): N[];

    attr(input: Render): string;
}

export type View = { render: <N>(props: Props, config: Config<N>) => N[] }

interface IComponent extends Type<any> {
    _NAME: string
    _VIEW: View,
    _EVENT_LISTENERS: Map<string, () => void>
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

function checkComponent(constructor: Type<any>): void {
    if (
        !constructor.hasOwnProperty("_NAME") ||
        !constructor.hasOwnProperty("_VIEW")
    ) {
        throw new Error(Message.InvalidComponent)
    }
}

export function createRoot(element: HTMLElement): Root {
    return new Root(element)
}

export function createModule(components: Array<Type<any>>): Module {
    let componentMap = new Map()
    for (const c of components) {
        if (componentMap.has(c)) {
            throw new Error(Message.ModuleDuplicateComponent)
        }
        checkComponent(c)
        let component = c as IComponent
        componentMap.set(component._NAME, component)
    }
    return new Module(componentMap)
}

enum Message {
    InvalidComponent = "The component provided is not valid!",
    ComponentRootNotFound = "Can't find component root!",
    ComponentNotFound = "The Component ... was not found!",
    EntryComponentNotFound = "The Component ... was not found!",
    RootNotEmpty = "Can't render into root that is not empty!",
    ComponentAlreadyExist = "",
    ModuleDuplicateComponent = "Component already exists in module!"
}

export class Module {
    constructor(readonly _components: Map<string, IComponent>) {}
}

export class Root {

    constructor(private readonly _root: HTMLElement) {}

    private static isStd(name: string): boolean {
        return true
    }

    private _hydrate(componentEl: Node, component: IComponent, module: Module): void {
        let self = this
        if (!(componentEl instanceof HTMLElement)) {
            throw new Error(HydrateMessage.NodeTypeMismatch)
        }
        if (componentEl.tagName !== component._NAME) {
            throw new Error(HydrateMessage.TagNameMismatch)
        }
        // TODO hand over props to component constructor
        // TODO check attributes
        (componentEl as any)["_COMPONENT_INSTANCE"] = new component()
        let currentEl = componentEl.firstChild
        // _VIEW calls all el(), text() functions on one level
        component._VIEW.render({ name: "" }, {
            attr(input: Render): string {
                return render(input)
            },
            el<T>(
                tagName: string,
                attributes: Map<string, () => string>,
                id: () => ([string, T | null] | null),
                children: Array<() => Node[]>,
                inAttr: boolean
            ): Node[] {
                if (inAttr) {
                    // TODO implement the same render mechanics as in the
                    // TODO normal render function
                    return []
                }
                if (currentEl === null) {
                    throw new Error(HydrateMessage.ChildNotExist)
                }
                if (!(currentEl instanceof HTMLElement)) {
                    throw new Error(HydrateMessage.NodeTypeMismatch)
                }
                if (currentEl.tagName !== tagName) {
                    throw new Error(HydrateMessage.TagNameMismatch)
                }
                if (module._components.has(componentEl.tagName)) {
                    self._hydrate(
                        componentEl,
                        module._components.get(componentEl.tagName)!,
                        module
                    )
                    return []
                }
                if (!Root.isStd(tagName)) {
                    console.warn(`Non standard tag-name: ${tagName}! Did you forget to mount a component? Please don't use non std tags!`)
                }
                // Save checkpoint
                let tmp = currentEl
                for (let child_index = 0;child_index < children.length;child_index++) {
                    if (child_index >= currentEl.childNodes.length) {
                        throw new Error(HydrateMessage.ChildNotExist)
                    }
                    // el is the current cursor. Change it to the child that should be inspected.
                    currentEl = currentEl.childNodes.item(child_index)
                    children[child_index]()
                }
                //After all children processed, advance the position
                currentEl = tmp.nextSibling
                return []
            },
            text(text: Render, inAttr: boolean): Node[] {
                if (inAttr) {
                    return []
                }
                if (currentEl === null) {
                    throw new Error(HydrateMessage.ChildNotExist)
                }
                if (!(currentEl instanceof Text)) {
                    throw new Error(HydrateMessage.NodeTypeMismatch)
                }
                if (currentEl.textContent != text) {
                    throw new Error(HydrateMessage.TextNotEqual)
                }
                currentEl = currentEl.nextSibling
                return []
            }
        })
    }

    private _render(component: IComponent, props: Props, module: Module): Node {
        let self = this
        let root = document.createElement(component._NAME)
        let children = component._VIEW.render(props, {
            el<T>(
                tagName: string,
                attributes: Map<string, () => string>,
                id: () => ([string, (T | null)] | null),
                children: Array<() => Node[]>,
                _inAttr: boolean
            ): Node[] {
                let childComponent = module._components.get(tagName)
                // TODO attributes
                if (childComponent) {
                    // TODO children of components
                    // TODO event handler registration
                    return [self._render(childComponent, props, module)]
                } else {
                    if (!Root.isStd(tagName)) {
                        console.warn(`Non standard tag-name: ${tagName}! Did you forget to mount a component? Please don't use non std tags!`)
                    }
                    let el = document.createElement(tagName)
                    for (const child of children) {
                        el.append(...child())
                    }
                    return [el]
                }
            },
            text(input: Render, _inAttr: boolean): Node[] {
                let text = render(input)
                return [document.createTextNode(text)];
            },
            attr(input: Render): string {
                return render(input);
            }
        })
        root.append(...children)
        return root
    }

    public hydrate(module: Module): void {
        if (this._root.children.length != 1) {
            throw new Error(Message.ComponentRootNotFound)
        }
        let componentRoot = this._root.childNodes.item(0)
        let component = module._components.get(componentRoot.nodeName)
        if (!component) {
            throw new Error(Message.ComponentNotFound)
        }
        this._hydrate(componentRoot, component, module)
    }

    public render(entry: Type<any>, module: Module, props: Props): void {
        if (this._root.children.length != 0) {
            throw new Error(Message.RootNotEmpty)
        }
        checkComponent(entry)
        let entryComponent = entry as IComponent
        let keySet = new Set(module._components.values())
        if (!keySet.has(entryComponent)) {
            throw new Error(Message.EntryComponentNotFound)
        }
        let child = this._render(entryComponent, props, module)
        this._root.appendChild(child)
    }

}
