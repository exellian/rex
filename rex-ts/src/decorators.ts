import { IComponent } from "."
import {Options} from "./options";
import {createElement, createTextNode} from "./browser";

export function Component(options: Options) {
    return <T extends { new(...args: any[]): {} }>(constructor: T) => {
        return class Component extends constructor implements IComponent {
            props: any;
            constructor(...args: any[]) {
                super(...args)
                if (args.length < 1)
                    throw new Error("props parameter not supplied!")
                this.props = args[0]
            }

            render(): Node {
                return options.view(this.props, {
                    createElement: createElement,
                    createTextNode: createTextNode
                })
            }

            onClick(): void {
                console.log("test")
            }
        }
    }
}