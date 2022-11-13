import {IComponent, IComponentView} from "."
import {Options} from "./options";
import {el, text} from "./browser";

export function Component(options: Options) {
    return <T extends { new(...args: any[]): {} }>(constructor: T) => {
        return class Component extends constructor implements IComponent, IComponentView {
            static _VIEW: (props: string, config: any) => any = options.view
            props: any
            constructor(...args: any[]) {
                super(...args)
                if (args.length < 1)
                    throw new Error("props parameter not supplied!")
                this.props = args[0]
            }

            render(): Node {
                return Component._VIEW(this.props, {
                    el: el,
                    text: text
                })
            }

            onClick(): void {
                console.log("test")
            }

            onDrop(): void {
            }

            onInit(): void {
            }

            onMounted(): void {
            }
        }
    }
}