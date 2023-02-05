import {Config, IComponent, INode, View} from "."
import {el, text} from "./browser";
import {IOptions} from "./options";

export function Component(options: IOptions) {
    return <T extends { new(...args: any[]): {} }>(constructor: T) => {
        return class extends constructor implements INode {
            static _NAME: string = constructor.name
            static _VIEW: View = require(options.view)
            props: any
            constructor(...args: any[]) {
                super(...args)
                if (args.length < 1)
                    throw new Error("props parameter not supplied!")
                this.props = args[0]
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