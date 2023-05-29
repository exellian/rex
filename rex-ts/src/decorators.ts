import {Config, INode, View} from "."
import {IOptions} from "./options";

const EVENT_LISTENER_KEY = "_EVENT_LISTENER"

export function onclick(selector: string) {
    return (target: any, propertyKey: string, _: PropertyDescriptor) => {
        if (!target.hasOwnProperty(EVENT_LISTENER_KEY)) {
            target[EVENT_LISTENER_KEY] = new Map()
        }
        target[EVENT_LISTENER_KEY].set(propertyKey, target[propertyKey])
    }
}

export function onmount() {
    return (target: any, propertyKey: string, _: PropertyDescriptor) => {
        if (!target[EVENT_LISTENER_KEY]) {
            target[EVENT_LISTENER_KEY] = new Map()
        }
        target[EVENT_LISTENER_KEY].set(propertyKey, target[propertyKey])
    }
}



export function Component(options: IOptions) {
    return <T extends { new(...args: any[]): {} }>(constructor: T) => {
        return class extends constructor implements INode {
            static _NAME: string = constructor.name
            static _VIEW: View = options.view
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