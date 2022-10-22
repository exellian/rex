import { IComponent } from "."
import { View } from "./view"

export function Component<V extends { new(): View } >(viewConstructor: V) {
    return <T extends { new(...args: any[]): {} }>(constructor: T) => {
        return class extends constructor implements IComponent {
            constructor(...args: any[]) {
                super(...args)
            }

            onClick(): void {
                console.log("test")
            }
        }
    }
}