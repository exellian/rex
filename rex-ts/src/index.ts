
import { Component } from './decorators'
export { Component }

export interface IComponent {
    onClick(): void
}

export function hydrate<C extends { new(...args: any[]): any }>(element: HTMLElement, Component: C, props: any): void {

    let obj = new Component(props)
    let node = obj.render()
    element?.appendChild(node)
}