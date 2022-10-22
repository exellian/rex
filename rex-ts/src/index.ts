
import { Component } from './decorators'
export { Component }

export interface IComponent {
    onClick(): void
}

export function hydrate<C extends { new(...args: any[]): any }>(element: HTMLElement | null, component: C): void {
    let obj = new component()
    obj.onClick()
    console.log(obj)
}