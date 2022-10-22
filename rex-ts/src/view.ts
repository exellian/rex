
export interface View {
    renderToElement(props: {}): HTMLElement
    renderToString(props: {}): string
}

export function compile(code: string): View {
    return {
        renderToElement(props: {}): HTMLElement {
            return document.createElement("div")
        },
        
        renderToString(props: {}): string {
            return ""
        }
    }
}