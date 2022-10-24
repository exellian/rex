export interface View {
    render<T>(props: {}, createElement: () => T): T
}
