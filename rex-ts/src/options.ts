import {Config} from "./index";

export interface Options {
    view: <T>(props: string, config: Config<T, Node>) => T
}
