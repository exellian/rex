import path from 'path'
import { compileView } from "rex-compiler-ts";

export default function(this: any, source: string): string {

    let newSource = compileView(source)

    const filename = path.basename(this.resourcePath)
    const assetInfo = { sourceFilename: filename }

    this.emitFile(filename, newSource, null, assetInfo)

    return newSource
}