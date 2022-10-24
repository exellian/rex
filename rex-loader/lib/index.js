"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const path_1 = __importDefault(require("path"));
const rex_compiler_ts_1 = require("rex-compiler-ts");
function default_1(source) {
    let newSource = (0, rex_compiler_ts_1.compileView)(source);
    const filename = path_1.default.basename(this.resourcePath);
    const assetInfo = { sourceFilename: filename };
    this.emitFile(filename, newSource, null, assetInfo);
    return newSource;
}
exports.default = default_1;
