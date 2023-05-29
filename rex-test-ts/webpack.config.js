"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const path_1 = __importDefault(require("path"));
// in case you run into any typescript error when configuring `devServer`
require("webpack-dev-server");
const html_webpack_plugin_1 = __importDefault(require("html-webpack-plugin"));
const config = {
    module: {
        rules: [
            { test: /\.ts$/, loader: 'ts-loader', exclude: /node_modules/ },
            { test: /\.rex$/, use: ['rex-loader'], exclude: /node_modules/ }
        ]
    },
    resolve: {
        extensions: ['.ts', '.js', '.rex']
    },
    devtool: 'inline-source-map',
    mode: 'development',
    entry: './src/index.ts',
    output: {
        path: path_1.default.resolve(__dirname, 'dist'),
        filename: 'bundle.js',
    },
    plugins: [new html_webpack_plugin_1.default()]
};
exports.default = config;
//# sourceMappingURL=webpack.config.js.map