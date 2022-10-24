import path from 'path'
import * as webpack from 'webpack'
// in case you run into any typescript error when configuring `devServer`
import 'webpack-dev-server'
import HtmlWebpackPlugin from "html-webpack-plugin"

const config: webpack.Configuration = {
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
        path: path.resolve(__dirname, 'dist'),
        filename: 'bundle.js',
    },
    plugins: [new HtmlWebpackPlugin()]
};

export default config;