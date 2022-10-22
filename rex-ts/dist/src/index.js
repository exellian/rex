"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.hydrate = exports.Component = void 0;
const decorators_1 = require("./decorators");
Object.defineProperty(exports, "Component", { enumerable: true, get: function () { return decorators_1.Component; } });
function hydrate(element, component) {
    let obj = new component();
    obj.onClick();
    console.log(obj);
}
exports.hydrate = hydrate;
