"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Component = void 0;
function Component(viewConstructor) {
    return (constructor) => {
        return class extends constructor {
            constructor(...args) {
                super(...args);
            }
            onClick() {
                console.log("test");
            }
        };
    };
}
exports.Component = Component;
