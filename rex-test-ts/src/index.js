"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
const rex_ts_1 = require("rex-ts");
let MyButton = class MyButton {
    //@onmount()
    mount() {
        console.log("component mounted!");
    }
    click(event) {
        console.log("click");
    }
};
__decorate([
    (0, rex_ts_1.onclick)("product")
], MyButton.prototype, "click", null);
MyButton = __decorate([
    (0, rex_ts_1.Component)({
        view: require("./button.rex")
    })
], MyButton);
let module = (0, rex_ts_1.createModule)([
    MyButton
]);
let root = (0, rex_ts_1.createRoot)(document.body);
root.render(MyButton, module, { products: [
        { name: "hallo" }, { name: "skrr" }
    ] });
//root.hydrate(document.body)
//MyButton, { products: [ { id: 0, name: "produkt1" },  { id: 1, name: "produkt2" },  { id: 2, name: "produkt3" } ] }
//# sourceMappingURL=index.js.map