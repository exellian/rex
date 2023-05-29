import {Component, createModule, createRoot, onclick} from "rex-ts"
import {onmount} from "rex-ts/src/decorators";

@Component({
    view: require("./button.rex")
})
class MyButton {

    //@onmount()
    mount() {
        console.log("component mounted!")
    }

    @onclick("product")
    click(event: Event) {
        console.log("click");
    }
}


let module = createModule([
    MyButton

])
let root = createRoot(document.body)
root.render(MyButton, module, { products: [
    { name: "hallo" }, { name: "skrr" }
    ] })
//root.hydrate(document.body)

//MyButton, { products: [ { id: 0, name: "produkt1" },  { id: 1, name: "produkt2" },  { id: 2, name: "produkt3" } ] }

