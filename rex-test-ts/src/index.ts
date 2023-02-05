import {Component, createRoot} from "rex-ts"

@Component({
    view: require("./button.rex")
})
class MyButton {

}

let root = createRoot(new MyButton())

root.hydrate(document.body)

//MyButton, { products: [ { id: 0, name: "produkt1" },  { id: 1, name: "produkt2" },  { id: 2, name: "produkt3" } ] }

