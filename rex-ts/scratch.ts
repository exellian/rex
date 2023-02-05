import {Component, createRoot} from './src'

@Component({
    view: "views/button.rex"
})
class Button {

    constructor() {
        
    }
}

let root = createRoot(Button)
root.hydrate(document.getElementById("root")!)
