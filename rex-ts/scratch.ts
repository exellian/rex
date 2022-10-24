import { hydrate, Component } from './src'

@Component({
    view: "../button.rex"
})
class Button {

    constructor() {
        
    }
}

hydrate(null, Button)

export default (props: any) => {

}
