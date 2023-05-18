# Rex

Rex wird ein Webframework ähnlich wie React und Angular. Basis bilden reaktive Html/Css/Js Komponenten, mit denen Webseiten aufgebaut werden können. Kern des Projekts bildet dabei eine eigene Templating-Sprache, die für verschiedene Backends (insbesondere Rust) genutzt werden kann.

## Templating Sprache

Momentaner Fokus ist die Fertigstellung der Templatingsprache:

```html
<div>
    {
        for product in products {
            <div>{product.name}</div>
        }
    }
</div>
```
wird zu folgendem Rust-Code transpiled:
```rust
use rex::{AttributeFn, AttributeValue, ChildFn, ChildValue, Config};
use structx::*;
pub type Props<T: Render> = Structx! { 
    products: Vec<Structx!{ name: T }> 
};
pub fn render<'props, T: Render, NODE: Node>(
    props: &'props Props<T>,
    config: &'props Config<NODE>,
) -> impl Flatten<Node = NODE> {
    (config.el)(
        "div",
        HashMap::from([]),
        vec![Box::new(|| {
            Box::new(
                (props.products)
                    .iter()
                    .map(|product| {
                        (config.el)(
                            "div",
                            HashMap::from([]),
                            vec![Box::new(|| {
                                Box::new((config.text)(&((product).name).render()))
                                    as ChildValue<'props, NODE>
                            }) as ChildFn<'props, NODE>],
                        )
                    })
                    .collect::<Vec<_>>(),
            ) as ChildValue<'props, NODE>
        }) as ChildFn<'props, NODE>],
    )
}
```
Die Grundidee ist dabei, dass die Templating-Sprache sich 1:1 in Rust-Code/JavaScript-Code übersetzen lässt. Das Template ist dabei möglichst allgemein gehalten, sodass damit nicht nur einfache Strings erzeugt werden können, sondern beliebige Objekte. Die erzeugte Methode kann sogar nicht nur zum Erzeugen/Rendern von der Seitenstruktur benutzt werden, sondern auch vorallem zum Vergleichen mit einer anderen Struktur. Dies ist vorallem bei der Hydration einer Website wichtig. 

Desweiteren wird das erzeugte Template in einer lazy-evaluation-Variante erzeugt. Das heißt beim Aufrufen werden Attribute und Kinder nicht direkt evaluiert, sondern werden der Element-Erzeuger-Funktion als Funktionen höherer Ordnung zur Verfügung gestellt. Dies ermöglicht eine Top-down-Evaluation des Templates und ist besonders wichtig, um effiziente Hydration zu realisieren. 

Templates werden außerdem in einer sehr generischen Weise generiert, sodass sie letztendlich mit beliebigen Daten gefüttert werden können.

Hezstück ist außerdem die automatische Typinferenz, die aus jedem rohen Template einen Props-Typ generiert, welcher ein Modell der hineinzugebenden Daten ist. Um gute Performance zu erreichen verwende ich hier anonyme-Strukturen (siehe Structural-Records), die mithilfe des externen Crates structx (https://github.com/oooutlk/structx) realisiert werden. Die Generierung von anonymen Strukturen erlaubt es komplett um eine Serialisierung herumzukommen, wie viele andere Rust-Templatingsprachen es machen. Die Templates können so mit der nativen Rust-Performance evaluiert werden.

Momentaner Fokus ist nun die korrekte Generierung von Dereferenzierungen und Clone-Aufrufen. 

## Fusion zum reaktiven Webframework

Nachdem die Templating-Sprache fertiggestellt ist, ist der nächste Schritt die Fusion zu einem produktionsreifen reaktiven Webframework. Das bedeutet Templates müssen in reaktive Web-Komponenten eingebettet werden können. Rex soll nämlich nicht nur eine Templating-Sprache sein, sondern vorallem ein Ersatz für bisherige SPA-Frameworks. Der große Vorteil wäre dann, dass man performantes ServerSideRendering durch Rust ermöglichen könnte. Web-Templates wären also endlich Programmiersprachen-unabhängig ohne, dass man auf Features wie reaktive Web-Komponenten verzichten müsste.

