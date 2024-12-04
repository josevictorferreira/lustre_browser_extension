import lustre/attribute
import lustre/element/html

pub fn view(context: String) {
  html.p([attribute.class("mt-2 opacity-50")], [
    html.text("This is the " <> context <> " page"),
  ])
}
