import components/shared_subtitle
import gleam/int
import lib/project_config
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

fn init(_flags) {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model, msg) {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}

fn view(model) {
  html.main([main_container_classes()], [
    html.div([], [html.text("Popup")]),
    shared_subtitle.view("Popup"),
    html.button([attribute.class("btn mt-2")], [html.text("Open Options")]),
  ])
}

fn main_container_classes() {
  attribute.classes([
    #("w-300px", True),
    #("px-4", True),
    #("py-5", True),
    #("text-center", True),
    #("text-gray-700", True),
  ])
}
