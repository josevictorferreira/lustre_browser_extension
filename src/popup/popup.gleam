import components/logo
import components/shared_subtitle
import lustre
import lustre/attribute
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

fn init(_flags) {
  "Popup"
}

type Msg {
  UserClickedOpenOptionPage
}

fn update(_model, msg) {
  case msg {
    UserClickedOpenOptionPage -> open_options_page()
  }
}

fn view(model) {
  html.main([main_container_classes()], [
    html.div([], [html.text(model)]),
    logo.view(),
    shared_subtitle.view(model),
    html.button(
      [attribute.class("btn mt-2"), event.on_click(UserClickedOpenOptionPage)],
      [html.text("Open Options")],
    ),
  ])
}

fn main_container_classes() {
  attribute.class(
    "
    w-300px
    px-4
    py-5
    text-center
    text-gray-700
  ",
  )
}

@external(javascript, "../lib/ffi.mjs", "openOptionsPage")
fn open_options_page() -> a
