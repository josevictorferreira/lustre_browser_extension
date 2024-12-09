import components/logo
import components/shared_subtitle
import gleam/result
import lib/extension_utils
import lib/storage
import lustre
import lustre/attribute
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

pub type PopupDefaults {
  PopupDefaults(page_name: String, shared_input: String)
}

fn init(_flags) {
  let shared_input = storage.get_item("shared_input") |> result.unwrap("")

  PopupDefaults(page_name: "Sidepanel", shared_input: shared_input)
}

type Msg {
  UserClickedOpenOptionPage
}

fn update(model, msg) {
  case msg {
    UserClickedOpenOptionPage -> {
      extension_utils.open_options_page()
      model
    }
  }
}

fn view(model: PopupDefaults) {
  html.main([main_container_classes()], [
    html.div([], [html.text(model.page_name)]),
    logo.view([]),
    shared_subtitle.view(model.page_name),
    html.button(
      [attribute.class("btn mt-2"), event.on_click(UserClickedOpenOptionPage)],
      [html.text("Open Options")],
    ),
    shared_input_view(model.shared_input),
  ])
}

fn main_container_classes() {
  attribute.class(
    "
    w-full
    px-4
    py-5
    text-center
    text-gray-800
    dark:text-gray-200
    dark:bg-gray-900
  ",
  )
}

fn shared_input_view(shared_input) {
  html.div([attribute.class("mt-2")], [
    html.span([attribute.class("opacity-50")], [
      html.text("Shared input: " <> shared_input),
    ]),
  ])
}
