import components/logo
import components/shared_subtitle
import gleam/result
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

pub type OptionPageDefaults {
  OptionPageDefaults(page_name: String, shared_input: String)
}

fn init(_flags) {
  let shared_input = storage.get_item("shared_input") |> result.unwrap("")

  OptionPageDefaults(page_name: "Options", shared_input: shared_input)
}

type Msg {
  UserTypedInput(String)
}

fn update(model, msg) {
  case msg {
    UserTypedInput(input) -> {
      storage.set_item("shared_input", input)
      OptionPageDefaults(..model, shared_input: input)
    }
  }
}

fn view(model: OptionPageDefaults) {
  html.main([main_classes()], [
    html.div([], [html.text(model.page_name)]),
    logo.view([]),
    shared_subtitle.view(model.page_name),
    shared_input(model.shared_input),
  ])
}

fn shared_input(shared_input) {
  html.input([
    attribute.class(
      "border border-gray-400 rounded px-2 py-1 mt-2 text-gray-800",
    ),
    attribute.value(shared_input),
    event.on_input(fn(value: String) { UserTypedInput(value) }),
  ])
}

fn main_classes() {
  attribute.class(
    "
    px-4 py-10 text-center text-gray-800 dark:text-gray-200 dark:bg-gray-900
  ",
  )
}
