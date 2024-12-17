import components/lucide_icons
import components/shared_subtitle
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/result
import lib/message
import lustre
import lustre/attribute
import lustre/element/html
import lustre/event

pub fn main(element_id: String) {
  let app = lustre.simple(init, update, view)

  message.listen("tab_prev", fn(message: Dynamic) {
    let message_content = message |> dynamic.string |> result.unwrap("ERROR")
    io.debug(
      "[lustre-browser-extension] Navigate from page \""
      <> message_content
      <> "\"",
    )
  })

  case lustre.start(app, element_id, Nil) {
    Ok(_) -> {
      io.debug("Lustre content script started successfully")
    }
    Error(_e) -> {
      io.debug("Error starting Lustre content script app")
    }
  }
}

pub type ContentScriptDefaults {
  PopupDefaults(page_name: String, is_visible: Bool)
}

fn init(_flags) {
  PopupDefaults(page_name: "Content Script", is_visible: False)
}

type Msg {
  UserToggledVisibility
}

fn update(model, msg) {
  case msg {
    UserToggledVisibility -> {
      PopupDefaults(..model, is_visible: !model.is_visible)
    }
  }
}

fn view(model: ContentScriptDefaults) {
  html.div([main_container_classes()], [
    html.div([top_container_classes(), visibility_classes(model.is_visible)], [
      html.h1([attribute.class("text-lg")], [html.text("Lustre Web Extension")]),
      shared_subtitle.view(model.page_name),
    ]),
    html.button([button_classes(), event.on_click(UserToggledVisibility)], [
      lucide_icons.cable([]),
    ]),
  ])
}

fn visibility_classes(is_visible: Bool) {
  case is_visible {
    True -> attribute.class("opacity-100")
    False -> attribute.class("opacity-0")
  }
}

fn main_container_classes() {
  attribute.class(
    "fixed right-0 bottom-0 m-5 z-100 flex items-end font-sans select-none leading-1em",
  )
}

fn top_container_classes() {
  attribute.class(
    "bg-white text-gray-800 rounded-lg shadow w-max h-min px-4 py-2 my-auto mr-2 transition-opacity duration-300",
  )
}

fn button_classes() {
  attribute.class(
    "flex w-10 h-10 rounded-full shadow cursor-pointer text-gray-200 border-none items-center justify-center bg-teal-600 hover:bg-teal-700",
  )
}
