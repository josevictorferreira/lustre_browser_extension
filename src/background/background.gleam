import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/javascript/promise
import gleam/option.{None, Some}
import gleam/result
import lib/extension
import lib/message
import lib/tab.{type Tab}
import ref

pub fn main() {
  let prev_tab_id = ref.cell(None)

  extension.on_installed(fn() { io.debug("Extension installed") })

  tab.on_activated(fn(tab_activation) {
    let current_tab_id = case tab_activation {
      Ok(activation_tab) -> Some(activation_tab.tab_id)
      _ -> None
    }

    case ref.get(prev_tab_id) {
      Some(prev_id) -> {
        prev_id
        |> tab.get_by_id()
        |> promise.map_try(fn(tab: Tab) {
          tab.title |> dynamic.from |> send_to_previous_tab(prev_id) |> Ok
        })
        |> promise.rescue(fn(_e) { Error(tab.NotFound) })
      }
      None -> Error(tab.NotFound) |> promise.resolve
    }

    ref.set(prev_tab_id, fn(_) { current_tab_id })
  })

  message.listen("open_options_page", fn(content: Dynamic) {
    io.debug(
      "Received a message: "
      <> dynamic.string(content) |> result.unwrap("Empty"),
    )
  })
}

fn send_to_previous_tab(content: Dynamic, tab_id: Int) {
  content
  |> message.new(
    origin: message.Background,
    destiny: Some(message.ContentScript(tab_id)),
  )
  |> message.send("tab_prev")
}
