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
  extension.on_installed(fn() { io.debug("Extension installed") })

  handle_tab_activation()

  message.listen("open_options_page", open_options_page_listener)
}

fn handle_tab_activation() {
  let prev_tab_id = ref.cell(None)

  tab.on_activated(fn(tab_activation) {
    let current_tab_id = case tab_activation {
      Ok(tab) -> Some(tab.tab_id)
      _ -> None
    }

    let prev_id = case ref.get(prev_tab_id) {
      Some(prev_id) -> Some(prev_id)
      None -> None
    }

    case prev_id {
      Some(prev_id) -> {
        prev_id
        |> tab.get_by_id()
        |> promise.map(fn(tab_result: Result(Tab, tab.TabError)) {
          case tab_result {
            Ok(previous_tab) -> {
              previous_tab.title
              |> dynamic.from
              |> send_to_previous_tab(current_tab_id |> option.unwrap(0))
              |> Ok
            }
            Error(_tab_error) -> io.debug("Tab not found") |> Error
          }
        })
        |> promise.rescue(fn(_) { Error(io.debug("Cant get tab")) })
      }
      None -> {
        Error(io.debug("No previous tab id")) |> promise.resolve
      }
    }

    ref.set(prev_tab_id, fn(_) { current_tab_id })
  })
}

fn send_to_previous_tab(content: Dynamic, tab_id: Int) {
  content
  |> message.new(
    origin: message.Background,
    destination: Some(message.ContentScript(tab_id)),
  )
  |> message.send("tab_prev")
}

fn open_options_page_listener(message_data: Dynamic) {
  let message_string = dynamic.string(message_data) |> result.unwrap("Empty")
  io.debug("[open_options_page] Received a message: " <> message_string)
}
