import gleam/dynamic.{type DecodeError}
import gleam/io
import gleam/javascript/promise
import gleam/option.{None, Some}
import lib/extension
import lib/message.{type Message}
import lib/tab.{type Tab}
import ref

pub fn main() {
  let prev_tab_id = ref.cell(None)

  extension.on_installed(fn() {
    io.debug("Extension installed")
    Nil
  })

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
          tab.title
          |> io.debug
          |> Ok
        })
      }
      None -> Ok(io.debug("No previous tab")) |> promise.resolve
    }

    ref.set(prev_tab_id, fn(_) { current_tab_id })

    Nil
  })

  message.on("open-options-page", fn(message: Message) {
    io.debug("Received a message:")
    io.debug(message.content)
    Nil
  })

  message.on("get-current-tab", fn(message: Message) {
    io.debug("Received a message:")
    io.debug(message.content)
    Nil
  })
}
