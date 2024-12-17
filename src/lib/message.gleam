import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}

pub type Message {
  Message(
    content: Dynamic,
    origin: MessageContext,
    destiny: Option(MessageContext),
  )
}

pub type MessageContext {
  Background
  Popup
  Options
  Sidepanel
  ContentScript(tab_id: Int)
}

pub fn new(
  content content: Dynamic,
  origin origin: MessageContext,
  destiny destiny: Option(MessageContext),
) -> Message {
  Message(content, origin, destiny)
}

pub fn send(message message: Message, message_id message_id: String) -> a {
  case message.destiny {
    None -> do_send(message.content, message_id)
    Some(context) ->
      case context {
        ContentScript(tab_id) -> {
          io.debug("Sending message to tab: " <> tab_id |> int.to_string)
          do_send_to_tab(message.content, tab_id, message_id)
        }
        _ -> do_send(message.content, message_id)
      }
  }
}

pub fn listen(
  message_id message_id: String,
  callback callback: fn(Dynamic) -> a,
) -> a {
  do_listen(message_id, callback)
}

@external(javascript, "../ffi.mjs", "listenMessage")
fn do_listen(
  message_id message_id: String,
  callback callback: fn(Dynamic) -> a,
) -> a

@external(javascript, "../ffi.mjs", "sendMessage")
fn do_send(message message: Dynamic, message_id message_id: String) -> a

@external(javascript, "../ffi.mjs", "sendMessageToTab")
fn do_send_to_tab(
  message message: Dynamic,
  tab_id tab_id: Int,
  message_id message_id: String,
) -> a
