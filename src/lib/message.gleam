import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/option.{type Option, None, Some}

pub type Message {
  Message(
    content: Dynamic,
    origin: MessageContext,
    destination: Option(MessageContext),
  )
}

pub type MessageError {
  DecodeError
}

pub type MessageContext {
  Background
  Popup
  Options
  Sidepanel
  ContentScript(tab_id: Int)
}

pub fn context_to_string(context context: MessageContext) -> String {
  case context {
    Background -> "background"
    Popup -> "popup"
    Options -> "options"
    Sidepanel -> "sidepanel"
    ContentScript(tab_id) -> "content-script@" <> int.to_string(tab_id)
  }
}

pub fn new(
  content content: Dynamic,
  origin origin: MessageContext,
  destination destination: Option(MessageContext),
) -> Message {
  Message(content, origin, destination)
}

pub fn send(message message: Message, action action: String) -> a {
  case message.destination {
    None -> do_send(message.content, "background")
    Some(ContentScript(tab_id)) ->
      do_send_to_tab(tab_id, message.content, action)
    _ -> do_send(message.content, action)
  }
}

pub fn listen(action action: String, callback callback: fn(Dynamic) -> a) -> a {
  do_listen(action, callback)
}

@external(javascript, "../ffi.mjs", "listenMessage")
fn do_listen(action action: String, callback callback: fn(Dynamic) -> a) -> a

@external(javascript, "../ffi.mjs", "sendMessage")
fn do_send(message message: Dynamic, action action: String) -> a

@external(javascript, "../ffi.mjs", "sendMessageToTab")
fn do_send_to_tab(
  tab_id: Int,
  message message: Dynamic,
  action action: String,
) -> a
