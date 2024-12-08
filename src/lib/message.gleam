pub type Message {
  Message(content: String, sender: String)
}

pub fn new(content content: String, sender sender: String) -> Message {
  Message(content, sender)
}

@external(javascript, "../ffi.mjs", "onMessage")
pub fn on(address address: String, callback callback: fn(Message) -> Nil) -> Nil

@external(javascript, "../ffi.mjs", "postMessage")
pub fn post(message message: Message, to to: String) -> Nil
