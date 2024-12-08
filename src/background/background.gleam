import gleam/io
import lib/message.{type Message}

pub fn main() {
  io.debug("Hello from the background.js!")

  message.on("open_options_page", fn(message: Message) {
    io.debug("Received a message:")
    io.debug(message.content)
    Nil
  })
}
