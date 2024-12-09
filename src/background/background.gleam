import gleam/io
import lib/extension_utils
import lib/message.{type Message}

pub fn main() {
  io.debug("Hello from background.js!")

  extension_utils.on_installed(fn() {
    io.debug("Extension installed!")
    Nil
  })

  message.on("open_options_page", fn(message: Message) {
    io.debug("Received a message:")
    io.debug(message.content)
    Nil
  })

  message.on("get_current_tab", fn(message: Message) {
    io.debug("Received a message:")
    io.debug(message.content)
    Nil
  })
}
