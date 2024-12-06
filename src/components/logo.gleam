import lustre/attribute
import lustre/element/html

pub fn view() {
  html.a(
    [
      attribute.href("https://gleam.run"),
      attribute.class("icon-btn mx-2 text-2xl"),
      attribute.rel("noreferrer"),
      attribute.target("_blank"),
      attribute.title("Gleam"),
    ],
    [
      html.img([
        attribute.src("../assets/logo.svg"),
        attribute.alt("Gleam logo"),
        attribute.class("w-8 h-8"),
      ]),
    ],
  )
}
