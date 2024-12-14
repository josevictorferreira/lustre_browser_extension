import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/javascript/promise.{type Promise}

pub type Tab {
  Tab(title: String)
}

pub type TabActivation {
  TabActivation(tab_id: Int, window_id: Int)
}

pub type GetTabError {
  TabNotFound
  DecodeError(String)
}

pub fn get_by_id(id: Int) -> Promise(Result(Tab, GetTabError)) {
  do_get_by_id(id)
  |> promise.await(fn(data) {
    case decode_tab(data) {
      Ok(tab) -> Ok(tab) |> promise.resolve
      Error(_) -> Error(DecodeError("Failed to decode tab")) |> promise.resolve
    }
  })
  |> promise.rescue(fn(_error) { Error(TabNotFound) })
}

pub fn on_activated(
  callback: fn(Result(TabActivation, List(dynamic.DecodeError))) -> Nil,
) -> Nil {
  do_on_activated(fn(data) {
    case decode_tab_activation(data) {
      Ok(tab_activation) -> callback(Ok(tab_activation))
      Error(errors) -> callback(Error(errors))
    }
  })
}

fn decode_tab(data: Dynamic) -> Result(Tab, List(DecodeError)) {
  data
  |> dynamic.decode1(Tab, dynamic.field("title", dynamic.string))
}

fn decode_tab_activation(
  data: Dynamic,
) -> Result(TabActivation, List(dynamic.DecodeError)) {
  data
  |> dynamic.decode2(
    TabActivation,
    dynamic.field("tabId", dynamic.int),
    dynamic.field("windowId", dynamic.int),
  )
}

@external(javascript, "../ffi.mjs", "tabsOnActivated")
fn do_on_activated(callback callback: fn(Dynamic) -> Nil) -> Nil

@external(javascript, "../ffi.mjs", "getTabById")
fn do_get_by_id(id: Int) -> Promise(Dynamic)
