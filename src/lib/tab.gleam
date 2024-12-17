import gleam/dynamic.{type Dynamic}
import gleam/javascript/promise.{type Promise}
import gleam/result

pub type Tab {
  Tab(title: String)
}

pub type TabActivation {
  TabActivation(tab_id: Int, window_id: Int)
}

pub type TabError {
  NotFound
  DecodeError
}

pub fn get_by_id(id: Int) -> Promise(Result(Tab, TabError)) {
  do_get_by_id(id)
  |> promise.map(decode_tab)
}

pub fn on_activated(callback: fn(Result(TabActivation, TabError)) -> a) -> a {
  do_on_activated(fn(data) {
    case decode_tab_activation(data) {
      Ok(tab_activation) -> callback(Ok(tab_activation))
      Error(_errors) -> callback(Error(NotFound))
    }
  })
}

fn decode_tab(data: Dynamic) -> Result(Tab, TabError) {
  data
  |> dynamic.decode1(Tab, dynamic.field("title", dynamic.string))
  |> result.map_error(fn(_) { DecodeError })
}

fn decode_tab_activation(data: Dynamic) -> Result(TabActivation, TabError) {
  data
  |> dynamic.decode2(
    TabActivation,
    dynamic.field("tabId", dynamic.int),
    dynamic.field("windowId", dynamic.int),
  )
  |> result.map_error(fn(_) { DecodeError })
}

@external(javascript, "../ffi.mjs", "tabsOnActivated")
fn do_on_activated(callback callback: fn(Dynamic) -> a) -> a

@external(javascript, "../ffi.mjs", "getTabById")
fn do_get_by_id(id: Int) -> Promise(Dynamic)
