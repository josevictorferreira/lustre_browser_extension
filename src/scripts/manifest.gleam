import gleam/int
import gleam/io
import gleam/json.{
  type Json, array, bool, int, object, preprocessed_array, string,
}
import gleam/list
import lib/file_utils
import lib/project_config
import simplifile

pub fn main() {
  io.debug("Generating manifest.json")
  let extension_dir = "./extension/"
  file_utils.ensure_dir(extension_dir)
  let filepath = extension_dir <> "manifest.json"
  let assert Ok(_) =
    get_manifest_json_string()
    |> simplifile.write(to: filepath)
  io.debug("Manifest.json generated")
}

pub fn get_manifest_json_string() -> String {
  get_manifest_json()
  |> json.to_string
}

pub fn get_manifest_json() -> Json {
  let manifest_options = [
    #("manifest_version", int(3)),
    #("name", string(project_config.name())),
    #("version", string(project_config.version())),
    #("description", string(project_config.description())),
    #("action", get_action_object()),
    #("options_ui", get_options_ui()),
    #("background", get_background()),
    #("icons", get_icons()),
    #("permissions", get_permissions()),
    #("host_permissions", array(["*://*/*"], of: string)),
    #("content_scripts", get_content_scripts()),
    #("web_accessible_resources", get_web_accessible_resources()),
    #("content_security_policy", get_security_policy()),
  ]

  let full_browser_options = case project_config.is_firefox() {
    True -> list.append(manifest_options, get_firefox_specific_options())
    False -> list.append(manifest_options, get_chrome_specific_options())
  }

  object(full_browser_options)
}

fn get_action_object() -> Json {
  object([
    #("default_icon", string("./assets/icons/icon_128.png")),
    #("default_popup", string("./dist/popup/index.html")),
  ])
}

fn get_options_ui() -> Json {
  object([
    #("page", string("./dist/options/index.html")),
    #("open_in_tab", bool(True)),
  ])
}

fn get_background() -> Json {
  case project_config.is_firefox() {
    True ->
      object([
        #("scripts", array(["./dist/background/index.mjs"], of: string)),
        #("type", string("module")),
      ])
    False ->
      object([
        #("service_worker", array(["./dist/background/index.mjs"], of: string)),
      ])
  }
}

fn get_icons() -> Json {
  object([
    #("16", string("./assets/icons/icon_16.png")),
    #("32", string("./assets/icons/icon_32.png")),
    #("48", string("./assets/icons/icon_48.png")),
    #("128", string("./assets/icons/icon_128.png")),
  ])
}

fn get_permissions() -> Json {
  array(["storage", "tabs", "activeTab", "sidePanel"], of: string)
}

fn get_content_scripts() -> Json {
  preprocessed_array([
    object([
      #("matches", array(["<all_urls>"], of: string)),
      #("js", array(["./dist/content_scripts/index.mjs"], of: string)),
    ]),
  ])
}

fn get_web_accessible_resources() -> Json {
  preprocessed_array([
    object([
      #("resources", array(["./dist/content_scripts/style.css"], of: string)),
      #("matches", array(["<all_urls>"], of: string)),
    ]),
  ])
}

fn get_security_policy() -> Json {
  let extension_pages = case project_config.is_dev() {
    True ->
      string(
        "script-src 'self' http://localhost:"
        <> int.to_string(project_config.get_port())
        <> "; object-src 'self'",
      )
    False -> string("script-src 'self'; object-src 'self'")
  }

  object([#("extension_pages", extension_pages)])
}

fn get_firefox_specific_options() {
  [
    #(
      "sidebar_action",
      object([
        #("default_panel", string("./dist/sidepanel/index.html")),
        #("default_icon", string("./assets/icons/icon_128.png")),
      ]),
    ),
  ]
}

fn get_chrome_specific_options() {
  [
    #(
      "side_panel",
      object([#("default_path", string("./dist/sidepanel/index.html"))]),
    ),
  ]
}
