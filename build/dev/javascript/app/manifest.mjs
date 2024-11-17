import * as $json from "../gleam_json/gleam/json.mjs";
import { array, bool, int, object, preprocessed_array, string } from "../gleam_json/gleam/json.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import { toList } from "./gleam.mjs";
import * as $project_config from "./lib/project_config.mjs";

function get_action_object() {
  return object(
    toList([
      ["default_icon", string("./assets/icon-512.png")],
      ["default_popup", string("./dist/popup/index.html")],
    ]),
  );
}

function get_options_ui() {
  return object(
    toList([
      ["page", string("./dist/options/index.html")],
      ["open_in_tab", bool(true)],
    ]),
  );
}

function get_background() {
  let $ = $project_config.is_firefox();
  if ($) {
    return object(
      toList([
        ["scripts", array(toList(["./dist/background/index.mjs"]), string)],
        ["type", string("module")],
      ]),
    );
  } else {
    return object(
      toList([
        [
          "service_worker",
          array(toList(["./dist/background/index.mjs"]), string),
        ],
      ]),
    );
  }
}

function get_icons() {
  return object(
    toList([
      ["16", string("./assets/icon-512.png")],
      ["48", string("./assets/icon-512.png")],
      ["128", string("./assets/icon-512.png")],
    ]),
  );
}

function get_permissions() {
  return array(toList(["storage", "tabs", "activeTab", "sidePanel"]), string);
}

function get_content_scripts() {
  return preprocessed_array(
    toList([
      object(
        toList([
          ["matches", array(toList(["<all_urls>"]), string)],
          ["js", array(toList(["./dist/content/index.mjs"]), string)],
        ]),
      ),
    ]),
  );
}

function get_web_accessible_resources() {
  return preprocessed_array(
    toList([
      object(
        toList([
          ["resources", array(toList(["./dist/content/style.css"]), string)],
          ["matches", array(toList(["<all_urls>"]), string)],
        ]),
      ),
    ]),
  );
}

function get_security_policy() {
  let extension_pages = (() => {
    let $ = $project_config.is_dev();
    if ($) {
      return string(
        ("script-src 'self' http://localhost:" + $int.to_string(
          $project_config.get_port(),
        )) + "; object-src 'self'",
      );
    } else {
      return string("script-src 'self'; object-src 'self'");
    }
  })();
  return object(toList([["extension_pages", extension_pages]]));
}

function get_firefox_specific_options() {
  return toList([
    [
      "sidebar_action",
      object(
        toList([
          ["default_panel", string("./dist/sidepanel/index.html")],
          ["default_icon", string("./assets/icon-512.png")],
        ]),
      ),
    ],
  ]);
}

function get_chrome_specific_options() {
  return toList([
    [
      "side_panel",
      object(toList([["default_path", string("./dist/sidepanel/index.html")]])),
    ],
  ]);
}

function get_manifest_json() {
  let manifest_options = toList([
    ["manifest_version", int(3)],
    ["name", string($project_config.name())],
    ["version", string($project_config.version())],
    ["description", string($project_config.description())],
    ["action", get_action_object()],
    ["options_ui", get_options_ui()],
    ["background", get_background()],
    ["icons", get_icons()],
    ["permissions", get_permissions()],
    ["host_permissions", array(toList(["*://*/*"]), string)],
    ["content_scripts", get_content_scripts()],
    ["web_accessible_resources", get_web_accessible_resources()],
    ["content_security_policy", get_security_policy()],
  ]);
  let full_browser_options = (() => {
    let $ = $project_config.is_firefox();
    if ($) {
      return $list.append(manifest_options, get_firefox_specific_options());
    } else {
      return $list.append(manifest_options, get_chrome_specific_options());
    }
  })();
  return object(full_browser_options);
}

export function get_manifest_json_string() {
  let _pipe = get_manifest_json();
  return $json.to_string(_pipe);
}
