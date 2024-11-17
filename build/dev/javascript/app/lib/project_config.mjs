import * as $dot from "../../dot_env/dot_env.mjs";
import * as $env from "../../dot_env/dot_env/env.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import * as $tom from "../../tom/tom.mjs";
import { toList, makeError } from "../gleam.mjs";

function load_dotenv() {
  let _pipe = $dot.new$();
  let _pipe$1 = $dot.set_path(_pipe, "../../.env");
  let _pipe$2 = $dot.set_debug(_pipe$1, false);
  return $dot.load(_pipe$2);
}

export function is_firefox() {
  load_dotenv();
  let $ = $env.get_string("EXTENSION");
  if ($.isOk()) {
    let extension = $[0];
    return extension === "firefox";
  } else {
    return false;
  }
}

export function is_dev() {
  load_dotenv();
  let $ = $env.get_string("GLEAM_ENV");
  if ($.isOk()) {
    let env = $[0];
    return env === "development";
  } else {
    return false;
  }
}

export function get_port() {
  load_dotenv();
  let $ = $env.get_int("PORT");
  if ($.isOk()) {
    let port = $[0];
    return port;
  } else {
    return 1234;
  }
}

function get_string_property(config, keys) {
  let $ = $tom.get_string(config, keys);
  if ($.isOk()) {
    let version$1 = $[0];
    return version$1;
  } else {
    $io.print_error("Could not read property from gleam.toml\n");
    return "";
  }
}

function load_config_file() {
  let filepath = "./gleam.toml";
  let $ = $simplifile.read(filepath);
  if ($.isOk()) {
    let contents = $[0];
    return contents;
  } else {
    $io.print_error("Could not read gleam.toml\n");
    return "";
  }
}

function parsed_config() {
  let config_content = load_config_file();
  let $ = $tom.parse(config_content);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "lib/project_config",
      69,
      "parsed_config",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let parsed = $[0];
  return parsed;
}

export function name() {
  let _pipe = parsed_config();
  return get_string_property(_pipe, toList(["name"]));
}

export function version() {
  let _pipe = parsed_config();
  return get_string_property(_pipe, toList(["version"]));
}

export function description() {
  let _pipe = parsed_config();
  return get_string_property(_pipe, toList(["description"]));
}
