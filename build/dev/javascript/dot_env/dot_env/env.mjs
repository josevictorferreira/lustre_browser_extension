import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { set_env as set, get_env as get, get_env as get_string } from "../dot_env_ffi.mjs";
import { Ok, Error } from "../gleam.mjs";

export { get, get_string, set };

export function get_or(key, default$) {
  let _pipe = get_string(key);
  return $result.unwrap(_pipe, default$);
}

export function get_string_or(key, default$) {
  let _pipe = get_string(key);
  return $result.unwrap(_pipe, default$);
}

export function get_then(key, next) {
  let $ = get_string(key);
  if ($.isOk()) {
    let value = $[0];
    return next(value);
  } else {
    let err = $[0];
    return new Error(err);
  }
}

export function get_int(key) {
  return get_then(
    key,
    (raw_value) => {
      let _pipe = $int.parse(raw_value);
      return $result.map_error(
        _pipe,
        (_) => {
          return ("Failed to parse environment variable for `" + key) + "` as integer";
        },
      );
    },
  );
}

export function get_int_or(key, default$) {
  let _pipe = get_int(key);
  return $result.unwrap(_pipe, default$);
}

export function get_bool(key) {
  return get_then(
    key,
    (raw_value) => {
      let $ = $string.lowercase(raw_value);
      if ($ === "true") {
        return new Ok(true);
      } else if ($ === "1") {
        return new Ok(true);
      } else if ($ === "false") {
        return new Ok(false);
      } else if ($ === "0") {
        return new Ok(false);
      } else {
        return new Error(
          ("Invalid boolean value for environment variable `" + key) + "`. Expected one of `true`, `false`, `1`, or `0`.",
        );
      }
    },
  );
}

export function get_bool_or(key, default$) {
  let _pipe = get_bool(key);
  return $result.unwrap(_pipe, default$);
}
