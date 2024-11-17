import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import { try$ } from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import * as $env from "./dot_env/env.mjs";
import * as $parser from "./dot_env/internal/parser.mjs";
import { Ok, Error, CustomType as $CustomType } from "./gleam.mjs";

export class Opts extends $CustomType {
  constructor(path, debug, capitalize, ignore_missing_file) {
    super();
    this.path = path;
    this.debug = debug;
    this.capitalize = capitalize;
    this.ignore_missing_file = ignore_missing_file;
  }
}

export class Default extends $CustomType {}

class DotEnv extends $CustomType {
  constructor(path, debug, capitalize, ignore_missing_file) {
    super();
    this.path = path;
    this.debug = debug;
    this.capitalize = capitalize;
    this.ignore_missing_file = ignore_missing_file;
  }
}

export function set_debug(instance, debug) {
  return instance.withFields({ debug: debug });
}

export function set_capitalize(instance, capitalize) {
  return instance.withFields({ capitalize: capitalize });
}

export function set_ignore_missing_file(instance, ignore_missing_file) {
  return instance.withFields({ ignore_missing_file: ignore_missing_file });
}

export function set_path(instance, path) {
  return instance.withFields({ path: path });
}

export function path(instance) {
  return instance.path;
}

function handle_file_result(res, ignore_error) {
  return $bool.guard(
    $result.is_error(res) && ignore_error,
    new Ok(""),
    () => { return res; },
  );
}

function set_env(config, pair) {
  let key = $bool.guard(
    !config.capitalize,
    pair[0],
    () => { return $string.uppercase(pair[0]); },
  );
  let _pipe = key;
  return $env.set(_pipe, pair[1]);
}

function recursively_set_environment_variables(config, kv_pairs) {
  if (kv_pairs.hasLength(0)) {
    return new Ok(undefined);
  } else if (kv_pairs.hasLength(1)) {
    let pair = kv_pairs.head;
    return set_env(config, pair);
  } else {
    let pair = kv_pairs.head;
    let rest = kv_pairs.tail;
    return $result.try$(
      set_env(config, pair),
      (_) => { return recursively_set_environment_variables(config, rest); },
    );
  }
}

function read_file(dotenv) {
  return $result.try$(
    (() => {
      let _pipe = $simplifile.is_file(dotenv.path);
      return $result.map_error(
        _pipe,
        (_) => {
          return "Failed to access file, ensure the file exists and is a readable file";
        },
      );
    })(),
    (is_file) => {
      return $bool.guard(
        !is_file,
        new Error(("Specified file at `" + dotenv.path) + "` does not exist"),
        () => {
          return $result.try$(
            (() => {
              let _pipe = $simplifile.read(dotenv.path);
              return $result.map_error(
                _pipe,
                (_) => {
                  return ("Unable to read file at `" + dotenv.path) + "`, ensure the file exists and is readable";
                },
              );
            })(),
            (contents) => { return new Ok(contents); },
          );
        },
      );
    },
  );
}

function load_and_return_error(dotenv) {
  return try$(
    (() => {
      let _pipe = read_file(dotenv);
      return handle_file_result(_pipe, dotenv.ignore_missing_file);
    })(),
    (content) => {
      return try$(
        $parser.parse(content),
        (kv_pairs) => {
          let _pipe = dotenv;
          return recursively_set_environment_variables(_pipe, kv_pairs);
        },
      );
    },
  );
}

export const default$ = /* @__PURE__ */ new DotEnv(".env", true, true, true);

export function new$() {
  return default$;
}

export function new_with_path(path) {
  return default$.withFields({ path: path });
}

export function load_with_opts(opts) {
  let dotenv = (() => {
    if (opts instanceof Opts) {
      let path$1 = opts.path;
      let debug = opts.debug;
      let capitalize = opts.capitalize;
      let ignore_missing_file = opts.ignore_missing_file;
      return new DotEnv(path$1, debug, capitalize, ignore_missing_file);
    } else {
      return default$;
    }
  })();
  let state = (() => {
    let _pipe = dotenv;
    return load_and_return_error(_pipe);
  })();
  if (state.isOk()) {
    return undefined;
  } else {
    let msg = state[0];
    return $bool.guard(
      !dotenv.debug,
      undefined,
      () => { return $io.println_error(msg); },
    );
  }
}

export function load(dotenv) {
  return load_with_opts(
    new Opts(
      dotenv.path,
      dotenv.debug,
      dotenv.capitalize,
      dotenv.ignore_missing_file,
    ),
  );
}

export function load_default() {
  return load_with_opts(new Default());
}
