import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $pair from "../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $shellout from "../shellout/shellout.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import * as $tom from "../tom/tom.mjs";
import { String } from "../tom/tom.mjs";
import { Ok, Error, toList, makeError } from "./gleam.mjs";
import { os_arch, os_platform } from "./tailwind_js.mjs";

function target() {
  let $ = os_platform();
  let $1 = os_arch();
  if ($ === "win32" && $1 === "x86_64") {
    return "windows-x64.exe";
  } else if ($ === "win32" && $1 === "x64") {
    return "windows-x64.exe";
  } else if ($ === "win32" && $1.startsWith("arm")) {
    return "windows-arm64.exe";
  } else if ($ === "darwin" && $1 === "aarch64") {
    return "macos-arm64";
  } else if ($ === "darwin" && $1.startsWith("arm")) {
    return "macos-arm64";
  } else if ($ === "darwin" && $1 === "x86_64") {
    return "macos-x64";
  } else if ($ === "darwin" && $1 === "x64") {
    return "macos-x64";
  } else if ($ === "linux" && $1 === "aarch64") {
    return "linux-arm64";
  } else if ($ === "linux" && $1 === "arm64") {
    return "linux-arm64";
  } else if ($ === "linux" && $1.startsWith("armv7")) {
    return "linux-armv7";
  } else if ($ === "linux" && $1 === "x86_64") {
    return "linux-x64";
  } else if ($ === "linux" && $1 === "x64") {
    return "linux-x64";
  } else if ($ === "linux" && $1 === "amd64") {
    return "linux-x64";
  } else {
    let os = $;
    let arch = $1;
    throw makeError(
      "panic",
      "tailwind",
      183,
      "target",
      $string.join(
        toList(["Error: TailwindCSS CLI is not available for", os, arch]),
        " ",
      ),
      {}
    )
  }
}

const tailwind_config_path = "./tailwind.config.js";

function generate_config() {
  let $ = $simplifile.is_file(tailwind_config_path);
  if ($.isOk() && $[0]) {
    $io.println("TailwindCSS config already exists.");
    return new Ok(undefined);
  } else {
    let _pipe = "\n// See the Tailwind configuration guide for advanced usage\n// https://tailwindcss.com/docs/configuration\n\nlet plugin = require('tailwindcss/plugin')\n\nmodule.exports = {\n  content: ['./src/**/*.{html,gleam}'],\n  theme: {\n    extend: {},\n  },\n  plugins: [require('@tailwindcss/forms')],\n}\n";
    let _pipe$1 = $simplifile.write(tailwind_config_path, _pipe);
    let _pipe$2 = $result.map_error(
      _pipe$1,
      (err) => {
        return "Error: Couldn't create tailwind config. Reason: " + $string.inspect(
          err,
        );
      },
    );
    return $result.map(
      _pipe$2,
      (_) => {
        $io.println("TailwindCSS config created.");
        return undefined;
      },
    );
  }
}

const tailwindcli_path = "./build/bin/tailwindcss-cli";

function download_bin(path) {
  let _pipe = $shellout.command(
    "curl",
    toList(["-sL", "https://github.com" + path, "-o", tailwindcli_path]),
    ".",
    toList([]),
  );
  let _pipe$1 = $result.replace(_pipe, undefined);
  return $result.map_error(
    _pipe$1,
    (err) => {
      return "Error: Couldn't download tailwindcss cli. Reason: " + $pair.second(
        err,
      );
    },
  );
}

function download_tailwind(version, target) {
  let $ = $simplifile.is_file(tailwindcli_path);
  if ($.isOk() && $[0]) {
    $io.println("TailwindCSS CLI already exists.");
    return new Ok(undefined);
  } else {
    let url_path = $string.concat(
      toList([
        "/tailwindlabs/tailwindcss/releases/download/v",
        version,
        "/tailwindcss-",
        target,
      ]),
    );
    let $1 = $simplifile.create_directory_all("./build/bin/");
    if (!$1.isOk() || $1[0]) {
      throw makeError(
        "let_assert",
        "tailwind",
        206,
        "download_tailwind",
        "Pattern match failed, no pattern matched the value.",
        { value: $1 }
      )
    }
    $io.println(("Downloading TailwindCSS " + target) + "...");
    let _pipe = download_bin(url_path);
    return $result.try$(
      _pipe,
      (_) => {
        let _pipe$1 = $simplifile.set_permissions_octal(tailwindcli_path, 0o755);
        return $result.map_error(
          _pipe$1,
          (err) => {
            return "Error: Can't change tailwindcli permissions. Reason: " + $string.inspect(
              err,
            );
          },
        );
      },
    );
  }
}

const config_path = "./gleam.toml";

function get_config() {
  let _pipe = $simplifile.read(config_path);
  let _pipe$1 = $result.map_error(
    _pipe,
    (err) => {
      return "Error: Couldn't read config. Reason: " + $string.inspect(err);
    },
  );
  return $result.try$(
    _pipe$1,
    (config) => {
      let _pipe$2 = $tom.parse(config);
      return $result.replace_error(_pipe$2, "Error: Couldn't parse config.");
    },
  );
}

function get_config_string(key) {
  let _pipe = get_config();
  return $result.try$(
    _pipe,
    (parsed) => {
      let _pipe$1 = $tom.get_string(parsed, toList(["tailwind", key]));
      return $result.replace_error(
        _pipe$1,
        ("Error: Config key \"" + key) + "\" not found.",
      );
    },
  );
}

export function get_args() {
  let _pipe = get_config();
  return $result.try$(
    _pipe,
    (parsed) => {
      let _pipe$1 = $tom.get_array(parsed, toList(["tailwind", "args"]));
      let _pipe$2 = $result.replace_error(
        _pipe$1,
        "Error: Config arguments not found. Is the \"args\" key set in the \"gleam.toml\"?",
      );
      return $result.map(
        _pipe$2,
        (args) => {
          return $list.map(
            args,
            (arg) => {
              if (arg instanceof String) {
                let a = arg[0];
                return a;
              } else {
                return "";
              }
            },
          );
        },
      );
    },
  );
}

function get_cli_path() {
  let _pipe = get_config_string("path");
  return $result.unwrap(_pipe, tailwindcli_path);
}

export function run(args) {
  let cli = get_cli_path();
  let $ = $simplifile.is_file(cli);
  if ($.isOk() && $[0]) {
    let _pipe = $shellout.command(cli, args, ".", toList([]));
    return $result.map_error(_pipe, (err) => { return $pair.second(err); });
  } else {
    return new Error("Error: TailwindCSS CLI isn't installed.");
  }
}

const latest_version = "3.4.1";

function get_tailwind_version() {
  let _pipe = get_config_string("version");
  return $result.unwrap(_pipe, latest_version);
}

export function install() {
  $io.println("Installing TailwindCSS...");
  let output = (() => {
    let _pipe = generate_config();
    return $result.try$(
      _pipe,
      (_) => {
        let version = get_tailwind_version();
        return download_tailwind(version, target());
      },
    );
  })();
  if (output.isOk()) {
    $io.println("TailwindCSS installed!");
    return new Ok(undefined);
  } else {
    let err = output[0];
    $io.println(err);
    return new Error(err);
  }
}

export function install_and_run(args) {
  let _pipe = install();
  return $result.try$(_pipe, (_) => { return run(args); });
}
