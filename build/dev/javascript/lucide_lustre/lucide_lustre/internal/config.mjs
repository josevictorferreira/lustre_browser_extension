import * as $argv from "../../../argv/argv.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import * as $tom from "../../../tom/tom.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError } from "../../gleam.mjs";

export class Config extends $CustomType {
  constructor(output_module, output_type) {
    super();
    this.output_module = output_module;
    this.output_type = output_type;
  }
}

export class Single extends $CustomType {}

export class Multiple extends $CustomType {}

function output_type_to_type(output_type) {
  let $ = $string.lowercase(output_type);
  if ($ === "single") {
    return new Single();
  } else if ($ === "multiple") {
    return new Multiple();
  } else {
    $io.println(
      ("Invalid output_type found " + output_type) + " expected, single or multiple",
    );
    throw makeError(
      "panic",
      "lucide_lustre/internal/config",
      82,
      "output_type_to_type",
      "`panic` expression evaluated.",
      {}
    )
  }
}

export function get_config(ignore_first_arg) {
  let $ = $simplifile.read("./gleam.toml");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "lucide_lustre/internal/config",
      17,
      "get_config",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let gleam_toml_raw = $[0];
  let $1 = $tom.parse(gleam_toml_raw);
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "lucide_lustre/internal/config",
      18,
      "get_config",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let gleam_toml = $1[0];
  let static_output_module = $tom.get_string(
    gleam_toml,
    toList(["lucide_lustre", "output_module"]),
  );
  let static_output_type = $tom.get_string(
    gleam_toml,
    toList(["lucide_lustre", "output_type"]),
  );
  let args = $argv.load().arguments;
  let arg_output_module = (() => {
    if (ignore_first_arg) {
      if (args.atLeastLength(2)) {
        let output_module = args.tail.head;
        return new Ok(output_module);
      } else {
        return new Error(undefined);
      }
    } else {
      if (args.atLeastLength(1)) {
        let output_module = args.head;
        return new Ok(output_module);
      } else {
        return new Error(undefined);
      }
    }
  })();
  let arg_output_type = (() => {
    if (ignore_first_arg) {
      if (args.atLeastLength(3)) {
        let output_type = args.tail.tail.head;
        return new Ok(output_type);
      } else {
        return new Error(undefined);
      }
    } else {
      if (args.atLeastLength(2)) {
        let output_type = args.tail.head;
        return new Ok(output_type);
      } else {
        return new Error(undefined);
      }
    }
  })();
  return new Config(
    (() => {
      if (arg_output_module.isOk()) {
        let output_module = arg_output_module[0];
        return output_module;
      } else {
        if (static_output_module.isOk()) {
          let output_module = static_output_module[0];
          return output_module;
        } else {
          return "lucide_lustre";
        }
      }
    })(),
    output_type_to_type(
      (() => {
        if (arg_output_type.isOk()) {
          let output_type = arg_output_type[0];
          return output_type;
        } else {
          if (static_output_type.isOk()) {
            let output_type = static_output_type[0];
            return output_type;
          } else {
            return "single";
          }
        }
      })(),
    ),
  );
}
