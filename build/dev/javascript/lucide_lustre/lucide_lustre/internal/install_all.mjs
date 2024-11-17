import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $gleamyshell from "../../../gleamyshell/gleamyshell.mjs";
import * as $globlin from "../../../globlin/globlin.mjs";
import * as $html_lustre_converter from "../../../html_lustre_converter/html_lustre_converter.mjs";
import * as $deno_polyfill from "../../../javascript_dom_parser/javascript_dom_parser/deno_polyfill.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import { toList, makeError } from "../../gleam.mjs";
import * as $config from "../../lucide_lustre/internal/config.mjs";

export function install_all(config) {
  $io.println("Cloning icons locally...");
  let $ = $gleamyshell.execute(
    "git",
    ".",
    toList(["clone", "https://github.com/lucide-icons/lucide.git"]),
  );
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "lucide_lustre/internal/install_all",
      16,
      "install_all",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  $deno_polyfill.install_polyfill();
  let $1 = $globlin.new_pattern("./lucide/icons/*.svg");
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "lucide_lustre/internal/install_all",
      23,
      "install_all",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let pattern = $1[0];
  let $2 = $simplifile.get_files("./lucide/icons");
  if (!$2.isOk()) {
    throw makeError(
      "let_assert",
      "lucide_lustre/internal/install_all",
      24,
      "install_all",
      "Pattern match failed, no pattern matched the value.",
      { value: $2 }
    )
  }
  let icons_folder = $2[0];
  $io.println("Deleting old icons...");
  let $3 = $simplifile.delete$(("./src/" + config.output_module) + ".gleam");
  
  $io.println("Generating new icons...");
  let $4 = (() => {
    let _pipe = icons_folder;
    let _pipe$1 = $list.filter(
      _pipe,
      (_capture) => { return $globlin.match_pattern(pattern, _capture); },
    );
    let _pipe$2 = $list.map(
      _pipe$1,
      (path) => { return [path, $simplifile.read(path)]; },
    );
    let _pipe$3 = $list.map(
      _pipe$2,
      (file) => {
        let $5 = file[1];
        if (!$5.isOk()) {
          throw makeError(
            "let_assert",
            "lucide_lustre/internal/install_all",
            35,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $5 }
          )
        }
        let svg = $5[0];
        let name = (() => {
          let $6 = file[0];
          if ($6 === "./lucide/icons/type.svg") {
            return "type_";
          } else if ($6 === "./lucide/icons/import.svg") {
            return "import_";
          } else if ($6.startsWith("./lucide/icons/")) {
            let path = $6.slice(15);
            let _pipe$3 = $string.split(path, ".");
            let _pipe$4 = $list.first(_pipe$3);
            let _pipe$5 = $result.unwrap(_pipe$4, "");
            return $string.replace(_pipe$5, "-", "_");
          } else {
            return "";
          }
        })();
        return ((("pub fn " + name) + "(attributes: List(Attribute(a))) {") + $html_lustre_converter.convert(
          svg,
        )) + "}";
      },
    );
    let _pipe$4 = $string.join(_pipe$3, "\n");
    return ((contents) => {
      $io.println(
        ("Writing new icons to " + config.output_module) + ".gleam...",
      );
      return $simplifile.write(
        ("./src/" + config.output_module) + ".gleam",
        "import lustre/attribute.{type Attribute, attribute}\nimport lustre/element/svg\n" + $string.replace(
          contents,
          "attribute(\"xmlns\", \"http://www.w3.org/2000/svg\"),",
          "..attributes",
        ),
      );
    })(_pipe$4);
  })();
  
  $io.println("Removing cloned icons...");
  let $5 = $gleamyshell.execute("rm", ".", toList(["-rf", "./lucide"]));
  if (!$5.isOk()) {
    throw makeError(
      "let_assert",
      "lucide_lustre/internal/install_all",
      71,
      "install_all",
      "Pattern match failed, no pattern matched the value.",
      { value: $5 }
    )
  }
  $io.println("Formatting gleam code...");
  let $6 = $gleamyshell.execute("gleam", ".", toList(["format"]));
  if (!$6.isOk()) {
    throw makeError(
      "let_assert",
      "lucide_lustre/internal/install_all",
      76,
      "install_all",
      "Pattern match failed, no pattern matched the value.",
      { value: $6 }
    )
  }
  return $io.println("Finished generating all icons");
}
