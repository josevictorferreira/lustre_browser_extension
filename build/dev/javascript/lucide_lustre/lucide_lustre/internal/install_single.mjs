import * as $argv from "../../../argv/argv.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $gleamyshell from "../../../gleamyshell/gleamyshell.mjs";
import * as $html_lustre_converter from "../../../html_lustre_converter/html_lustre_converter.mjs";
import * as $deno_polyfill from "../../../javascript_dom_parser/javascript_dom_parser/deno_polyfill.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import { Ok, Error, toList, makeError } from "../../gleam.mjs";
import * as $config from "../../lucide_lustre/internal/config.mjs";

export function install_single(icon_name, config) {
  $io.println(("Cloning " + icon_name) + " locally...");
  let download_path = "lucide_lustre_download";
  let icons_file_path = ("./src/" + config.output_module) + ".gleam";
  $deno_polyfill.install_polyfill();
  let file = (() => {
    let $ = $simplifile.read(icons_file_path);
    if ($.isOk()) {
      let file = $[0];
      return file;
    } else {
      let $1 = $simplifile.write(
        icons_file_path,
        "import lustre/attribute.{type Attribute, attribute}\nimport lustre/element/svg",
      );
      
      return "import lustre/attribute.{type Attribute, attribute}\nimport lustre/element/svg";
    }
  })();
  let name = (() => {
    if (icon_name === "type") {
      return "type_";
    } else if (icon_name === "import") {
      return "import_";
    } else {
      let _pipe = icon_name;
      return $string.replace(_pipe, "-", "_");
    }
  })();
  return $bool.guard(
    (() => {
      let $ = $string.contains(file, "pub fn " + name);
      if ($) {
        $io.println(("Icon " + icon_name) + " is already installed");
        return true;
      } else {
        return false;
      }
    })(),
    new Error(undefined),
    () => {
      let $ = $gleamyshell.execute(
        "curl",
        ".",
        toList([
          "-o",
          download_path,
          ("https://raw.githubusercontent.com/lucide-icons/lucide/refs/heads/main/icons/" + $string.replace(
            icon_name,
            "_",
            "-",
          )) + ".svg",
        ]),
      );
      
      let $1 = $simplifile.read(download_path);
      if (!$1.isOk()) {
        throw makeError(
          "let_assert",
          "lucide_lustre/internal/install_single",
          65,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $1 }
        )
      }
      let svg = $1[0];
      return $bool.guard(
        (() => {
          let $2 = $string.contains(svg, "404: Not Found");
          if ($2) {
            let $3 = $gleamyshell.execute("rm", ".", toList([download_path]));
            if (!$3.isOk()) {
              throw makeError(
                "let_assert",
                "lucide_lustre/internal/install_single",
                70,
                "",
                "Pattern match failed, no pattern matched the value.",
                { value: $3 }
              )
            }
            $io.println(("Icon " + icon_name) + " doesn't exist");
            return true;
          } else {
            return false;
          }
        })(),
        new Error(undefined),
        () => {
          let function$ = ((("pub fn " + name) + "(attributes: List(Attribute(a))) {") + $html_lustre_converter.convert(
            svg,
          )) + "}";
          $io.println(
            ((("Writing " + icon_name) + " to ") + config.output_module) + ".gleam...",
          );
          let $2 = $simplifile.write(
            icons_file_path,
            (file + "\n") + $string.replace(
              function$,
              "attribute(\"xmlns\", \"http://www.w3.org/2000/svg\"),",
              "..attributes",
            ),
          );
          
          $io.println("Removing cloned icon...");
          let $3 = $gleamyshell.execute("rm", ".", toList([download_path]));
          if (!$3.isOk()) {
            throw makeError(
              "let_assert",
              "lucide_lustre/internal/install_single",
              105,
              "",
              "Pattern match failed, no pattern matched the value.",
              { value: $3 }
            )
          }
          $io.println("Formatting gleam code...");
          let $4 = $gleamyshell.execute("gleam", ".", toList(["format"]));
          if (!$4.isOk()) {
            throw makeError(
              "let_assert",
              "lucide_lustre/internal/install_single",
              109,
              "",
              "Pattern match failed, no pattern matched the value.",
              { value: $4 }
            )
          }
          $io.println("Finished generating " + icon_name);
          return new Ok("");
        },
      );
    },
  );
}
