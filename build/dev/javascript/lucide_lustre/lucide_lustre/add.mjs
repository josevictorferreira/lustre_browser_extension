import * as $argv from "../../argv/argv.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $config from "../lucide_lustre/internal/config.mjs";
import * as $install_single from "../lucide_lustre/internal/install_single.mjs";

export function main() {
  let config = $config.get_config(true);
  let $ = $argv.load().arguments;
  if ($.atLeastLength(1)) {
    let icon_name = $.head;
    let $1 = $install_single.install_single(icon_name, config);
    
    return undefined;
  } else {
    return $io.println(
      "No icon name provided, usage: gleam run -m lucide_lustre/add [icon_name] {output_module default: lucide_lustre}",
    );
  }
}
