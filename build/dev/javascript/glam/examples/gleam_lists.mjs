import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $doc from "../glam/doc.mjs";
import { toList } from "../gleam.mjs";

export function pretty_list(list) {
  let list_item_to_document = (item) => {
    return $doc.from_string(("\"" + item) + "\"");
  };
  let comma = $doc.concat(toList([$doc.from_string(","), $doc.space]));
  let open_square = $doc.concat(
    toList([$doc.from_string("["), $doc.soft_break]),
  );
  let trailing_comma = $doc.break$("", ",");
  let close_square = $doc.concat(
    toList([trailing_comma, $doc.from_string("]")]),
  );
  let _pipe = $list.map(list, list_item_to_document);
  let _pipe$1 = $doc.join(_pipe, comma);
  let _pipe$2 = $doc.prepend(_pipe$1, open_square);
  let _pipe$3 = $doc.nest(_pipe$2, 2);
  let _pipe$4 = $doc.append(_pipe$3, close_square);
  return $doc.group(_pipe$4);
}

export function main() {
  return $list.each(
    toList([10, 20, 30]),
    (max_width) => {
      $io.println("Max width: " + $int.to_string(max_width));
      $io.println($string.repeat("-", max_width));
      let _pipe = toList(["Gleam", "is", "fun!"]);
      let _pipe$1 = pretty_list(_pipe);
      let _pipe$2 = $doc.to_string(_pipe$1, max_width);
      $io.println(_pipe$2)
      return $io.println("");
    },
  );
}
