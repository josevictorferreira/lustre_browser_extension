import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $doc from "../glam/doc.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";

export class String extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Number extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Bool extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Null extends $CustomType {}

export class Array extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Object extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function colon() {
  return $doc.from_string(":");
}

function comma() {
  return $doc.from_string(",");
}

function bool_to_doc(bool) {
  let _pipe = $bool.to_string(bool);
  let _pipe$1 = $string.lowercase(_pipe);
  return $doc.from_string(_pipe$1);
}

function parenthesise(doc, open, close) {
  let _pipe = doc;
  let _pipe$1 = $doc.prepend_docs(
    _pipe,
    toList([$doc.from_string(open), $doc.space]),
  );
  let _pipe$2 = $doc.nest(_pipe$1, 2);
  let _pipe$3 = $doc.append_docs(
    _pipe$2,
    toList([$doc.space, $doc.from_string(close)]),
  );
  return $doc.group(_pipe$3);
}

export function example_json() {
  return new Object(
    toList([
      ["title", new String("The sundial")],
      ["author", new String("Shirley Jackson")],
      ["publication_year", new Number(1958.0)],
      ["read", new Bool(true)],
      [
        "characters",
        new Array(
          toList([
            new String("Mrs. Halloran"),
            new String("Essex"),
            new String("Captain Scarabombardon"),
          ]),
        ),
      ],
      ["average_rating", new Number(5.0)],
      [
        "ratings",
        new Array(
          toList([
            new Object(
              toList([["from", new String("Ben")], ["value", new Number(5.0)]]),
            ),
            new Object(
              toList([
                ["from", new String("Giacomo")],
                ["value", new Number(5.0)],
              ]),
            ),
          ]),
        ),
      ],
    ]),
  );
}

function array_to_doc(objects) {
  let _pipe = $list.map(objects, json_to_doc);
  let _pipe$1 = $doc.concat_join(_pipe, toList([comma(), $doc.space]));
  return parenthesise(_pipe$1, "[", "]");
}

export function json_to_doc(json) {
  if (json instanceof String) {
    let string = json[0];
    return $doc.from_string(("\"" + string) + "\"");
  } else if (json instanceof Number) {
    let number = json[0];
    return $doc.from_string($float.to_string(number));
  } else if (json instanceof Bool) {
    let bool = json[0];
    return bool_to_doc(bool);
  } else if (json instanceof Null) {
    return $doc.from_string("null");
  } else if (json instanceof Array) {
    let objects = json[0];
    return array_to_doc(objects);
  } else {
    let fields = json[0];
    return object_to_doc(fields);
  }
}

function field_to_doc(field) {
  let name = field[0];
  let value = field[1];
  let name_doc = $doc.from_string(name);
  let value_doc = json_to_doc(value);
  let _pipe = toList([name_doc, colon(), $doc.from_string(" "), value_doc]);
  return $doc.concat(_pipe);
}

function object_to_doc(fields) {
  let _pipe = $list.map(fields, field_to_doc);
  let _pipe$1 = $doc.concat_join(_pipe, toList([comma(), $doc.space]));
  return parenthesise(_pipe$1, "{", "}");
}

export function main() {
  return $list.each(
    toList([30, 50, 80]),
    (max_width) => {
      $io.println("Max width: " + $int.to_string(max_width));
      $io.println($string.repeat("-", max_width));
      let _pipe = json_to_doc(example_json());
      let _pipe$1 = $doc.to_string(_pipe, max_width);
      $io.println(_pipe$1)
      return $io.println("");
    },
  );
}
