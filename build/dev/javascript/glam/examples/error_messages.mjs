import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $doc from "../glam/doc.mjs";
import { toList, CustomType as $CustomType, makeError } from "../gleam.mjs";

export class Span extends $CustomType {
  constructor(line, column_start, column_end) {
    super();
    this.line = line;
    this.column_start = column_start;
    this.column_end = column_end;
  }
}

export class Error extends $CustomType {
  constructor(code, name, message, span) {
    super();
    this.code = code;
    this.name = name;
    this.message = message;
    this.span = span;
  }
}

function header_doc(code, name) {
  let _pipe = toList(["[", code, "]:", name]);
  let _pipe$1 = $string.join(_pipe, " ");
  return $doc.from_string(_pipe$1);
}

function line_prefix(line_number) {
  return $int.to_string(line_number + 1) + " | ";
}

function line_doc(source_code, line_number) {
  let $ = $dict.get(source_code, line_number);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "examples/error_messages",
      92,
      "line_doc",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let line = $[0];
  let prefix = line_prefix(line_number);
  let prefix_size = $string.length(prefix);
  return [$doc.from_string(prefix + line), prefix_size];
}

function underlined_pointer(length) {
  let _pipe = toList([
    $doc.from_string("┬" + $string.repeat("─", length - 1)),
    $doc.line,
    $doc.from_string("╰─ "),
  ]);
  return $doc.concat(_pipe);
}

function flexible_text(text) {
  let to_flexible_line = (line) => {
    let _pipe = $string.split(line, " ");
    let _pipe$1 = $list.map(_pipe, $doc.from_string);
    let _pipe$2 = $doc.join(_pipe$1, $doc.flex_space);
    return $doc.group(_pipe$2);
  };
  let _pipe = $string.split(text, "\n");
  let _pipe$1 = $list.map(_pipe, to_flexible_line);
  let _pipe$2 = $doc.join(_pipe$1, $doc.line);
  return $doc.group(_pipe$2);
}

function message_doc(message, length) {
  let _pipe = toList([
    underlined_pointer(length),
    (() => {
      let _pipe = flexible_text(message);
      return $doc.nest(_pipe, 3);
    })(),
  ]);
  return $doc.concat(_pipe);
}

function error_to_doc(source_code, error) {
  let underline_size = (error.span.column_end - error.span.column_start) + 1;
  let $ = line_doc(source_code, error.span.line);
  let line_doc$1 = $[0];
  let prefix_size = $[1];
  let _pipe = toList([
    header_doc(error.code, error.name),
    $doc.line,
    line_doc$1,
    (() => {
      let _pipe = toList([$doc.line, message_doc(error.message, underline_size)]);
      let _pipe$1 = $doc.concat(_pipe);
      return $doc.nest(_pipe$1, error.span.column_start + prefix_size);
    })(),
  ]);
  return $doc.concat(_pipe);
}

export function errors_to_doc(source_code, errors) {
  let source_code$1 = (() => {
    let _pipe = $string.split(source_code, "\n");
    let _pipe$1 = $list.index_map(_pipe, (line, i) => { return [i, line]; });
    return $dict.from_list(_pipe$1);
  })();
  let _pipe = $list.map(
    errors,
    (_capture) => { return error_to_doc(source_code$1, _capture); },
  );
  return $doc.join(_pipe, $doc.lines(2));
}

export function example_errors() {
  return toList([
    new Error(
      "E001",
      "Unused function",
      "This function is unused!\nYou can safely remove it or make it public with the `pub` keyword.",
      new Span(0, 3, 7),
    ),
    new Error(
      "E011",
      "Unknown variable",
      "The name `println` is not in scope here.\nDid you mean to use `io.println`?",
      new Span(1, 2, 8),
    ),
  ]);
}

export const example_source_code = "fn greet(message: String) -> Nil {\n  println(\"Hello\" <> message)\n}";

export function main() {
  let _pipe = errors_to_doc(example_source_code, example_errors());
  let _pipe$1 = $doc.to_string(_pipe, 30);
  return $io.println(_pipe$1);
}
