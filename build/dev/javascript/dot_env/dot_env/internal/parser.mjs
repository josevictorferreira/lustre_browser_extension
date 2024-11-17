import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { try$ } from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, prepend as listPrepend } from "../../gleam.mjs";

function parse_comment(loop$text, loop$next) {
  while (true) {
    let text = loop$text;
    let next = loop$next;
    if (text.atLeastLength(1) && text.head === "\n") {
      return next(text);
    } else if (text.atLeastLength(1)) {
      let rest = text.tail;
      loop$text = rest;
      loop$next = next;
    } else {
      return next(text);
    }
  }
}

function join(strings) {
  let _pipe = strings;
  let _pipe$1 = $list.reverse(_pipe);
  return $string.join(_pipe$1, "");
}

function parse_key(loop$text, loop$acc) {
  while (true) {
    let text = loop$text;
    let acc = loop$acc;
    if (text.atLeastLength(1) && text.head === "=") {
      let rest = text.tail;
      return new Ok([$string.trim(join(acc)), rest]);
    } else if (text.atLeastLength(1)) {
      let c = text.head;
      let rest = text.tail;
      loop$text = rest;
      loop$acc = listPrepend(c, acc);
    } else {
      return new Error("unexpected end of input");
    }
  }
}

function parse_value_unquoted(loop$text, loop$acc) {
  while (true) {
    let text = loop$text;
    let acc = loop$acc;
    if (text.atLeastLength(1) && text.head === "\n") {
      let rest = text.tail;
      return new Ok([$string.trim(join(acc)), rest]);
    } else if (text.atLeastLength(1) && text.head === "#") {
      let rest = text.tail;
      return parse_comment(
        rest,
        (r) => { return parse_value_unquoted(r, acc); },
      );
    } else if (text.atLeastLength(1)) {
      let c = text.head;
      let rest = text.tail;
      loop$text = rest;
      loop$acc = listPrepend(c, acc);
    } else {
      return new Ok([$string.trim(join(acc)), toList([])]);
    }
  }
}

function parse_value_double_quoted(loop$text, loop$acc) {
  while (true) {
    let text = loop$text;
    let acc = loop$acc;
    if (text.atLeastLength(1) && text.head === "\"") {
      let rest = text.tail;
      return new Ok([join(acc), rest]);
    } else if (text.atLeastLength(2) &&
    text.head === "\\" &&
    text.tail.head === "\"") {
      let c = text.tail.head;
      let rest = text.tail.tail;
      loop$text = rest;
      loop$acc = listPrepend(c, acc);
    } else if (text.atLeastLength(2) &&
    text.head === "\\" &&
    text.tail.head === "n") {
      let rest = text.tail.tail;
      loop$text = rest;
      loop$acc = listPrepend("\n", acc);
    } else if (text.atLeastLength(1)) {
      let c = text.head;
      let rest = text.tail;
      loop$text = rest;
      loop$acc = listPrepend(c, acc);
    } else {
      return new Error("unclosed double quote");
    }
  }
}

function parse_value_single_quoted(loop$text, loop$acc) {
  while (true) {
    let text = loop$text;
    let acc = loop$acc;
    if (text.atLeastLength(1) && text.head === "'") {
      let rest = text.tail;
      return new Ok([join(acc), rest]);
    } else if (text.atLeastLength(2) &&
    text.head === "\\" &&
    text.tail.head === "'") {
      let c = text.tail.head;
      let rest = text.tail.tail;
      loop$text = rest;
      loop$acc = listPrepend(c, acc);
    } else if (text.atLeastLength(1)) {
      let c = text.head;
      let rest = text.tail;
      loop$text = rest;
      loop$acc = listPrepend(c, acc);
    } else {
      return new Error("unclosed single quote");
    }
  }
}

function parse_value_backtick_quoted(loop$text, loop$acc) {
  while (true) {
    let text = loop$text;
    let acc = loop$acc;
    if (text.atLeastLength(1) && text.head === "`") {
      let rest = text.tail;
      return new Ok([join(acc), rest]);
    } else if (text.atLeastLength(2) &&
    text.head === "\\" &&
    text.tail.head === "`") {
      let char = text.tail.head;
      let rest = text.tail.tail;
      loop$text = rest;
      loop$acc = listPrepend(char, acc);
    } else if (text.atLeastLength(1)) {
      let char = text.head;
      let rest = text.tail;
      loop$text = rest;
      loop$acc = listPrepend(char, acc);
    } else {
      return new Error("unclosed backtick quote");
    }
  }
}

function parse_value(text) {
  if (text.atLeastLength(1) && text.head === "\n") {
    let rest = text.tail;
    return new Ok(["", rest]);
  } else if (text.atLeastLength(1) && text.head === "\"") {
    let rest = text.tail;
    return parse_value_double_quoted(rest, toList([]));
  } else if (text.atLeastLength(1) && text.head === "'") {
    let rest = text.tail;
    return parse_value_single_quoted(rest, toList([]));
  } else if (text.atLeastLength(1) && text.head === "`") {
    let rest = text.tail;
    return parse_value_backtick_quoted(rest, toList([]));
  } else if (text.atLeastLength(1) && text.head === "#") {
    let rest = text.tail;
    return parse_comment(rest, (r) => { return parse_value(r); });
  } else if (text.atLeastLength(1)) {
    let c = text.head;
    let rest = text.tail;
    return parse_value_unquoted(rest, toList([c]));
  } else {
    return new Ok(["", toList([])]);
  }
}

function parse_kv(text) {
  return try$(
    parse_key(text, toList([])),
    (_use0) => {
      let key = _use0[0];
      let rest = _use0[1];
      return try$(
        parse_value(rest),
        (_use0) => {
          let value = _use0[0];
          let rest$1 = _use0[1];
          return new Ok([[key, value], rest$1]);
        },
      );
    },
  );
}

function parse_kvs(loop$text, loop$acc) {
  while (true) {
    let text = loop$text;
    let acc = loop$acc;
    if (text.hasLength(0)) {
      return new Ok($list.reverse(acc));
    } else if (text.atLeastLength(1) && text.head === "\n") {
      let rest = text.tail;
      loop$text = rest;
      loop$acc = acc;
    } else if (text.atLeastLength(1) && text.head === " ") {
      let rest = text.tail;
      loop$text = rest;
      loop$acc = acc;
    } else if (text.atLeastLength(1) && text.head === "#") {
      let rest = text.tail;
      return parse_comment(rest, (r) => { return parse_kvs(r, acc); });
    } else if (text.atLeastLength(7) &&
    text.head === "e" &&
    text.tail.head === "x" &&
    text.tail.tail.head === "p" &&
    text.tail.tail.tail.head === "o" &&
    text.tail.tail.tail.tail.head === "r" &&
    text.tail.tail.tail.tail.tail.head === "t" &&
    text.tail.tail.tail.tail.tail.tail.head === " ") {
      let rest = text.tail.tail.tail.tail.tail.tail.tail;
      loop$text = rest;
      loop$acc = acc;
    } else {
      return try$(
        parse_kv(text),
        (_use0) => {
          let pair = _use0[0];
          let rest = _use0[1];
          return parse_kvs(rest, listPrepend(pair, acc));
        },
      );
    }
  }
}

function explode_to_graphemes(text) {
  let _pipe = $string.replace(text, "\r\n", "\n");
  return $string.to_graphemes(_pipe);
}

export function parse(text) {
  let _pipe = text;
  let _pipe$1 = explode_to_graphemes(_pipe);
  return parse_kvs(_pipe$1, toList([]));
}
