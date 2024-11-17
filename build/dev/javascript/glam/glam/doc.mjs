import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../../gleam_stdlib/gleam/order.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType, makeError } from "../gleam.mjs";

class Line extends $CustomType {
  constructor(size) {
    super();
    this.size = size;
  }
}

class Concat extends $CustomType {
  constructor(docs) {
    super();
    this.docs = docs;
  }
}

class Text extends $CustomType {
  constructor(text, length) {
    super();
    this.text = text;
    this.length = length;
  }
}

class Nest extends $CustomType {
  constructor(doc, indentation) {
    super();
    this.doc = doc;
    this.indentation = indentation;
  }
}

class ForceBreak extends $CustomType {
  constructor(doc) {
    super();
    this.doc = doc;
  }
}

class Break extends $CustomType {
  constructor(unbroken, broken) {
    super();
    this.unbroken = unbroken;
    this.broken = broken;
  }
}

class FlexBreak extends $CustomType {
  constructor(unbroken, broken) {
    super();
    this.unbroken = unbroken;
    this.broken = broken;
  }
}

class Group extends $CustomType {
  constructor(doc) {
    super();
    this.doc = doc;
  }
}

class Broken extends $CustomType {}

class ForceBroken extends $CustomType {}

class Unbroken extends $CustomType {}

export function append(first, second) {
  if (first instanceof Concat) {
    let docs = first.docs;
    return new Concat($list.append(docs, toList([second])));
  } else {
    return new Concat(toList([first, second]));
  }
}

export function break$(unbroken, broken) {
  return new Break(unbroken, broken);
}

export function concat(docs) {
  return new Concat(docs);
}

export function append_docs(first, docs) {
  return append(first, concat(docs));
}

export function flex_break(unbroken, broken) {
  return new FlexBreak(unbroken, broken);
}

export function force_break(doc) {
  return new ForceBreak(doc);
}

export function from_string(string) {
  return new Text(string, $string.length(string));
}

export function zero_width_string(string) {
  return new Text(string, 0);
}

export function group(doc) {
  return new Group(doc);
}

export function join(docs, separator) {
  return concat($list.intersperse(docs, separator));
}

export function concat_join(docs, separators) {
  return join(docs, concat(separators));
}

export function lines(size) {
  return new Line(size);
}

export function nest(doc, indentation) {
  return new Nest(doc, indentation);
}

export function nest_docs(docs, indentation) {
  return new Nest(concat(docs), indentation);
}

export function prepend(first, second) {
  if (first instanceof Concat) {
    let docs = first.docs;
    return new Concat(listPrepend(second, docs));
  } else {
    return new Concat(toList([second, first]));
  }
}

export function prepend_docs(first, docs) {
  return prepend(first, concat(docs));
}

function fits(loop$docs, loop$max_width, loop$current_width) {
  while (true) {
    let docs = loop$docs;
    let max_width = loop$max_width;
    let current_width = loop$current_width;
    if (current_width > max_width) {
      return false;
    } else if (docs.hasLength(0)) {
      return true;
    } else {
      let indent = docs.head[0];
      let mode = docs.head[1];
      let doc = docs.head[2];
      let rest = docs.tail;
      if (doc instanceof Line) {
        return true;
      } else if (doc instanceof ForceBreak) {
        return false;
      } else if (doc instanceof Text) {
        let length = doc.length;
        loop$docs = rest;
        loop$max_width = max_width;
        loop$current_width = current_width + length;
      } else if (doc instanceof Nest) {
        let doc$1 = doc.doc;
        let i = doc.indentation;
        let _pipe = listPrepend([indent + i, mode, doc$1], rest);
        loop$docs = _pipe;
        loop$max_width = max_width;
        loop$current_width = current_width;
      } else if (doc instanceof Break) {
        let unbroken = doc.unbroken;
        if (mode instanceof Broken) {
          return true;
        } else if (mode instanceof ForceBroken) {
          return true;
        } else {
          loop$docs = rest;
          loop$max_width = max_width;
          loop$current_width = current_width + $string.length(unbroken);
        }
      } else if (doc instanceof FlexBreak) {
        let unbroken = doc.unbroken;
        if (mode instanceof Broken) {
          return true;
        } else if (mode instanceof ForceBroken) {
          return true;
        } else {
          loop$docs = rest;
          loop$max_width = max_width;
          loop$current_width = current_width + $string.length(unbroken);
        }
      } else if (doc instanceof Group) {
        let doc$1 = doc.doc;
        loop$docs = listPrepend([indent, mode, doc$1], rest);
        loop$max_width = max_width;
        loop$current_width = current_width;
      } else {
        let docs$1 = doc.docs;
        let _pipe = $list.map(docs$1, (doc) => { return [indent, mode, doc]; });
        let _pipe$1 = $list.append(_pipe, rest);
        loop$docs = _pipe$1;
        loop$max_width = max_width;
        loop$current_width = current_width;
      }
    }
  }
}

function do_flatten(loop$docs, loop$acc) {
  while (true) {
    let docs = loop$docs;
    let acc = loop$acc;
    if (docs.hasLength(0)) {
      return $list.reverse(acc);
    } else if (docs.hasLength(1)) {
      let one = docs.head;
      return $list.reverse(listPrepend(one, acc));
    } else if (docs.atLeastLength(2) &&
    docs.head instanceof Concat &&
    docs.tail.head instanceof Concat) {
      let one = docs.head.docs;
      let two = docs.tail.head.docs;
      let rest = docs.tail.tail;
      loop$docs = listPrepend(new Concat($list.append(one, two)), rest);
      loop$acc = acc;
    } else if (docs.atLeastLength(2) &&
    docs.head instanceof Text &&
    docs.tail.head instanceof Text) {
      let one = docs.head.text;
      let len_one = docs.head.length;
      let two = docs.tail.head.text;
      let len_two = docs.tail.head.length;
      let rest = docs.tail.tail;
      loop$docs = listPrepend(new Text(one + two, len_one + len_two), rest);
      loop$acc = acc;
    } else {
      let one = docs.head;
      let two = docs.tail.head;
      let rest = docs.tail.tail;
      loop$docs = listPrepend(two, rest);
      loop$acc = listPrepend(one, acc);
    }
  }
}

function flatten(docs) {
  return do_flatten(docs, toList([]));
}

function do_split_groups(loop$docs, loop$current_group, loop$acc) {
  while (true) {
    let docs = loop$docs;
    let current_group = loop$current_group;
    let acc = loop$acc;
    if (docs.hasLength(0)) {
      if (current_group.hasLength(0)) {
        return $list.reverse(acc);
      } else {
        return $list.reverse(listPrepend($list.reverse(current_group), acc));
      }
    } else if (docs.atLeastLength(1) && docs.head instanceof Group) {
      let doc = docs.head;
      let rest = docs.tail;
      if (current_group.hasLength(0)) {
        loop$docs = rest;
        loop$current_group = toList([]);
        loop$acc = listPrepend(toList([doc]), acc);
      } else {
        loop$docs = rest;
        loop$current_group = toList([]);
        loop$acc = listPrepend(
          toList([doc]),
          listPrepend($list.reverse(current_group), acc),
        );
      }
    } else {
      let doc = docs.head;
      let rest = docs.tail;
      loop$docs = rest;
      loop$current_group = listPrepend(doc, current_group);
      loop$acc = acc;
    }
  }
}

function split_groups(docs) {
  return do_split_groups(docs, toList([]), toList([]));
}

function superscript_number(number) {
  let $ = $int.digits(number, 10);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "glam/doc",
      752,
      "superscript_number",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let digits = $[0];
  return $list.fold(
    digits,
    "",
    (acc, digit) => {
      let digit$1 = (() => {
        if (digit === 0) {
          return "⁰";
        } else if (digit === 1) {
          return "¹";
        } else if (digit === 2) {
          return "²";
        } else if (digit === 3) {
          return "³";
        } else if (digit === 4) {
          return "⁴";
        } else if (digit === 5) {
          return "⁵";
        } else if (digit === 6) {
          return "⁶";
        } else if (digit === 7) {
          return "⁷";
        } else if (digit === 8) {
          return "⁸";
        } else if (digit === 9) {
          return "⁹";
        } else {
          throw makeError("panic", "glam/doc", 765, "", "not a digit", {})
        }
      })();
      return acc + digit$1;
    },
  );
}

function indentation(size) {
  return $string.repeat(" ", size);
}

function do_to_string(loop$acc, loop$max_width, loop$current_width, loop$docs) {
  while (true) {
    let acc = loop$acc;
    let max_width = loop$max_width;
    let current_width = loop$current_width;
    let docs = loop$docs;
    if (docs.hasLength(0)) {
      return acc;
    } else {
      let indent = docs.head[0];
      let mode = docs.head[1];
      let doc = docs.head[2];
      let rest = docs.tail;
      if (doc instanceof Line) {
        let size = doc.size;
        let _pipe = ((acc + $string.repeat("\n", size)) + indentation(indent));
        loop$acc = _pipe;
        loop$max_width = max_width;
        loop$current_width = indent;
        loop$docs = rest;
      } else if (doc instanceof FlexBreak) {
        let unbroken = doc.unbroken;
        let broken = doc.broken;
        let new_unbroken_width = current_width + $string.length(unbroken);
        let $ = fits(rest, max_width, new_unbroken_width);
        if ($) {
          let _pipe = (acc + unbroken);
          loop$acc = _pipe;
          loop$max_width = max_width;
          loop$current_width = new_unbroken_width;
          loop$docs = rest;
        } else {
          let _pipe = (((acc + broken) + "\n") + indentation(indent));
          loop$acc = _pipe;
          loop$max_width = max_width;
          loop$current_width = indent;
          loop$docs = rest;
        }
      } else if (doc instanceof Break) {
        let unbroken = doc.unbroken;
        let broken = doc.broken;
        if (mode instanceof Unbroken) {
          let new_width = current_width + $string.length(unbroken);
          loop$acc = acc + unbroken;
          loop$max_width = max_width;
          loop$current_width = new_width;
          loop$docs = rest;
        } else if (mode instanceof Broken) {
          let _pipe = (((acc + broken) + "\n") + indentation(indent));
          loop$acc = _pipe;
          loop$max_width = max_width;
          loop$current_width = indent;
          loop$docs = rest;
        } else {
          let _pipe = (((acc + broken) + "\n") + indentation(indent));
          loop$acc = _pipe;
          loop$max_width = max_width;
          loop$current_width = indent;
          loop$docs = rest;
        }
      } else if (doc instanceof ForceBreak) {
        let doc$1 = doc.doc;
        let docs$1 = listPrepend([indent, new ForceBroken(), doc$1], rest);
        loop$acc = acc;
        loop$max_width = max_width;
        loop$current_width = current_width;
        loop$docs = docs$1;
      } else if (doc instanceof Concat) {
        let docs$1 = doc.docs;
        let docs$2 = (() => {
          let _pipe = $list.map(
            docs$1,
            (doc) => { return [indent, mode, doc]; },
          );
          return $list.append(_pipe, rest);
        })();
        loop$acc = acc;
        loop$max_width = max_width;
        loop$current_width = current_width;
        loop$docs = docs$2;
      } else if (doc instanceof Group) {
        let doc$1 = doc.doc;
        let fits$1 = fits(
          toList([[indent, new Unbroken(), doc$1]]),
          max_width,
          current_width,
        );
        let new_mode = (() => {
          if (fits$1) {
            return new Unbroken();
          } else {
            return new Broken();
          }
        })();
        let docs$1 = listPrepend([indent, new_mode, doc$1], rest);
        loop$acc = acc;
        loop$max_width = max_width;
        loop$current_width = current_width;
        loop$docs = docs$1;
      } else if (doc instanceof Nest) {
        let doc$1 = doc.doc;
        let i = doc.indentation;
        let docs$1 = listPrepend([indent + i, mode, doc$1], rest);
        loop$acc = acc;
        loop$max_width = max_width;
        loop$current_width = current_width;
        loop$docs = docs$1;
      } else {
        let text = doc.text;
        let length = doc.length;
        loop$acc = acc + text;
        loop$max_width = max_width;
        loop$current_width = current_width + length;
        loop$docs = rest;
      }
    }
  }
}

export function to_string(doc, limit) {
  return do_to_string("", limit, 0, toList([[0, new Unbroken(), doc]]));
}

export const empty = /* @__PURE__ */ new Concat(/* @__PURE__ */ toList([]));

export const flex_space = /* @__PURE__ */ new FlexBreak(" ", "");

export const line = /* @__PURE__ */ new Line(1);

export const soft_break = /* @__PURE__ */ new Break("", "");

export const space = /* @__PURE__ */ new Break(" ", "");

const debug_nesting = 2;

function parenthesise(document, open, close) {
  let _pipe = toList([
    from_string(open),
    nest(line, debug_nesting),
    nest(document, debug_nesting),
    line,
    from_string(close),
  ]);
  let _pipe$1 = concat(_pipe);
  return group(_pipe$1);
}

export function debug(document) {
  if (document instanceof Text) {
    let text = document.text;
    let escaped = $string.replace(text, "\"", "\\\"");
    return from_string(("\"" + escaped) + "\"");
  } else if (document instanceof ForceBreak) {
    let doc = document.doc;
    return parenthesise(debug(doc), "force(", ")");
  } else if (document instanceof Group) {
    let doc = document.doc;
    let _pipe = parenthesise(debug(doc), "[", "]");
    return force_break(_pipe);
  } else if (document instanceof Nest) {
    let doc = document.doc;
    let indentation$1 = document.indentation;
    let _pipe = parenthesise(
      debug(doc),
      superscript_number(indentation$1) + "⟨",
      "⟩",
    );
    return force_break(_pipe);
  } else if (document instanceof Break &&
  document.unbroken === " " &&
  document.broken === "") {
    return from_string("space");
  } else if (document instanceof Break) {
    let unbroken = document.unbroken;
    let broken = document.broken;
    return from_string(((("{ \"" + unbroken) + "\", \"") + broken) + "\" }");
  } else if (document instanceof FlexBreak &&
  document.unbroken === " " &&
  document.broken === "") {
    return from_string("flex_space");
  } else if (document instanceof FlexBreak) {
    let unbroken = document.unbroken;
    let broken = document.broken;
    return from_string(((("flex{ \"" + unbroken) + "\", \"") + broken) + "\" }");
  } else if (document instanceof Line) {
    let size = document.size;
    let $ = $int.compare(size, 1);
    if ($ instanceof Lt) {
      return from_string("lf");
    } else if ($ instanceof Eq) {
      return from_string("lf");
    } else {
      return from_string("lf" + superscript_number(size));
    }
  } else {
    let docs = document.docs;
    let _pipe = split_groups(flatten(docs));
    let _pipe$1 = $list.map(
      _pipe,
      (docs) => {
        if (docs.hasLength(0)) {
          throw makeError("panic", "glam/doc", 686, "", "empty", {})
        } else {
          undefined
        }
        let _pipe$1 = $list.map(docs, debug);
        let _pipe$2 = join(_pipe$1, flex_break(" . ", " ."));
        return group(_pipe$2);
      },
    );
    return join(_pipe$1, concat(toList([flex_break(" . ", " ."), line])));
  }
}
