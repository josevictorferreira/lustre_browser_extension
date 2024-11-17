import { toList, CustomType as $CustomType, isEqual } from "../gleam.mjs";
import * as $list from "../gleam/list.mjs";
import {
  add as do_append,
  concat as do_from_strings,
  concat as do_concat,
  identity as do_from_string,
  identity as do_to_string,
  length as do_byte_size,
  lowercase as do_lowercase,
  uppercase as do_uppercase,
  graphemes as do_to_graphemes,
  split as do_split,
  string_replace as replace,
} from "../gleam_stdlib.mjs";

export { replace };

class All extends $CustomType {}

export function prepend_tree(tree, prefix) {
  return do_append(prefix, tree);
}

export function append_tree(tree, suffix) {
  return do_append(tree, suffix);
}

export function new$() {
  return do_from_strings(toList([]));
}

export function from_strings(strings) {
  return do_from_strings(strings);
}

export function concat(trees) {
  return do_concat(trees);
}

export function from_string(string) {
  return do_from_string(string);
}

export function prepend(tree, prefix) {
  return append_tree(from_string(prefix), tree);
}

export function append(tree, second) {
  return append_tree(tree, from_string(second));
}

export function to_string(tree) {
  return do_to_string(tree);
}

export function byte_size(tree) {
  return do_byte_size(tree);
}

export function join(trees, sep) {
  let _pipe = trees;
  let _pipe$1 = $list.intersperse(_pipe, from_string(sep));
  return concat(_pipe$1);
}

export function lowercase(tree) {
  return do_lowercase(tree);
}

export function uppercase(tree) {
  return do_uppercase(tree);
}

function do_reverse(tree) {
  let _pipe = tree;
  let _pipe$1 = to_string(_pipe);
  let _pipe$2 = do_to_graphemes(_pipe$1);
  let _pipe$3 = $list.reverse(_pipe$2);
  return from_strings(_pipe$3);
}

export function reverse(tree) {
  return do_reverse(tree);
}

export function split(tree, pattern) {
  return do_split(tree, pattern);
}

export function is_equal(a, b) {
  return isEqual(a, b);
}

export function is_empty(tree) {
  return isEqual(from_string(""), tree);
}
