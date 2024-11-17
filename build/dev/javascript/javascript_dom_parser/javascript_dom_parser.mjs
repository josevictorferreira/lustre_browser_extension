import { CustomType as $CustomType } from "./gleam.mjs";
import {
  parse as parse_to_dom,
  toRecords as dom_to_records,
  toString as dom_to_string,
} from "./javascript_dom_parser_ffi.mjs";

export { dom_to_records, dom_to_string, parse_to_dom };

export class Element extends $CustomType {
  constructor(tag, attributes, children) {
    super();
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
  }
}

export class Text extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Comment extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function parse_to_records(html) {
  let _pipe = html;
  let _pipe$1 = parse_to_dom(_pipe);
  return dom_to_records(_pipe$1);
}
