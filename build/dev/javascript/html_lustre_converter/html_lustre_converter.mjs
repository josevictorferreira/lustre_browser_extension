import * as $doc from "../glam/glam/doc.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $parser from "../javascript_dom_parser/javascript_dom_parser.mjs";
import { Comment, Element, Text } from "../javascript_dom_parser/javascript_dom_parser.mjs";
import { Ok, Error, toList, CustomType as $CustomType, isEqual } from "./gleam.mjs";

class PreserveWhitespace extends $CustomType {}

class StripWhitespace extends $CustomType {}

class SVG extends $CustomType {}

class HTML extends $CustomType {}

function strip_body_wrapper(html, source) {
  let full_page = $string.contains(source, "<head>");
  if (html instanceof Element &&
  html.tag === "HTML" &&
  html.attributes.hasLength(0) &&
  html.children.hasLength(2) &&
  html.children.head instanceof Element &&
  html.children.head.tag === "HEAD" &&
  html.children.head.attributes.hasLength(0) &&
  html.children.head.children.hasLength(0) &&
  html.children.tail.head instanceof Element &&
  html.children.tail.head.tag === "BODY" &&
  html.children.tail.head.attributes.hasLength(0) &&
  (!full_page)) {
    let nodes = html.children.tail.head.children;
    return nodes;
  } else {
    return toList([html]);
  }
}

function print_string(t) {
  return ("\"" + $string.replace(t, "\"", "\\\"")) + "\"";
}

function print_text(t) {
  return $doc.from_string(("html.text(" + print_string(t)) + ")");
}

function get_text_content(nodes) {
  let _pipe = $list.filter_map(
    nodes,
    (node) => {
      if (node instanceof Text) {
        let t = node[0];
        return new Ok(t);
      } else {
        return new Error(undefined);
      }
    },
  );
  return $string.concat(_pipe);
}

function wrap(items, open, close) {
  let comma = $doc.concat(toList([$doc.from_string(","), $doc.space]));
  let open$1 = $doc.concat(toList([$doc.from_string(open), $doc.soft_break]));
  let trailing_comma = $doc.break$("", ",");
  let close$1 = $doc.concat(toList([trailing_comma, $doc.from_string(close)]));
  let _pipe = items;
  let _pipe$1 = $doc.join(_pipe, comma);
  let _pipe$2 = $doc.prepend(_pipe$1, open$1);
  let _pipe$3 = $doc.nest(_pipe$2, 2);
  let _pipe$4 = $doc.append(_pipe$3, close$1);
  return $doc.group(_pipe$4);
}

function print_attribute(attribute, svg_mode) {
  let $ = attribute[0];
  if ($ === "action") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "alt") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "attribute") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "autocomplete") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "class") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "download") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "enctype") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "for") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "form_action") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "form_enctype") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "form_method") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "form_target") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "href") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "id") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "map") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "max") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "method") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "min") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "msg") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "name") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "none") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "on") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "pattern") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "placeholder") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "rel") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "role") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "src") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "step") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "target") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "value") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "wrap") {
    return $doc.from_string(
      ((("attribute." + attribute[0]) + "(") + print_string(attribute[1])) + ")",
    );
  } else if ($ === "viewbox") {
    return $doc.from_string(
      ("attribute(\"viewBox\", " + print_string(attribute[1])) + ")",
    );
  } else if ($ === "type") {
    return $doc.from_string(
      ("attribute.type_(" + print_string(attribute[1])) + ")",
    );
  } else if ($ === "checked") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "controls") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "disabled") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "form_novalidate") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "loop") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "novalidate") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "readonly") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "required") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "selected") {
    return $doc.from_string(("attribute." + attribute[0]) + "(True)");
  } else if ($ === "width") {
    if (svg_mode instanceof SVG) {
      let children = toList([
        $doc.from_string(print_string(attribute[0])),
        $doc.from_string(print_string(attribute[1])),
      ]);
      let _pipe = $doc.from_string("attribute");
      return $doc.append(_pipe, wrap(children, "(", ")"));
    } else {
      return $doc.from_string(
        ((("attribute." + attribute[0]) + "(") + attribute[1]) + ")",
      );
    }
  } else if ($ === "height") {
    if (svg_mode instanceof SVG) {
      let children = toList([
        $doc.from_string(print_string(attribute[0])),
        $doc.from_string(print_string(attribute[1])),
      ]);
      let _pipe = $doc.from_string("attribute");
      return $doc.append(_pipe, wrap(children, "(", ")"));
    } else {
      return $doc.from_string(
        ((("attribute." + attribute[0]) + "(") + attribute[1]) + ")",
      );
    }
  } else if ($ === "cols") {
    if (svg_mode instanceof SVG) {
      let children = toList([
        $doc.from_string(print_string(attribute[0])),
        $doc.from_string(print_string(attribute[1])),
      ]);
      let _pipe = $doc.from_string("attribute");
      return $doc.append(_pipe, wrap(children, "(", ")"));
    } else {
      return $doc.from_string(
        ((("attribute." + attribute[0]) + "(") + attribute[1]) + ")",
      );
    }
  } else if ($ === "rows") {
    if (svg_mode instanceof SVG) {
      let children = toList([
        $doc.from_string(print_string(attribute[0])),
        $doc.from_string(print_string(attribute[1])),
      ]);
      let _pipe = $doc.from_string("attribute");
      return $doc.append(_pipe, wrap(children, "(", ")"));
    } else {
      return $doc.from_string(
        ((("attribute." + attribute[0]) + "(") + attribute[1]) + ")",
      );
    }
  } else {
    let children = toList([
      $doc.from_string(print_string(attribute[0])),
      $doc.from_string(print_string(attribute[1])),
    ]);
    let _pipe = $doc.from_string("attribute");
    return $doc.append(_pipe, wrap(children, "(", ")"));
  }
}

function print_svg_element(tag, attributes, children, ws) {
  let tag$1 = $string.lowercase(tag);
  let attributes$1 = (() => {
    let _pipe = $list.map(
      attributes,
      (a) => { return print_attribute(a, new SVG()); },
    );
    return wrap(_pipe, "[", "]");
  })();
  if (tag$1 === "animate") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "animatemotion") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "animatetransform") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "mpath") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "set") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "circle") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "ellipse") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "line") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "polygon") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "polyline") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "rect") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "feblend") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fecolormatrix") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fecomponenttransfer") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fecomposite") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "feconvolvematrix") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fedisplacementmap") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fedropshadow") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "feflood") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fefunca") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fefuncb") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fefuncg") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fefuncr") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fegaussianblur") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "feimage") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "femergenode") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "femorphology") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "feoffset") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "feturbulance") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "stop") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "image") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "path") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fedistantlight") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fepointlight") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "fespotlight") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "title") {
    let _pipe = $doc.from_string(("svg." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes$1);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "textarea") {
    let content = $doc.from_string(print_string(get_text_content(children)));
    let _pipe = $doc.from_string("text." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes$1, content]), "(", ")"));
  } else if (tag$1 === "text") {
    let content = $doc.from_string(print_string(get_text_content(children)));
    let _pipe = $doc.from_string("svg." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes$1, content]), "(", ")"));
  } else if (tag$1 === "use") {
    let _pipe = $doc.from_string("svg.use_");
    return $doc.append(_pipe, attributes$1);
  } else if (tag$1 === "defs") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "g") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "marker") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "mask") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "missing-glyph") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "pattern") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "switch") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "symbol") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "desc") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "metadata") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "fediffuselighting") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "femerge") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "fespecularlighting") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "fetile") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "lineargradient") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "radialgradient") {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg." + $string.replace(tag$1, "-", "_"));
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else {
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let tag$2 = $doc.from_string(print_string(tag$1));
    let _pipe = $doc.from_string("element");
    return $doc.append(
      _pipe,
      wrap(toList([tag$2, attributes$1, children$1]), "(", ")"),
    );
  }
}

function print_children(children, ws, svg_mode) {
  return $list.filter_map(
    children,
    (node) => {
      if (node instanceof Element) {
        let tag = node.tag;
        let attrs = node.attributes;
        let children$1 = node.children;
        if (svg_mode instanceof SVG) {
          return new Ok(print_svg_element(tag, attrs, children$1, ws));
        } else {
          return new Ok(print_element(tag, attrs, children$1, ws));
        }
      } else if (node instanceof Comment) {
        return new Error(undefined);
      } else if (node instanceof Text && (isEqual(ws, new StripWhitespace()))) {
        let t = node[0];
        let $ = $string.trim_left(t);
        if ($ === "") {
          return new Error(undefined);
        } else {
          let t$1 = $;
          return new Ok(print_text(t$1));
        }
      } else {
        let t = node[0];
        return new Ok(print_text(t));
      }
    },
  );
}

export function convert(html) {
  let documents = (() => {
    let _pipe = html;
    let _pipe$1 = $parser.parse_to_records(_pipe);
    let _pipe$2 = strip_body_wrapper(_pipe$1, html);
    return print_children(_pipe$2, new StripWhitespace(), new HTML());
  })();
  let _pipe = (() => {
    if (documents.hasLength(0)) {
      return $doc.empty;
    } else if (documents.hasLength(1)) {
      let document = documents.head;
      return document;
    } else {
      return wrap(documents, "[", "]");
    }
  })();
  return $doc.to_string(_pipe, 80);
}

function print_element(tag, given_attributes, children, ws) {
  let tag$1 = $string.lowercase(tag);
  let attributes = (() => {
    let _pipe = $list.map(
      given_attributes,
      (a) => { return print_attribute(a, new HTML()); },
    );
    return wrap(_pipe, "[", "]");
  })();
  if (tag$1 === "area") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "base") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "br") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "col") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "embed") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "hr") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "img") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "input") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "link") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "meta") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "param") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "source") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "track") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "wbr") {
    let _pipe = $doc.from_string(("html." + tag$1) + "(");
    let _pipe$1 = $doc.append(_pipe, attributes);
    return $doc.append(_pipe$1, $doc.from_string(")"));
  } else if (tag$1 === "a") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "abbr") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "address") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "article") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "aside") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "audio") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "b") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "bdi") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "bdo") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "blockquote") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "body") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "button") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "canvas") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "caption") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "cite") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "code") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "colgroup") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "data") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "datalist") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "dd") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "del") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "details") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "dfn") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "dialog") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "div") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "dl") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "dt") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "em") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "fieldset") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "figcaption") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "figure") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "footer") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "form") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "h1") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "h2") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "h3") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "h4") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "h5") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "h6") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "head") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "header") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "hgroup") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "html") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "i") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "iframe") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "ins") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "kbd") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "label") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "legend") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "li") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "main") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "map") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "mark") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "math") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "menu") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "meter") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "nav") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "noscript") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "object") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "ol") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "optgroup") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "output") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "p") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "picture") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "portal") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "progress") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "q") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "rp") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "rt") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "ruby") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "s") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "samp") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "search") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "section") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "select") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "slot") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "small") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "span") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "strong") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "style") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "sub") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "summary") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "sup") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "table") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "tbody") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "td") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "template") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "text") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "tfoot") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "th") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "thead") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "time") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "tr") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "u") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "ul") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "var") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "video") {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "svg") {
    let attributes$1 = (() => {
      let _pipe = $list.map(
        given_attributes,
        (a) => { return print_attribute(a, new SVG()); },
      );
      return wrap(_pipe, "[", "]");
    })();
    let children$1 = wrap(print_children(children, ws, new SVG()), "[", "]");
    let _pipe = $doc.from_string("svg.svg");
    return $doc.append(
      _pipe,
      wrap(toList([attributes$1, children$1]), "(", ")"),
    );
  } else if (tag$1 === "pre") {
    let children$1 = wrap(
      print_children(children, new PreserveWhitespace(), new HTML()),
      "[",
      "]",
    );
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, children$1]), "(", ")"));
  } else if (tag$1 === "option") {
    let content = $doc.from_string(print_string(get_text_content(children)));
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, content]), "(", ")"));
  } else if (tag$1 === "textarea") {
    let content = $doc.from_string(print_string(get_text_content(children)));
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, content]), "(", ")"));
  } else if (tag$1 === "title") {
    let content = $doc.from_string(print_string(get_text_content(children)));
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, content]), "(", ")"));
  } else if (tag$1 === "script") {
    let content = $doc.from_string(print_string(get_text_content(children)));
    let _pipe = $doc.from_string("html." + tag$1);
    return $doc.append(_pipe, wrap(toList([attributes, content]), "(", ")"));
  } else {
    let children$1 = wrap(print_children(children, ws, new HTML()), "[", "]");
    let tag$2 = $doc.from_string(print_string(tag$1));
    let _pipe = $doc.from_string("element");
    return $doc.append(
      _pipe,
      wrap(toList([tag$2, attributes, children$1]), "(", ")"),
    );
  }
}
