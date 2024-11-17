import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $regex from "../gleam_stdlib/gleam/regex.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "./gleam.mjs";

class Pattern extends $CustomType {
  constructor(regex, options) {
    super();
    this.regex = regex;
    this.options = options;
  }
}

export class PatternOptions extends $CustomType {
  constructor(ignore_case, match_dotfiles) {
    super();
    this.ignore_case = ignore_case;
    this.match_dotfiles = match_dotfiles;
  }
}

export class AbsolutePatternFromDirError extends $CustomType {}

export class InvalidGlobStarError extends $CustomType {}

export class MissingClosingBracketError extends $CustomType {}

export function match_pattern(pattern, path) {
  return $regex.check(pattern.regex, path);
}

function escape_meta_char(char) {
  if (char === "\\") {
    return "\\\\";
  } else if (char === "^") {
    return "\\^";
  } else if (char === "$") {
    return "\\$";
  } else if (char === ".") {
    return "\\.";
  } else if (char === "[") {
    return "\\[";
  } else if (char === "|") {
    return "\\|";
  } else if (char === "(") {
    return "\\(";
  } else if (char === ")") {
    return "\\)";
  } else if (char === "?") {
    return "\\?";
  } else if (char === "*") {
    return "\\*";
  } else if (char === "+") {
    return "\\+";
  } else if (char === "{") {
    return "\\{";
  } else if (char === "]") {
    return "\\]";
  } else if (char === "}") {
    return "\\}";
  } else {
    return char;
  }
}

function parse_path_chars(prefix) {
  let _pipe = prefix;
  let _pipe$1 = $string.to_graphemes(_pipe);
  let _pipe$2 = $list.map(_pipe$1, escape_meta_char);
  let _pipe$3 = $list.reverse(_pipe$2);
  return ((path_chars) => {
    if (path_chars.hasLength(0)) {
      return path_chars;
    } else if (path_chars.atLeastLength(1) && path_chars.head === "/") {
      return path_chars;
    } else {
      return listPrepend("/", path_chars);
    }
  })(_pipe$3);
}

function start_of_directory(path_chars) {
  if (path_chars.hasLength(0)) {
    return true;
  } else if (path_chars.hasLength(1) && path_chars.head === "") {
    return true;
  } else {
    let previous = path_chars.head;
    return $string.ends_with(previous, "/");
  }
}

function ignore_dotfiles(path_chars, options) {
  return !options.match_dotfiles && start_of_directory(path_chars);
}

function do_convert_pattern(
  loop$graphemes,
  loop$path_chars,
  loop$in_range,
  loop$options
) {
  while (true) {
    let graphemes = loop$graphemes;
    let path_chars = loop$path_chars;
    let in_range = loop$in_range;
    let options = loop$options;
    if (in_range) {
      if (graphemes.hasLength(0)) {
        return new Error(new MissingClosingBracketError());
      } else if (graphemes.atLeastLength(1) && graphemes.head === "]") {
        let rest = graphemes.tail;
        loop$graphemes = rest;
        loop$path_chars = listPrepend("]", path_chars);
        loop$in_range = false;
        loop$options = options;
      } else if (graphemes.atLeastLength(2) && graphemes.head === "\\") {
        let second = graphemes.tail.head;
        let rest = graphemes.tail.tail;
        let _pipe = listPrepend(escape_meta_char(second), path_chars);
        return ((_capture) => {
          return do_convert_pattern(rest, _capture, true, options);
        })(_pipe);
      } else {
        let first = graphemes.head;
        let rest = graphemes.tail;
        let _pipe = listPrepend(escape_meta_char(first), path_chars);
        return ((_capture) => {
          return do_convert_pattern(rest, _capture, true, options);
        })(_pipe);
      }
    } else {
      if (graphemes.hasLength(0)) {
        let _pipe = path_chars;
        let _pipe$1 = $list.reverse(_pipe);
        let _pipe$2 = $string.concat(_pipe$1);
        return new Ok(_pipe$2);
      } else if (graphemes.atLeastLength(2) &&
      graphemes.head === "[" &&
      graphemes.tail.head === "]") {
        let rest = graphemes.tail.tail;
        loop$graphemes = rest;
        loop$path_chars = listPrepend("\\[\\]", path_chars);
        loop$in_range = false;
        loop$options = options;
      } else if (graphemes.atLeastLength(2) &&
      graphemes.head === "[" &&
      graphemes.tail.head === "!") {
        let rest = graphemes.tail.tail;
        loop$graphemes = rest;
        loop$path_chars = listPrepend("[^", path_chars);
        loop$in_range = true;
        loop$options = options;
      } else if (graphemes.atLeastLength(2) &&
      graphemes.head === "[" &&
      graphemes.tail.head === "^") {
        let rest = graphemes.tail.tail;
        loop$graphemes = rest;
        loop$path_chars = listPrepend("[\\^", path_chars);
        loop$in_range = true;
        loop$options = options;
      } else if (graphemes.atLeastLength(1) && graphemes.head === "[") {
        let rest = graphemes.tail;
        loop$graphemes = rest;
        loop$path_chars = listPrepend("[", path_chars);
        loop$in_range = true;
        loop$options = options;
      } else if (graphemes.atLeastLength(2) && graphemes.head === "\\") {
        let second = graphemes.tail.head;
        let rest = graphemes.tail.tail;
        let _pipe = listPrepend(escape_meta_char(second), path_chars);
        return ((_capture) => {
          return do_convert_pattern(rest, _capture, false, options);
        })(_pipe);
      } else if (graphemes.atLeastLength(1) && graphemes.head === "?") {
        let rest = graphemes.tail;
        let wildcard = (() => {
          let $ = ignore_dotfiles(path_chars, options);
          if ($) {
            return "[^/.]";
          } else {
            return "[^/]";
          }
        })();
        loop$graphemes = rest;
        loop$path_chars = listPrepend(wildcard, path_chars);
        loop$in_range = false;
        loop$options = options;
      } else if (graphemes.atLeastLength(2) &&
      graphemes.head === "*" &&
      graphemes.tail.head === "*") {
        let rest = graphemes.tail.tail;
        if (path_chars.hasLength(0) && rest.hasLength(0)) {
          let wildcard = (() => {
            let $ = options.match_dotfiles;
            if ($) {
              return ".*";
            } else {
              return "([^.][^/]*(/[^.][^/]*)*)?";
            }
          })();
          let path_chars$1 = listPrepend(wildcard, path_chars);
          loop$graphemes = rest;
          loop$path_chars = path_chars$1;
          loop$in_range = false;
          loop$options = options;
        } else if (path_chars.atLeastLength(1) &&
        path_chars.head === "/" &&
        rest.hasLength(0)) {
          let path_chars$1 = path_chars.tail;
          let wildcard = (() => {
            let $ = options.match_dotfiles;
            if ($) {
              return "(/.*)?";
            } else {
              return "(/[^.][^/]*)*";
            }
          })();
          let path_chars$2 = listPrepend(wildcard, path_chars$1);
          loop$graphemes = rest;
          loop$path_chars = path_chars$2;
          loop$in_range = false;
          loop$options = options;
        } else if (path_chars.hasLength(0) &&
        rest.atLeastLength(1) &&
        rest.head === "/") {
          let rest$1 = rest.tail;
          let wildcard = (() => {
            let $ = options.match_dotfiles;
            if ($) {
              return "(.*/)?";
            } else {
              return "([^.][^/]*/)*";
            }
          })();
          let path_chars$1 = listPrepend(wildcard, path_chars);
          loop$graphemes = rest$1;
          loop$path_chars = path_chars$1;
          loop$in_range = false;
          loop$options = options;
        } else if (path_chars.atLeastLength(1) &&
        path_chars.head === "/" &&
        rest.atLeastLength(1) &&
        rest.head === "/") {
          let rest$1 = rest.tail;
          let wildcard = (() => {
            let $ = options.match_dotfiles;
            if ($) {
              return "(.*/)?";
            } else {
              return "([^.][^/]*/)*";
            }
          })();
          let path_chars$1 = listPrepend(wildcard, path_chars);
          loop$graphemes = rest$1;
          loop$path_chars = path_chars$1;
          loop$in_range = false;
          loop$options = options;
        } else {
          return new Error(new InvalidGlobStarError());
        }
      } else if (graphemes.atLeastLength(1) && graphemes.head === "*") {
        let rest = graphemes.tail;
        let wildcard = (() => {
          let $ = ignore_dotfiles(path_chars, options);
          if ($) {
            return "([^.][^/]*)?";
          } else {
            return "[^/]*";
          }
        })();
        loop$graphemes = rest;
        loop$path_chars = listPrepend(wildcard, path_chars);
        loop$in_range = false;
        loop$options = options;
      } else {
        let first = graphemes.head;
        let rest = graphemes.tail;
        let _pipe = listPrepend(escape_meta_char(first), path_chars);
        return ((_capture) => {
          return do_convert_pattern(rest, _capture, false, options);
        })(_pipe);
      }
    }
  }
}

function convert_pattern(prefix, pattern, options) {
  let graphemes = $string.to_graphemes(pattern);
  let path_chars = parse_path_chars(prefix);
  if (graphemes.atLeastLength(1) &&
  graphemes.head === "/" &&
  path_chars.atLeastLength(1)) {
    return new Error(new AbsolutePatternFromDirError());
  } else {
    let $ = do_convert_pattern(graphemes, path_chars, false, options);
    if ($.isOk()) {
      let regex_pattern = $[0];
      return new Ok(("^" + regex_pattern) + "$");
    } else {
      let err = $[0];
      return new Error(err);
    }
  }
}

export function new_pattern_with(pattern, directory, options) {
  let $ = convert_pattern(directory, pattern, options);
  if ($.isOk()) {
    let pattern$1 = $[0];
    let regex_options = new $regex.Options(options.ignore_case, false);
    let $1 = $regex.compile(pattern$1, regex_options);
    if ($1.isOk()) {
      let regex = $1[0];
      return new Ok(new Pattern(regex, options));
    } else {
      let err = $1[0];
      let error_message = ((((("Globlin Regex Compile Bug: " + "with directory '") + directory) + "' and pattern '") + pattern$1) + "': ") + err.error;
      throw makeError(
        "panic",
        "globlin",
        57,
        "new_pattern_with",
        error_message,
        {}
      )
    }
  } else {
    let err = $[0];
    return new Error(err);
  }
}

const empty_options = /* @__PURE__ */ new PatternOptions(false, false);

export function new_pattern(pattern) {
  return new_pattern_with(pattern, "", empty_options);
}
