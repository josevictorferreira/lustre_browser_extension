import * as $request from "../gleam_http/gleam/http/request.mjs";
import * as $response from "../gleam_http/gleam/http/response.mjs";
import * as $promise from "../gleam_javascript/gleam/javascript/promise.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import {
  translateRequest as translate_request,
  translateResponse as translate_response,
  readText as read_text,
  readBits as read_bits,
  readJson as read_json,
  readForm as read_form,
} from "./ffi.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";

export {
  read_bits,
  read_form,
  read_json,
  read_text,
  translate_request,
  translate_response,
};

export class Text extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Bits extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Stream extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class FormData extends $CustomType {
  constructor(values, files) {
    super();
    this.values = values;
    this.files = files;
  }
}

export class UploadedFile extends $CustomType {
  constructor(file_name) {
    super();
    this.file_name = file_name;
  }
}

export class AlreadyRead extends $CustomType {}

export class ParseError extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class ReadError extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}
