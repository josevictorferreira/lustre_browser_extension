import * as $promise from "../../../../gleam_javascript/gleam/javascript/promise.mjs";
import { CustomType as $CustomType } from "../../../gleam.mjs";
import { digest as do_digest } from "../../../plinth_browser_crypto_subtle_ffi.mjs";

export class SHA1 extends $CustomType {}

export class SHA256 extends $CustomType {}

export class SHA384 extends $CustomType {}

export class SHA512 extends $CustomType {}

export function digest_algorithm_to_string(algorithm) {
  if (algorithm instanceof SHA1) {
    return "SHA-1";
  } else if (algorithm instanceof SHA256) {
    return "SHA-256";
  } else if (algorithm instanceof SHA384) {
    return "SHA-384";
  } else {
    return "SHA-512";
  }
}

export function digest(algorithm, data) {
  return do_digest(digest_algorithm_to_string(algorithm), data);
}
