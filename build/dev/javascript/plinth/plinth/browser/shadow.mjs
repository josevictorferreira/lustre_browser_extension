import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $element from "../../plinth/browser/element.mjs";
import { appendChild as append_child, attachShadow as attach_shadow } from "../../shadow_ffi.mjs";

export { append_child, attach_shadow };

export class Open extends $CustomType {}

export class Closed extends $CustomType {}
