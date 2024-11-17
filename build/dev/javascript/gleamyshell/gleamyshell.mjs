import { CustomType as $CustomType } from "./gleam.mjs";
import { execute, cwd, os, homeDirectory as home_directory, env, which } from "./gleamyshell_ffi.mjs";

export { cwd, env, execute, home_directory, os, which };

export class CommandOutput extends $CustomType {
  constructor(exit_code, output) {
    super();
    this.exit_code = exit_code;
    this.output = output;
  }
}

export class Unix extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Windows extends $CustomType {}

export class Darwin extends $CustomType {}

export class FreeBsd extends $CustomType {}

export class OpenBsd extends $CustomType {}

export class Linux extends $CustomType {}

export class SunOs extends $CustomType {}

export class OtherOs extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}
