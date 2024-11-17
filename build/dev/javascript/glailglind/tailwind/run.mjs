import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Ok } from "../gleam.mjs";
import * as $tailwind from "../tailwind.mjs";

export function main() {
  let output = (() => {
    let _pipe = $tailwind.get_args();
    let _pipe$1 = $result.try$(_pipe, (args) => { return $tailwind.run(args); });
    return $result.try$(
      _pipe$1,
      (output) => {
        $io.println(output);
        return new Ok(undefined);
      },
    );
  })();
  if (!output.isOk()) {
    let err = output[0];
    return $io.println(err);
  } else {
    return undefined;
  }
}
