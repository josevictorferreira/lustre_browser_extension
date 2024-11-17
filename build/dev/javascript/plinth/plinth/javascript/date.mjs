import {
  now,
  toISOString as to_iso_string,
  new_ as new$,
  year,
  month,
  date,
  day,
  hours,
  minutes,
  seconds,
} from "../../date_ffi.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

export {
  date,
  day,
  hours,
  minutes,
  month,
  new$,
  now,
  seconds,
  to_iso_string,
  year,
};

export class Date extends $CustomType {}
