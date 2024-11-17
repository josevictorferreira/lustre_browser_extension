import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { getCurrentPosition as get_current_position } from "../../geolocation_ffi.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";

export { get_current_position };

export class GeolocationPosition extends $CustomType {
  constructor(latitude, longitude, altitude, accuracy, altitude_accuracy, heading, speed, timestamp) {
    super();
    this.latitude = latitude;
    this.longitude = longitude;
    this.altitude = altitude;
    this.accuracy = accuracy;
    this.altitude_accuracy = altitude_accuracy;
    this.heading = heading;
    this.speed = speed;
    this.timestamp = timestamp;
  }
}

export function decode(raw) {
  return $dynamic.decode8(
    (var0, var1, var2, var3, var4, var5, var6, var7) => {
      return new GeolocationPosition(
        var0,
        var1,
        var2,
        var3,
        var4,
        var5,
        var6,
        var7,
      );
    },
    $dynamic.field("coords", $dynamic.field("latitude", $dynamic.float)),
    $dynamic.field("coords", $dynamic.field("longitude", $dynamic.float)),
    $dynamic.field(
      "coords",
      $dynamic.field("altitude", $dynamic.optional($dynamic.float)),
    ),
    $dynamic.field("coords", $dynamic.field("accuracy", $dynamic.float)),
    $dynamic.field(
      "coords",
      $dynamic.field("altitudeAccuracy", $dynamic.optional($dynamic.float)),
    ),
    $dynamic.field(
      "coords",
      $dynamic.field("heading", $dynamic.optional($dynamic.float)),
    ),
    $dynamic.field(
      "coords",
      $dynamic.field("speed", $dynamic.optional($dynamic.float)),
    ),
    $dynamic.field("timestamp", $dynamic.float),
  )(raw);
}

export function current_position() {
  return $promise.new$(
    (resolve) => {
      return get_current_position(
        (position) => {
          let _pipe = (() => {
            let $ = decode($dynamic.from(position));
            if ($.isOk()) {
              let position$1 = $[0];
              return new Ok(position$1);
            } else {
              let reason = $[0];
              return new Error($string.inspect(reason));
            }
          })();
          return resolve(_pipe);
        },
        (error) => { return resolve(new Error($string.inspect(error))); },
      );
    },
  );
}
