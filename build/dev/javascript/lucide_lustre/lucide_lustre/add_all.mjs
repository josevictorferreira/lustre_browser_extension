import * as $config from "../lucide_lustre/internal/config.mjs";
import * as $install_all from "../lucide_lustre/internal/install_all.mjs";

export function main() {
  let config = $config.get_config(false);
  return $install_all.install_all(config);
}
