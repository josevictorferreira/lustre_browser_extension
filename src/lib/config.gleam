import dot_env as dot
import dot_env/env
import gleam/dict.{type Dict}
import gleam/io
import simplifile
import tom.{type Toml}

pub const views: List(String) = ["options", "popup", "sidepanel"]

pub fn name() -> String {
  parsed_config()
  |> get_string_property(["name"])
}

pub fn version() -> String {
  parsed_config()
  |> get_string_property(["version"])
}

pub fn description() -> String {
  parsed_config()
  |> get_string_property(["description"])
}

pub fn is_firefox() -> Bool {
  load_dotenv()

  case env.get_string("EXTENSION") {
    Ok(extension) -> extension == "firefox"
    _ -> False
  }
}

pub fn is_dev() -> Bool {
  load_dotenv()

  case env.get_string("GLEAM_ENV") {
    Ok(env) -> env == "development"
    _ -> False
  }
}

pub fn get_port() -> Int {
  load_dotenv()

  case env.get_int("PORT") {
    Ok(port) -> port
    _ -> 1234
  }
}

fn load_dotenv() {
  dot.new()
  |> dot.set_path("../../.env")
  |> dot.set_debug(False)
  |> dot.load
}

fn get_string_property(config: Dict(String, Toml), keys: List(String)) -> String {
  case tom.get_string(config, keys) {
    Ok(version) -> version
    _ -> {
      io.print_error("Could not read property from gleam.toml\n")
      ""
    }
  }
}

fn parsed_config() -> Dict(String, Toml) {
  let config_content = load_config_file()
  let assert Ok(parsed) = tom.parse(config_content)

  parsed
}

fn load_config_file() -> String {
  let filepath = "./gleam.toml"

  case simplifile.read(from: filepath) {
    Ok(contents) -> contents
    Error(_) -> {
      io.print_error("Could not read gleam.toml\n")
      ""
    }
  }
}
