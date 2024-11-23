import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import lib/file_utils
import lib/project_config
import scripts/manifest
import simplifile

pub fn main() {
  manifest.main()
  stub_index_html(project_config.views)
}

pub fn stub_index_html(views: List(String)) {
  let port = project_config.get_port() |> int.to_string()

  list.each(views, fn(view) {
    let extension_dir = "./extension/dist/" <> view <> "/"
    let index_html_file_path = "./src/" <> view <> "/index.html"

    file_utils.ensure_dir(extension_dir)

    let assert Ok(file_content) = simplifile.read(index_html_file_path)

    let new_file_content =
      file_content
      |> string.replace(
        each: "\"./main.ts\"",
        with: "\"http://localhost:" <> port <> "/" <> view <> "/main.ts\"",
      )
      |> string.replace(
        each: "<div id=\"app\"></div>",
        with: "<div id=\"app\">Vite server did not start</div>",
      )

    case simplifile.write(new_file_content, to: extension_dir <> "index.html") {
      Ok(_) -> io.println("Wrote " <> extension_dir <> "index.html")
      _ -> io.print_error("Could not write file.\n")
    }
  })
}
