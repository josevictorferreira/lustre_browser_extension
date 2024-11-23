import simplifile

pub fn ensure_dir(dirpath: String) -> Bool {
  case simplifile.is_directory(dirpath) {
    Ok(True) -> True
    _ -> {
      case simplifile.create_directory_all(dirpath) {
        Ok(_) -> True
        _ -> False
      }
    }
  }
}
