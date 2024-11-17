import * as $array from "../../../gleam_javascript/gleam/javascript/array.mjs";
import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import {
  showDirectoryPicker as show_directory_picker,
  getFileHandle as get_file_handle,
  allEntries as all_entries,
  showOpenFilePicker as show_open_file_picker,
  showSaveFilePicker as show_save_file_picker,
  getFile as get_file,
  createWritable as create_writable,
  write,
  close,
} from "../../file_system_ffi.mjs";
import * as $file from "../../plinth/browser/file.mjs";

export {
  all_entries,
  close,
  create_writable,
  get_file,
  get_file_handle,
  show_directory_picker,
  show_open_file_picker,
  show_save_file_picker,
  write,
};
