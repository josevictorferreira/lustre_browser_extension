import plinth/javascript/storage

pub fn get_item(key: String) -> Result(String, Nil) {
  let assert Ok(local_storage) = storage.local()
  storage.get_item(local_storage, key)
}

pub fn set_item(key: String, value: String) {
  let assert Ok(local_storage) = storage.local()
  let assert Ok(_) = storage.set_item(local_storage, key, value)
  Nil
}
