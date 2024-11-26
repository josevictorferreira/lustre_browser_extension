import fs from "fs-extra";
import toml from 'toml'
import { resolve } from "node:path";
import process from "node:process";

function get_port() {
  if (process.env.PORT) {
    return Number(process.env.PORT || "") || 3000;
  }
  return 3000;
}

function is_dev() {
  if (process.env.NODE_ENV === 'development') {
    return true;
  }
  return false
}

function get_name() {
  const tomlFile = fs.readFileSync(r('gleam.toml'), 'utf8');
  const tomlData = toml.parse(tomlFile);

  return tomlData.name;
}

export const r = (...args) => resolve(__dirname, '..', ...args);

// export const port = get_port();
export const isDev = process.env.NODE_ENV !== "production";
export const port = Number(process.env.PORT || "") || 3303;
export const isFirefox = process.env.EXTENSION === "firefox";
//
// export const isDev = is_dev();
//
export const name = get_name();
