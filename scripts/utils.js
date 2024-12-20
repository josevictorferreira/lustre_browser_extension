import fs from "fs-extra";
import toml from 'toml'
import { resolve } from "node:path";
import process from "node:process";

function get_name() {
  const tomlFile = fs.readFileSync(r('gleam.toml'), 'utf8');
  const tomlData = toml.parse(tomlFile);

  return tomlData.name;
}

export const r = (...args) => resolve(__dirname, '..', ...args);

export const isDev = process.env.NODE_ENV !== "production";
export const port = Number(process.env.PORT || "") || 1234;
export const isFirefox = process.env.EXTENSION === "firefox";
export const name = get_name();
