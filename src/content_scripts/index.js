import { main } from './content_script.gleam'
import "../styles/app.css"

(() => {
  const browser = chrome || browser;
  console.log(`${__NAME__} content script loaded`);

  const container = document.createElement("div");
  container.id = `${__NAME__}-container`;
  const root = document.createElement("div");
  const styleEl = document.createElement("link");
  const shadowDOM =
    container.attachShadow?.({ mode: __DEV__ ? "open" : "closed" }) ||
    container;
  styleEl.setAttribute("rel", "stylesheet");
  styleEl.setAttribute(
    "href",
    browser.runtime.getURL("dist/content_scripts/style.css"),
  );
  const rootId = `${__NAME__}`;
  root.id = rootId;
  shadowDOM.appendChild(styleEl);
  shadowDOM.appendChild(root);
  document.body.appendChild(container);

  main(root);
})();
