import { main } from './content_script.gleam'
import "../styles/app.css"

(() => {
  const container = document.createElement("div");
  container.id = __NAME__;
  const root = document.createElement("div");
  const rootId = `${__NAME__}-root`;
  root.id = rootId;
  const styleEl = document.createElement("link");
  const shadowDOM =
    container.attachShadow?.({ mode: __DEV__ ? "open" : "closed" }) ||
    container;
  styleEl.setAttribute("rel", "stylesheet");
  styleEl.setAttribute(
    "href",
    browser.runtime.getURL("dist/content_scripts/style.css"),
  );
  shadowDOM.appendChild(styleEl);
  shadowDOM.appendChild(root);
  document.body.appendChild(container);

  main(rootId)
})();
