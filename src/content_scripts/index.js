import { main } from './content_script.gleam'
import "../styles/app.css"

(() => {
  const browser = chrome || browser;
  console.log(`${__NAME__} content script loaded`);

  const container = document.createElement("div");
  container.id = __NAME__;

  const root = document.createElement("div");
  const rootId = `${__NAME__}-root`;
  root.id = rootId;

  const styleUrl = browser.runtime.getURL("dist/content_scripts/style.css")

  fetch(styleUrl)
    .then(response => response.text())
    .then(css => {
      const style = document.createElement('style');
      style.textContent = css;
      container.appendChild(style);
      container.appendChild(root);
      document.body.appendChild(container);
      main(`#${rootId}`);
    });
})();
