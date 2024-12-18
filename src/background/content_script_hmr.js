const forbiddenProtocols = [
  "chrome-extension://",
  "chrome-search://",
  "chrome://",
  "devtools://",
  "edge://",
  "https://chrome.google.com/webstore",
];

function isForbiddenUrl(url) {
  return forbiddenProtocols.some((protocol) => url.startsWith(protocol));
}

const isFirefox = navigator.userAgent.includes("Firefox");

browser.webNavigation.onCommitted.addListener(({ tabId, frameId, url }) => {
  if (frameId !== 0) return;

  if (isForbiddenUrl(url)) return;

  browser.tabs
    .executeScript(tabId, {
      file: `${isFirefox ? "" : "."}/dist/content_scripts/index.global.js`,
      runAt: "document_end",
    })
    .catch((error) => console.error(error));
});
