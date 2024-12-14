const browser = chrome || browser;

export function openOptionsPage() {
  browser.runtime.openOptionsPage()
}

export function postMessage(message, to) {
  browser.runtime.sendMessage({ to, message })
}

export function onMessage(address, callback) {
  browser.runtime.onMessage.addListener((message) => {
    if (message.to === address) {
      callback(message.message)
    }
  })
}

export function onInstalled(callback) {
  browser.runtime.onInstalled.addListener(callback)
}

export function tabsOnActivated(callback) {
  browser.tabs.onActivated.addListener(callback)
}

export function getTabById(id) {
  return new Promise((resolve, reject) => {
    try {
      browser.tabs.get(id, (tab) => {
        if (browser.runtime.lastError) {
          reject(new Error(browser.runtime.lastError.message));
          return;
        }
        resolve({
          title: tab.title
        });
      });
    } catch (error) {
      reject(error)
    }
  })
}
