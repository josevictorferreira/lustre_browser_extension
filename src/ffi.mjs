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
