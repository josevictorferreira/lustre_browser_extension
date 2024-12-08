export function openOptionsPage() {
  chrome.runtime.openOptionsPage()
}

export function postMessage(message, to) {
  chrome.runtime.sendMessage({ to, message })
}

export function onMessage(address, callback) {
  chrome.runtime.onMessage.addListener((message) => {
    if (message.to === address) {
      callback(message.message)
    }
  })
}
