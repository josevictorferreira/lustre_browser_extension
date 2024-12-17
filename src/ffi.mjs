import browser from 'webextension-polyfill'

export function openOptionsPage() {
  browser.runtime.openOptionsPage()
}

export function sendMessage(message, messageId) {
  browser.runtime.sendMessage({ messageId, message })
}

export function listenMessage(messageId, callback) {
  browser.runtime.onMessage.addListener((message) => {
    if (message.messageId === messageId) {
      callback(message.message)
    }
  })
}

export function sendMessageToTab(message, tabId, messageId) {
  browser.tabs.sendMessage(tabId, { messageId, message })
}

export function onInstalled(callback) {
  browser.runtime.onInstalled.addListener(callback)
}

export function tabsOnActivated(callback) {
  browser.tabs.onActivated.addListener(callback)
}

export async function getTabById(id) {
  try {
    const tab = await browser.tabs.get(id)
    return {
      title: tab.title
    };
  } catch (error) {
    throw new Error(error.message)
  }
}
