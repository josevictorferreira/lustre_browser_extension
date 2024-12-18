import browser from 'webextension-polyfill'

export function openOptionsPage() {
  browser.runtime.openOptionsPage()
}

export function sendMessage(message, action) {
  browser.runtime.sendMessage({ action, message })
}

export async function sendMessageToTab(tabId, message, action) {
  try {
    await browser.tabs.sendMessage(tabId, { message, action })
    return true
  } catch (error) {
    return false
  }
}

export function listenMessage(action, callback) {
  browser.runtime.onMessage.addListener(function(message) {
    if (message.action === action) {
      return callback(message.message)
    }
  })
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
