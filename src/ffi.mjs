import browser from 'webextension-polyfill'

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
