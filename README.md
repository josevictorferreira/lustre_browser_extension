# Lustre Browser Extension

A modern boilerplate for creating cross-browser extensions using [Gleam](https://gleam.run) and [Lustre](https://lustre.build) with [Vite](https://vitejs.dev). Build extensions that work seamlessly on both Chrome and Firefox using a single codebase.

## Credits

This project is based on [vitesse-webext](https://github.com/antfu/vitesse-webext), adapted for use with Gleam and Lustre.

## Features

- 🚀 Cross-browser support for Chrome and Firefox
- ⚡️ Vite-powered build system
- 🌟 [Gleam](https://gleam.run) and [Lustre](https://lustre.build) for type-safe, functional programming
- 🎨 [Tailwind CSS](https://tailwindcss.com) for styling
- 🔥 Hot Module Replacement (HMR)
- 📦 Optimized builds
- 🧩 Extension manifest v3 support

## Getting Started

### Prerequisites

- [Node.js](https://nodejs.org/) (v14 or higher)
- [Gleam](https://gleam.run/getting-started/installing/)
- [pnpm](https://pnpm.io/installation) (recommended) or npm

### Installation

1. Clone this repository:
```bash
git clone https://github.com/josevictorferreira/lustre_browser_extension.git
cd lustre-browser-extension
```

2. Copy the and edit `.env` with defaults for building the project:
```bash
cp .example.env .env
```

3. Install dependencies:
```bash
pnpm install
gleam install
```

4. Start development server:
```bash
pnpm dev
```

### Development

The boilerplate supports different development modes:

```bash
# Development mode with HMR for Chrome
pnpm dev
# Development mode with HMR for Firefox
pnpm dev-firefox

# Build extension for production
pnpm build

# Build for Firefox
pnpm build-firefox
```

### Project Structure

```
├── src/
│   ├── assets/                # Assets to be used inside the browser extension
│   ├── background/            # Background scripts
│   ├── content_scripts/       # Content scripts
│   ├── components/            # Lustre shared view components
│   ├── options/               # Options page
│   ├── popup/                 # Popup page
│   ├── sidepanel/             # Sidepanel page
│   ├── lib/                   # Shared gleam libraries
│   ├── scripts/               # Useful gleam scripts
│   └── styles/                # Main css files
├── extension/                 # The output dir where your files will be build
│   └── assets/                # Assets to be used inside the browser extension manifest.json
├── vite.config.background.mjs # Vite configuration with defaults for building the background service
├── vite.config.content.mjs    # Vite configuration with defaults for building content scripts
└── vite.config.mjs            # Vite configuration
```

## Loading the Extension

### Chrome

1. Go to `chrome://extensions/`
2. Enable "Developer mode"
3. Click "Load unpacked"
4. Select the `extension` directory

### Firefox

1. Go to `about:debugging#/runtime/this-firefox`
2. Click "Load Temporary Add-on"
3. Select any file in the `extension` directory

## Customization

### Manifest Configuration

Edit `src/scripts/manifest.gleam` to customize your extension's manifest, edit it accordingly with your needs.

### Styling

This boilerplate uses Tailwind CSS for styling. Configure Tailwind in `tailwind.config.mjs`:

```javascript
module.exports = {
  content: ["./src/**/*.{gleam,html}"],
  // ... other configuration
}
```

## Acknowledgments

- [Vitesse WebExt](https://github.com/antfu/vitesse-webext)
- [Gleam](https://gleam.run)
- [Lustre](https://lustre.build)
- [Vite](https://vitejs.dev)
- [Tailwind CSS](https://tailwindcss.com)
