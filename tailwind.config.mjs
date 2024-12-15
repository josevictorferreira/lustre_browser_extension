import { name } from './scripts/utils.js'

export default {
  content: ['./src/**/*.{js,html,gleam}'],
  theme: {
    extend: {},
  },
  plugins: [],
  important: `#${name}`,
  corePlugins: {
    preflight: false,
  }
}
