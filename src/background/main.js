import { main } from './background.gleam'

if (import.meta.hot) {
  import("/@vite/client");
  import("./contentScriptHMR");
}

main()
