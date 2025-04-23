import { main } from "./output/Main/index.js";

if (module.hot) {
  module.hot.accept(() => {
    main();
  });
}
main();