// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
// import "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "./vendor/some-package.js"
//
// Alternatively, you can `npm install some-package` and import
// them using a path starting with the package name:
//
//     import "some-package"
//

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html";
// Establish Phoenix Socket and LiveView configuration.
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";
import Alpine from "../vendor/alpine";

import p5 from "../vendor/p5";
import Hydra from "../vendor/hydra-synth.js";

window.Alpine = Alpine;
Alpine.start();

let csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

let liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
  hooks: {
    // existing hooks ...
  },
  dom: {
    onBeforeElUpdated(from, to) {
      if (from._x_dataStack) {
        window.Alpine.clone(from, to);
      }
    },
  },
});

// Show progress bar on live navigation and form submits
topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (info) => topbar.show());
window.addEventListener("phx:page-loading-stop", (info) => topbar.hide());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// liveSocket.enableDebug();
// liveSocket.enableLatencySim(1000); // enabled for duration of browser session
// liveSocket.disableLatencySim();
// window.liveSocket = liveSocket;

const p5sketchMod = (p) => {
  let x = 0;
  let value = "rgba(0, 0, 0, 0.1)";
  p.setup = function () {
    p.frameRate(30);
    p.createCanvas(p.windowWidth, p.windowHeight);
    p.rectMode(p.CENTER);
  };

  p.draw = function () {
    x += 0.007;
    p.clear();
    p.background("rgba(0,0,0,0)");
    p.fill(value);
    p.translate(p.windowHeight / 16, p.windowHeight / 16);
    p.rotate(x);
    p.rect(0, 0, p.windowHeight / 16, p.windowHeight / 16);
  };

  p.mouseClicked = function () {
    console.log(value);
    if (value !== "rgba(0, 0, 0, 0.1)") {
      value = "rgba(0, 0, 0, 0.1)";
    } else {
      value = "rgba(1, 1, 1, 0.5)";
    }
  };

  p.windowResized = function () {
    p.resizeCanvas(p.windowWidth, p.windowHeight);
  };
};

let p5sketch = new p5(p5sketchMod, "p5sketch");
window.p5sketch = p5sketch;

const hydra = new Hydra({
  makeGlobal: false,
  detectAudio: false,
  canvas: window.document.getElementById("hydra"),
}).synth;

// hydra.s0.init({ src: p5sketch.canvas });
window.hydra = hydra;
hydra.setResolution(window.innerWidth, window.innerHeight);

window.addEventListener(
  "resize",
  function () {
    hydra.setResolution(window.innerWidth, window.innerHeight);
  },
  true
);

window.addEventListener(`phx:hydra-code`, (e) => {
  // run eval in indirect mode to make it more palatable to esbuild
  try {
    (0, eval)(e.detail.hydra_code);
  } catch (e) {
    console.log(e.message);
  }
});

