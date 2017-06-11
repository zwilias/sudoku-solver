const Elm = require('./elm.js');

const app = Elm.App.worker();

app.ports.emit.subscribe((msg) => {
  console.log(msg);
});
