importScripts('js.bc.js');

console.log('libASL_web js-side:', libASL_web);

libASL_web.go();
console.log('libASL_web worker ready');

onmessage = function(e) {
  const fn = libASL_web[e.data[0]];
  console.assert(fn, "libASL_web function not found: " + e.data);
  fn(...e.data[1]);
};
