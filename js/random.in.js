global.random = require("random");
// Configure sensible drop-in replacements for R's functions:
global.random.unifRand = random.uniform();
global.random.unifNorm = random.normal();
global.random.unifExp = random.exponential();
