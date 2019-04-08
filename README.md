## odin.js

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Travis-CI Build Status](https://travis-ci.org/mrc-ide/odin.js.svg?branch=master)](https://travis-ci.org/mrc-ide/odin.js)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/7o66jpuibiy6havb?svg=true)](https://ci.appveyor.com/project/richfitz/odin-js)
[![codecov.io](https://codecov.io/github/mrc-ide/odin.js/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/odin.js?branch=master)



Support for compiling [odin](https://github.com/mrc-ide/odin) models to javascript.

### Use from R

The function `odin.js::odin_js` provides a (roughly drop-in) replacement for `odin::odin` for compiling an odin model for use within R:


```r
logistic <- odin.js::odin_js({
  deriv(N) <- r * N * (1 - N / K)
  initial(N) <- N0
  N0 <- user(1)
  K <- 100
  r <- user()
})
logistic
```

```
## function (..., user = list(...))
## <an 'odin_generator' function>
##   use coef() to get information on user parameters
```

```r
logistic(r = 1)$run(0:10)
```

```
##        t         N
##  [1,]  0  1.000000
##  [2,]  1  2.672363
##  [3,]  2  6.945318
##  [4,]  3 16.866483
##  [5,]  4 35.546124
##  [6,]  5 59.986009
##  [7,]  6 80.295740
##  [8,]  7 91.719886
##  [9,]  8 96.785679
## [10,]  9 98.792996
## [11,] 10 99.552555
```

This might be slightly faster than the odin-to-R compiler, though benchmarks will have to wait until `odin.js` supports more odin features.

### Use from webpages

The function `odin.js::odin_js_bundle` generates a "bundle" of javascript suitable for calling from a webpage.  This is the same code that `odin.js::odin_js` but without R-wrappers around it.  It takes a vector of filenames and produces a file of javascript code:


```r
path <- odin.js::odin_js_bundle("inst/models/logistic.R",
                                include_dopri = FALSE)
```

The file at `path` now contains:

```js
// V8 does not support Array.fill from the look of it
function zeros(n) {
    var ret = new Array(n);
    for (var i = 0; i < n; ++i) {
        ret[i] = 0;
    }
    return ret;
}

function integrateOdin(obj, times, y0) {
    var t0 = times[0];
    var t1 = times[times.length - 1];
    if (isMissing(y0)) {
      y0 = obj.initial(times[0]);
    }
    var rhs = function(t, y, dy) {
        obj.rhs(t, y, dy);
    };
    var sol = dopri.integrate(rhs, y0, t0, t1);
    var y = sol(times);
    // Prepend the result vector with the times; this is going to be
    // required later on - it would be nice if dopri did this through
    // it's interpolation function though.
    for (var i = 0; i < times.length; ++i) {
        y[i].unshift(times[i]);
    }
    return {"y": y, "names": obj.metadata.ynames.slice(0)};
}

function getUser(user, name, internal, size, defaultValue,
                  min, max, isInteger) {
    var value = user[name];
    if (size !== null) {
        throw Error("Arrays not yet supported");
    }
    if (isMissing(value)) {
        if (isMissing(internal[name])) {
            if (defaultValue === null) {
                throw Error("Expected a value for '" + name + "'");
            } else {
                internal[name] = defaultValue;
            }
        }
    } else {
        if (typeof value !== "number") {
            throw Error("Expected a number for '" + name + "'");
        }
        if (min !== null && value < min) {
            throw Error("Expected '" + name + "' to be at least " + min);
        }
        if (max !== null && value > min) {
            throw Error("Expected '" + name + "' to be at most " + max);
        }
        internal[name] = value;
    }
}

function isMissing(x) {
    return x === undefined || x === null;
}
var odin = {};
odin.logistic = (function() {
  function logistic(user) {
    this.internal = {};
    var internal = this.internal;
    internal.K = 100;
    this.setUser(user);
  }
  logistic.prototype.setUser = function(user) {
    var internal = this.internal;
    getUser(user, "N0", internal, null, 1, null, null, false);
    getUser(user, "r", internal, null, null, null, null, false);
    internal.initial_N = internal.N0;
    this.updateMetadata();
  };
  logistic.prototype.rhs = function(t, state, dstatedt) {
    var internal = this.internal;
    var N = state[0];
    dstatedt[0] = internal.r * N * (1 - N / internal.K);
  };
  logistic.prototype.rhsEval = function(t, state) {
    var dstatedt = zeros(state.length);
    this.rhs(t, state, dstatedt);
    return dstatedt;
  };
  logistic.prototype.initial = function(t) {
    var internal = this.internal;
    var state = zeros(1);
    state[0] = internal.initial_N;
    return state;
  };
  logistic.prototype.run = function(times, y0) {
    return integrateOdin(this, times, y0);
  };
  logistic.prototype.updateMetadata = function() {
    this.metadata = {};
    this.metadata.ynames = ["t", "N"];
  };
  return logistic;
}());
```

(the option `include_dopri` prevents the [`dopri-js`](https://github.com/mrc-ide/dopri-js) solver being included here as that can be included separately and ends up being ~500 lines of extra code).

### Supported features

- [x] user variables
- [ ] output
- [ ] arrays
- [ ] delays
- [ ] interpolation
- [ ] discrete models
- [ ] stochastic models (this might be hard)

---

Please note that the 'odin.js' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
