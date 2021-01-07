## odin.js

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/mrc-ide/odin.js/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/odin.js/actions)
[![codecov.io](https://codecov.io/github/mrc-ide/odin.js/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/odin.js?branch=master)
<!-- badges: end -->



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
## function (..., user = list(...), unused_user_action = NULL)
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


function integrateOdin(obj, times, y0, tcrit, atol, rtol, maxSteps) {
    var t0 = times[0];
    var t1 = times[times.length - 1];
    if (obj.metadata.interpolateTimes !== null) {
        tcrit = interpolateCheckT(times, obj.metadata.interpolateTimes, tcrit);
    }
    if (isMissing(y0)) {
      y0 = obj.initial(times[0]);
    }
    var rhs = function(t, y, dy) {
        obj.rhs(t, y, dy);
    };
    var ctl = {};
    if (tcrit !== null) {
        ctl.tcrit = tcrit;
    }
    if (!isMissing(atol)) {
        ctl.atol = atol;
    }
    if (!isMissing(rtol)) {
        ctl.rtol = rtol;
    }
    if (!isMissing(maxSteps)) {
        ctl.maxSteps = maxSteps;
    }
    var sol = null;
    if (typeof obj.output === "function") {
        var output = function(t, y) {
            return obj.output(t, y);
        }
        sol = dopri.integrate(rhs, y0, t0, t1, ctl, output);
    } else {
        sol = dopri.integrate(rhs, y0, t0, t1, ctl);
    }
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
            throw Error("Expected a numeric value for '" + name + "'");
        }
        if (min !== null && value < min) {
            throw Error("Expected '" + name + "' to be at least " + min);
        }
        if (max !== null && value > min) {
            throw Error("Expected '" + name + "' to be at most " + max);
        }
        if (isInteger && !numberIsInteger(value)) {
            throw Error("Expected '" + name + "' to be integer-like");
        }

        internal[name] = value;
    }
}


function getUserArray(user, name, internal, size, defaultValue,
                      min, max, isInteger) {
    var value = user[name];

    if (isMissing(value)) {
        if (isMissing(internal[name])) {
            if (defaultValue === null) {
                throw Error("Expected a value for '" + name + "'");
            } else {
                // not totally clear how to do this as we need to get
                // the previous dimensions too!
                throw Error("This needs implementing....");
                internal[name] = defaultValue;
            }
        }
        return;
    }

    var rank = size.length - 1;
    value = getUserArrayCheckType(value, name);
    getUserArrayCheckRank(rank, value.dim.length, name);

    for (var i = 0; i < rank; ++i) {
        if (value.dim[i] !== size[i + 1]) {
            if (rank == 1) {
                throw Error("Expected length " + size[i + 1] +
                            " value for '" + name + "'");
            } else {
                throw Error("Incorrect size of dimension " + (i + 1) +
                            " of '" + name + "' (expected " + size[i + 1] +
                            ")");
            }
        }
    }

    getUserArrayCheckContents(value.data, min, max, isInteger, name);

    internal[name] = value.data.slice();
}

// With arrays there are really two ways that they might come in; on
// the one hand we could accept json-style nested arrays, which is
// cool, but requires considerable checking.  Or, given we want
// C-style arrays ultimately we could use an R-style construct like:
// {"data": <array>, "dim": <array>}.  I'm doing the latter for now,
// and we can revisit later.
function getUserArrayDim(user, name, internal, size, defaultValue,
                         min, max, isInteger) {
    var value = user[name];

    if (isMissing(value)) {
        if (isMissing(internal[name])) {
            if (defaultValue === null) {
                throw Error("Expected a value for '" + name + "'");
            } else {
                // not totally clear how to do this as we need to get
                // the previous dimensions too!
                throw Error("This needs implementing....");
                internal[name] = defaultValue;
            }
        }
        return;
    }

    var rank = size.length - 1;
    value = getUserArrayCheckType(value, name);
    getUserArrayCheckRank(rank, value.dim.length, name);
    getUserArrayCheckContents(value.data, min, max, isInteger, name);

    var len = value.data.length;
    size[0] = len;
    for (var i = 0; i < rank; ++i) {
        size[i + 1] = value.dim[i];
    }

    internal[name] = value.data.slice();;
}


function getUserArrayCheckType(value, name) {
    if (Array.isArray(value)) {
        value = flattenArray(value, name)
    } else if (!(typeof value === "object" &&
                 "data" in value &&
                 "dim" in value)) {
        throw Error("Expected an odin.js array object for '" + name + "'");
    }
    return value;
}


function getUserArrayCheckRank(expected, given, name) {
    if (given !== expected) {
        if (expected === 1) {
            throw Error("Expected a numeric vector for '" + name + "'");
        } else if (expected === 2) {
            throw Error("Expected a numeric matrix for '" + name + "'");
        } else {
            throw Error("Expected a numeric array of rank " + expected +
                        " for '" + name + "'");
        }
    }
}


function getUserArrayCheckContents(data, min, max, isInteger, name) {
    for (var i = 0; i < data.length; ++i) {
        if (typeof data[i] !== "number") {
            throw Error("Expected a numeric value for '" + name + "'");
        }
        if (min !== null && data[i] < min) {
            throw Error("Expected '" + name + "' to be at least " + min);
        }
        if (max !== null && data[i] > min) {
            throw Error("Expected '" + name + "' to be at most " + max);
        }
        if (isInteger && !numberIsInteger(data[i])) {
            throw Error("Expected a integer value for '" + name + "'");
        }
    }
}


function checkUser(user, allowed, unusedUserAction) {
    if (unusedUserAction === undefined) {
        unusedUserAction = "stop";
    }
    if (unusedUserAction === "ignore") {
        return;
    }
    var err = setDifference(Object.keys(user), allowed);
    if (err.length > 0) {
        var msg = "Unknown user parameters: " + err.join(", ");

        if (unusedUserAction === "message") {
            odinMessage(msg);
        } else if (unusedUserAction === "warning") {
            odinWarning(msg);
        } else if (unusedUserAction === "stop") {
            throw Error(msg);
        } else {
            throw Error(msg + " (and invalid value for unusedUserAction)");
        }
    }
}


function isMissing(x) {
    return x === undefined || x === null ||
        (typeof x === "number" && isNaN(x));
}


// Travis has ancient v8 version that lacks Number.isNumber.  However
// it also lacks Number.EPSILON so I'm just comparing against 1e-8
// which is close enough to sqrt(double.eps) anyway
function numberIsInteger(x) {
    return Math.abs(x - Math.round(x)) < 1e-8
}


// O(n^2) but does not use Set, which is not available in old v8
function setDifference(a, b) {
  var result = [];
  for (var i = 0; i < a.length; i++) {
    if (b.indexOf(a[i]) === -1) {
      result.push(a[i]);
    }
  }
  return result;
}


// nice behaviour both in and out of R
function odinWarning(msg) {
    try {
        console.r.call("warning", msg)
    } catch (e) {
        console.warn(msg)
    }
}


function odinMessage(msg) {
    try {
        console.r.call("message", msg)
    } catch (e) {
        console.warn(msg)
    }
}


function flattenArray(value, name) {
    var len = 1;
    var dim = [];
    var x = value;
    while (Array.isArray(x)) {
        dim.push(x.length);
        len *= x.length;
        x = x[0];
    }
    dim.reverse();

    var data = flatten(value, []);

    // Not a suffient check, but at least a necessary one:
    if (len !== data.length) {
        throw Error("Inconsistent array for '" + name + '"');
    }

    return {data: data, dim: dim};
}


function flatten(array, result) {
  if (array.length === 0) {
    return result
  }
  var head = array[0]
  var rest = array.slice(1)
  if (Array.isArray(head)) {
    return flatten(head.concat(rest), result)
  }
  result.push(head)
  return flatten(rest, result)
}


// https://en.wikipedia.org/wiki/Rounding#Round_half_to_even - same
// behaviour as R and IEEE 754, with better biases.
function roundHalfToEven(x) {
    if (modr(x, 1) === 0.5) {
        return 2 * Math.round(x / 2);
    } else {
        return Math.round(x);
    }
}


function round2(x, digits) {
    if (digits === undefined) {
        return roundHalfToEven(x);
    } else {
        var mult = Math.pow(10, digits);
        return roundHalfToEven(x * mult) / mult;
    }
}

// modulo that conforms to (approximately) the same behaviour as R
function modr(x, y) {
    var tmp = x % y;
    if (tmp * y < 0) {
        tmp += y;
    }
    return tmp;
}


function intdivr(x, y) {
    return Math.floor(x / y);
}


// Ranks 2..8 created by scripts/create_support_sum_js.R
function odinSum1(x, from, to) {
    var tot = 0.0;
    for (var i = from; i < to; ++i) {
        tot += x[i];
    }
    return tot;
}
var odin = {};
odin.logistic = (function() {
  function logistic(user, unusedUserAction) {
    this.internal = {};
    var internal = this.internal;
    internal.K = 100;
    this.setUser(user, unusedUserAction);
  }
  logistic.prototype.setUser = function(user, unusedUserAction) {
    checkUser(user, ["r", "N0"], unusedUserAction);
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
  logistic.prototype.interpolateTime = null;
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
  logistic.prototype.run = function(times, y0, tcrit, atol, rtol, maxSteps) {
    return integrateOdin(this, times, y0, tcrit, atol, rtol, maxSteps);
  };
  logistic.prototype.coef = {r: {has_default: true, default: null, rank: 0, min: -Infinity, max: Infinity, integer: false}, N0: {has_default: false, default: 1, rank: 0, min: -Infinity, max: Infinity, integer: false}};
  logistic.prototype.updateMetadata = function() {
    this.metadata = {};
    var internal = this.internal;
    this.metadata.ynames = ["t", "N"];
    this.metadata.interpolateTimes = null;
    this.metadata.internalOrder = {
  "initial_N": null,
  "K": null,
  "N0": null,
  "r": null
};
    this.metadata.variableOrder = {
  "N": null
};
    this.metadata.outputOrder = null;
  };
  return logistic;
}());
```

(the option `include_dopri` prevents the [`dopri-js`](https://github.com/mrc-ide/dopri-js) solver being included here as that can be included separately and ends up being ~500 lines of extra code).

### Complete example

A more complete example that can actually be used from a webpage is included:


```r
path <- odin.js::odin_js_example("inst/models/sir.R", "simple")
dir(path)
```

```
## [1] "index.html" "odin.js"
```

Chrome will not let you execute js directly off disk (though I think Firefox does) so to test this you should use something like [SimpleHttpServer](https://docs.python.org/2/library/simplehttpserver.html) to serve the directory and access that way.  This example is also available at [mrc-ide.github.io/odin.js/simple](https://mrc-ide.github.io/odin.js/simple)

### Supported features

- [x] user variables
- [x] output
- [x] arrays
- [ ] delays
- [x] interpolation
- [x] discrete models
- [x] stochastic models

### Limitations and differences

* The `use_dde` option to the constructor has gone
* Minor error message changes
* Stochastic interface does not use R's RNG so results are different and seed setting is not obvious

---

Please note that the 'odin.js' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
