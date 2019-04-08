(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
(function (global){
global.dopri = require('dopri');

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"dopri":6}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function dopriControl(control) {
    if (control === void 0) { control = {}; }
    var defaults = { atol: 1e-6, maxSteps: 10000, rtol: 1e-6,
        stiffCheck: 0 };
    var ret = {
        atol: withDefault(control.atol, defaults.atol),
        maxSteps: withDefault(control.maxSteps, defaults.maxSteps),
        rtol: withDefault(control.rtol, defaults.rtol),
        stiffCheck: withDefault(control.stiffCheck, defaults.stiffCheck),
    };
    if (ret.maxSteps < 1) {
        throw controlError("maxSteps", "must be at least 1");
    }
    if (ret.atol <= 0) {
        throw controlError("atol", "must be strictly positive");
    }
    if (ret.rtol <= 0) {
        throw controlError("rtol", "must be strictly positive");
    }
    return ret;
}
exports.dopriControl = dopriControl;
function controlError(nm, message) {
    return new Error("Invalid control parameter: '" + nm + "' " + message);
}
function withDefault(x, y) {
    return x === undefined ? y : x;
}

},{}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var control_1 = require("./control");
var dopri5 = require("./dopri5/stepper");
var interpolator = require("./interpolator");
var utils = require("./utils");
// needed for ES5 - will be ~= Number.EPSILON in ES6
var DBL_EPSILON = Math.pow(2, -52); // = 2.220446049250313e-16
var STEP_FACTOR_MIN = 1e-4;
function integrate(rhs, y, t0, t1) {
    var solver = new Dopri(rhs, y.length);
    solver.initialise(t0, y);
    return solver.run(t1);
}
exports.integrate = integrate;
function integrationError(message, t) {
    return new Error("Integration failure: " + message + " at " + t);
}
var Dopri = /** @class */ (function () {
    function Dopri(rhs, n, control) {
        if (control === void 0) { control = {}; }
        this._t = 0.0;
        this._h = 0.0;
        // state
        this._nSteps = 0;
        this._nStepsAccepted = 0;
        this._nStepsRejected = 0;
        this._stiffNStiff = 0;
        this._stiffNNonstiff = 0;
        this._lastError = 0;
        this._stepper = new dopri5.Dopri5(rhs, n);
        this._control = control_1.dopriControl(control);
    }
    Dopri.prototype.initialise = function (t, y) {
        var n = this._stepper.n;
        if (y.length !== n) {
            throw Error("Invalid size 'y' - expected a length " + n + " array");
        }
        this._stepper.reset(y);
        this._reset();
        this._h = this._stepper.initialStepSize(t, this._control.atol, this._control.rtol);
        this._t = t;
        return this;
    };
    Dopri.prototype.run = function (tEnd) {
        var ret = new interpolator.Interpolator(this._stepper);
        while (this._t < tEnd) {
            this._step();
            ret.add(this._stepper.history);
        }
        return function (t) { return ret.interpolate(t); };
    };
    Dopri.prototype.statistics = function () {
        return {
            lastError: this._lastError,
            nEval: this._stepper.nEval,
            nSteps: this._nSteps,
            nStepsAccepted: this._nStepsAccepted,
            nStepsRejected: this._nStepsRejected,
            stiffNNonstiff: this._stiffNNonstiff,
            stiffNStiff: this._stiffNStiff,
        };
    };
    Dopri.prototype._reset = function () {
        this._nSteps = 0;
        this._nStepsAccepted = 0;
        this._nStepsRejected = 0;
        this._stiffNStiff = 0;
        this._stiffNNonstiff = 0;
        this._lastError = 0;
    };
    Dopri.prototype._step = function () {
        var t = this._t;
        var h = this._h;
        var success = false;
        var reject = false;
        var facOld = Math.max(this._lastError, 1e-4);
        var stepControl = this._stepper.stepControl;
        while (!success) {
            if (this._nSteps > this._control.maxSteps) {
                throw integrationError("too many steps", t);
            }
            if (h < this._stepper.stepControl.sizeMin) {
                throw integrationError("step too small", t);
            }
            if (h <= Math.abs(t) * DBL_EPSILON) {
                throw integrationError("step size vanished", t);
            }
            // Carry out the step
            this._stepper.step(t, h);
            this._nSteps++;
            // Error estimation
            var err = this._stepper.error(this._control.atol, this._control.rtol);
            var fac11 = Math.pow(err, stepControl.constant);
            var facc1 = 1.0 / stepControl.factorMin;
            var facc2 = 1.0 / stepControl.factorMax;
            if (err <= 1) {
                success = true;
                this._nStepsAccepted++;
                if (this._isStiff(h)) {
                    throw integrationError("problem became stiff", t);
                }
                this._stepper.stepComplete(t, h);
                var fac = fac11 / Math.pow(facOld, stepControl.beta);
                fac = utils.constrain(fac / stepControl.factorSafe, facc2, facc1);
                var hNew = h / fac;
                this._t += h;
                this._h = reject ? Math.min(hNew, h) : hNew;
                this._lastError = err;
            }
            else {
                reject = true;
                if (this._nStepsAccepted >= 1) {
                    this._nStepsRejected++;
                }
                h /= Math.min(facc1, fac11 / stepControl.factorSafe);
            }
        }
        return this._t;
    };
    Dopri.prototype._isStiff = function (h) {
        var check = this._stiffNStiff > 0 ||
            this._nStepsAccepted % this._control.stiffCheck === 0;
        if (check) {
            if (this._stepper.isStiff(h)) {
                this._stiffNNonstiff = 0;
                if (this._stiffNStiff++ >= 15) {
                    return true;
                }
            }
            else if (this._stiffNStiff > 0) {
                if (this._stiffNNonstiff++ >= 6) {
                    this._stiffNStiff = 0;
                    this._stiffNNonstiff = 0;
                }
            }
        }
        return false;
    };
    return Dopri;
}());
exports.Dopri = Dopri;

},{"./control":2,"./dopri5/stepper":5,"./interpolator":7,"./utils":8}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Dopri5StepControl = /** @class */ (function () {
    function Dopri5StepControl() {
        // Essentially unlimited step size
        this.sizeMin = 1e-8; // should be Number.EPSILON, really
        this.sizeMax = Number.MAX_VALUE;
        // For scaling during adaptive stepping
        this.factorSafe = 0.9;
        this.factorMin = 0.2; // from dopri5.f:276, retard.f:328
        this.factorMax = 10.0; // from dopri5.f:281, retard.f:333
        this.beta = 0.04;
        this.constant = 0.2 - this.beta * 0.75;
    }
    return Dopri5StepControl;
}());
exports.Dopri5StepControl = Dopri5StepControl;

},{}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var utils = require("../utils");
var control = require("./control");
// Heaps of constants!
var C2 = 0.2;
var C3 = 0.3;
var C4 = 0.8;
var C5 = 8.0 / 9.0;
var A21 = 0.2;
var A31 = 3.0 / 40.0;
var A32 = 9.0 / 40.0;
var A41 = 44.0 / 45.0;
var A42 = -56.0 / 15.0;
var A43 = 32.0 / 9.0;
var A51 = 19372.0 / 6561.0;
var A52 = -25360.0 / 2187.0;
var A53 = 64448.0 / 6561.0;
var A54 = -212.0 / 729.0;
var A61 = 9017.0 / 3168.0;
var A62 = -355.0 / 33.0;
var A63 = 46732.0 / 5247.0;
var A64 = 49.0 / 176.0;
var A65 = -5103.0 / 18656.0;
var A71 = 35.0 / 384.0;
var A73 = 500.0 / 1113.0;
var A74 = 125.0 / 192.0;
var A75 = -2187.0 / 6784.0;
var A76 = 11.0 / 84.0;
var E1 = 71.0 / 57600.0;
var E3 = -71.0 / 16695.0;
var E4 = 71.0 / 1920.0;
var E5 = -17253.0 / 339200.0;
var E6 = 22.0 / 525.0;
var E7 = -1.0 / 40.0;
// ---- DENSE OUTPUT OF SHAMPINE (1986)
var D1 = -12715105075.0 / 11282082432.0;
var D3 = 87487479700.0 / 32700410799.0;
var D4 = -10690763975.0 / 1880347072.0;
var D5 = 701980252875.0 / 199316789632.0;
var D6 = -1453857185.0 / 822651844.0;
var D7 = 69997945.0 / 29380423.0;
// most of this can be made really quit private - everything not used
// in the interface really.
var Dopri5 = /** @class */ (function () {
    function Dopri5(rhs, n) {
        this.order = 5;
        this.stepControl = new control.Dopri5StepControl();
        this.nEval = 0;
        this.rhs = rhs;
        this.n = n;
        this.y = new Array(n);
        this.yNext = new Array(n);
        this.yStiff = new Array(n);
        this.k1 = new Array(n);
        this.k2 = new Array(n);
        this.k3 = new Array(n);
        this.k4 = new Array(n);
        this.k5 = new Array(n);
        this.k6 = new Array(n);
        this.history = new Array(this.order * n + 2);
    }
    // This is the ugliest function - quite a lot goes on in here to
    // do the full step
    Dopri5.prototype.step = function (t, h) {
        var n = this.n;
        var y = this.y;
        var yNext = this.yNext;
        var k1 = this.k1;
        var k2 = this.k2;
        var k3 = this.k3;
        var k4 = this.k4;
        var k5 = this.k5;
        var k6 = this.k6;
        var history = this.history;
        var i = 0;
        for (i = 0; i < n; ++i) { // 22
            yNext[i] = y[i] + h * A21 * k1[i];
        }
        this.rhs(t + C2 * h, yNext, k2);
        for (i = 0; i < n; ++i) { // 23
            yNext[i] = y[i] + h * (A31 * k1[i] + A32 * k2[i]);
        }
        this.rhs(t + C3 * h, yNext, k3);
        for (i = 0; i < n; ++i) { // 24
            yNext[i] = y[i] + h * (A41 * k1[i] + A42 * k2[i] + A43 * k3[i]);
        }
        this.rhs(t + C4 * h, yNext, k4);
        for (i = 0; i < n; ++i) { // 25
            yNext[i] = y[i] + h * (A51 * k1[i] + A52 * k2[i] + A53 * k3[i] +
                A54 * k4[i]);
        }
        this.rhs(t + C5 * h, yNext, k5);
        for (i = 0; i < n; ++i) { // 26
            this.yStiff[i] = y[i] + h * (A61 * k1[i] + A62 * k2[i] +
                A63 * k3[i] + A64 * k4[i] +
                A65 * k5[i]);
        }
        var tNext = t + h;
        this.rhs(tNext, this.yStiff, k6);
        for (i = 0; i < n; ++i) { // 27
            yNext[i] = y[i] + h * (A71 * k1[i] + A73 * k3[i] + A74 * k4[i] +
                A75 * k5[i] + A76 * k6[i]);
        }
        this.rhs(tNext, yNext, k2);
        var j = 4 * n;
        for (i = 0; i < n; ++i) {
            history[j++] = h * (D1 * k1[i] + D3 * k3[i] + D4 * k4[i] +
                D5 * k5[i] + D6 * k6[i] + D7 * k2[i]);
        }
        for (i = 0; i < n; ++i) {
            k4[i] = h * (E1 * k1[i] + E3 * k3[i] + E4 * k4[i] +
                E5 * k5[i] + E6 * k6[i] + E7 * k2[i]);
        }
        this.nEval += 6;
    };
    Dopri5.prototype.stepComplete = function (t, h) {
        this.saveHistory(t, h);
        utils.copyArray(this.k1, this.k2); // k1 <== k2
        utils.copyArray(this.y, this.yNext); // y  <== yNext
    };
    Dopri5.prototype.saveHistory = function (t, h) {
        var history = this.history;
        var n = this.n;
        for (var i = 0; i < n; ++i) {
            var ydiff = this.yNext[i] - this.y[i];
            var bspl = h * this.k1[i] - ydiff;
            history[i] = this.y[i];
            history[n + i] = ydiff;
            history[2 * n + i] = bspl;
            history[3 * n + i] = -h * this.k2[i] + ydiff - bspl;
        }
        history[this.order * n] = t;
        history[this.order * n + 1] = h;
    };
    Dopri5.prototype.error = function (atol, rtol) {
        var err = 0.0;
        var i = 0;
        for (i = 0; i < this.n; ++i) {
            var sk = atol + rtol *
                Math.max(Math.abs(this.y[i]), Math.abs(this.yNext[i]));
            err += utils.square(this.k4[i] / sk);
        }
        return Math.sqrt(err / this.n);
    };
    // It might be worth doing an optional argument history and then
    // pulling in this.history if it's not present
    Dopri5.prototype.interpolate = function (t, history) {
        var n = this.n;
        var tStep = history[this.order * n];
        var hStep = history[this.order * n + 1];
        var theta = (t - tStep) / hStep;
        var theta1 = 1 - theta;
        var ret = new Array(n);
        for (var i = 0; i < n; ++i) {
            ret[i] =
                history[i] + theta *
                    (history[n + i] + theta1 *
                        (history[2 * n + i] + theta *
                            (history[3 * n + i] + theta1 *
                                history[4 * n + i])));
        }
        return ret;
    };
    Dopri5.prototype.isStiff = function (h) {
        var stnum = 0.0;
        var stden = 0.0;
        for (var i = 0; i < this.n; ++i) {
            stnum += utils.square(this.k2[i] - this.k6[i]);
            stden += utils.square(this.yNext[i] - this.yStiff[i]);
        }
        return stden > 0 && Math.abs(h) * Math.sqrt(stnum / stden) > 3.25;
    };
    Dopri5.prototype.initialStepSize = function (t, atol, rtol) {
        var stepSizeMax = this.stepControl.sizeMax;
        // NOTE: This is destructive with respect to most of the information
        // in the dataect; in particular k2, k3 will be modified.
        var f0 = this.k1;
        var f1 = this.k2;
        var y1 = this.k3;
        // Compute a first guess for explicit Euler as
        //   h = 0.01 * norm (y0) / norm (f0)
        // the increment for explicit euler is small compared to the solution
        this.rhs(t, this.y, f0);
        this.nEval++;
        var normF = 0.0;
        var normY = 0.0;
        var i = 0;
        for (i = 0; i < this.n; ++i) {
            var sk = atol + rtol * Math.abs(this.y[i]);
            normF += utils.square(f0[i] / sk);
            normY += utils.square(this.y[i] / sk);
        }
        var h = (normF <= 1e-10 || normF <= 1e-10) ?
            1e-6 : Math.sqrt(normY / normF) * 0.01;
        h = Math.min(h, stepSizeMax);
        // Perform an explicit Euler step
        for (i = 0; i < this.n; ++i) {
            y1[i] = this.y[i] + h * f0[i];
        }
        this.rhs(t + h, y1, f1);
        this.nEval++;
        // Estimate the second derivative of the solution:
        var der2 = 0.0;
        for (i = 0; i < this.n; ++i) {
            var sk = atol + rtol * Math.abs(this.y[i]);
            der2 += utils.square((f1[i] - f0[i]) / sk);
        }
        der2 = Math.sqrt(der2) / h;
        // Step size is computed such that
        //   h^order * max(norm(f0), norm(der2)) = 0.01
        var der12 = Math.max(Math.abs(der2), Math.sqrt(normF));
        var h1 = (der12 <= 1e-15) ?
            Math.max(1e-6, Math.abs(h) * 1e-3) :
            Math.pow(0.01 / der12, 1.0 / this.order);
        h = Math.min(Math.min(100 * Math.abs(h), h1), stepSizeMax);
        return h;
    };
    Dopri5.prototype.reset = function (y) {
        for (var i = 0; i < this.n; ++i) {
            this.y[i] = y[i];
        }
        this.nEval = 0;
    };
    return Dopri5;
}());
exports.Dopri5 = Dopri5;

},{"../utils":8,"./control":4}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var dopri_1 = require("./dopri");
exports.Dopri = dopri_1.Dopri;
exports.integrate = dopri_1.integrate;

},{"./dopri":3}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// We could take either a stepper or the interpolation function here.
// The advantage of the former is that we can sensibly implement
// interpolation of single variables eventually.
// We could also take responsibility for doing the actual collection
// of times - that would be possibly worth doing, as then we could
// find indexes with a binary search e.g., z.find(x => x > 2.2)
// export type interpolate = (t: number, y: number[]) => number[];
// This is going to need some work if we ever want to support
// extending a history and continuing.
var Interpolator = /** @class */ (function () {
    function Interpolator(stepper) {
        this.stepper = stepper;
        this.history = [];
    }
    Interpolator.prototype.add = function (h) {
        this.history.push(h.slice(0));
    };
    Interpolator.prototype.interpolate = function (t) {
        var y = [];
        // TODO: validate that 't' is increasing and fits within
        // integration time.
        var h = this.history;
        // TODO: Time is currently saved into the second to last
        // element but this would be far better as a small structure
        // probably. e.g., {t0, h, t1, history}.  It's also possible
        // that we could do better with some sort of "history
        // container" object.
        //
        // Probably hold off doing most of that until (a) this is
        // known to work and (b) I work out how the delay lookup will
        // work as that needs to feed into the derivative function in
        // odd ways.
        //
        // TODO: need to protect us from walking off the end of the
        // array (or starting within it).
        //
        // TODO: don't allow anything to happen with a zero-length
        // history.
        var it = h[0].length - 2;
        var i = 0;
        for (var _i = 0, t_1 = t; _i < t_1.length; _i++) {
            var tj = t_1[_i];
            // This bit of calculation is not that nice - we're better
            // off holding both start and end times than doing this.
            while (h[i][it] + h[i][it + 1] < tj) {
                i++;
            }
            y.push(this.stepper.interpolate(tj, h[i]));
        }
        return y;
    };
    return Interpolator;
}());
exports.Interpolator = Interpolator;

},{}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var SQRT_DBL_EPSILON = Math.pow(2, -52 / 2);
function square(x) {
    return x * x;
}
exports.square = square;
// constrain x to lie in [min, max]
function constrain(x, min, max) {
    return Math.max(Math.min(x, max), min);
}
exports.constrain = constrain;
function copyArray(to, from) {
    var n = to.length;
    for (var i = 0; i < n; i++) {
        to[i] = from[i];
    }
}
exports.copyArray = copyArray;
function zeros(n) {
    var ret = Array(n);
    for (var i = 0; i < n; ++i) {
        ret[i] = 0.0;
    }
    return ret;
}
exports.zeros = zeros;
function approxEqual(x, y, tolerance) {
    if (tolerance === void 0) { tolerance = SQRT_DBL_EPSILON; }
    var xy = Math.abs(x - y);
    var xn = Math.abs(x);
    if (xn > tolerance) {
        xy /= xn;
    }
    return xy < tolerance;
}
exports.approxEqual = approxEqual;
function approxEqualArray(x, y, tolerance) {
    if (tolerance === void 0) { tolerance = SQRT_DBL_EPSILON; }
    if (y.length !== x.length) {
        throw Error("Incompatible arrays");
    }
    var scale = 0;
    var xy = 0;
    var n = 0;
    for (var i = 0; i < x.length; ++i) {
        if (x[i] !== y[i]) {
            scale += Math.abs(x[i]);
            xy += Math.abs(x[i] - y[i]);
            n++;
        }
    }
    if (n === 0) {
        return true;
    }
    scale /= n;
    xy /= n;
    if (scale > tolerance) {
        xy /= scale;
    }
    return xy < tolerance;
}
exports.approxEqualArray = approxEqualArray;
function seqLen(a, b, len) {
    var d = (a - b) / (len - 1);
    var ret = [];
    for (var i = 0; i < len; ++i) {
        var p = i / (len - 1);
        ret.push((1 - p) * a + p * b);
    }
    return ret;
}
exports.seqLen = seqLen;
function last(x) {
    return x[x.length - 1];
}
exports.last = last;

},{}]},{},[1]);
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
    return x === undefined || x === null || isNaN(x);
}
var odin = {};
odin.sir = (function() {
  function sir(user) {
    this.internal = {};
    var internal = this.internal;
    internal.b = 1 / 75;
    internal.delta = 1 / 5;
    internal.initial_R = 0;
    internal.N = 10000000;
    internal.sigma = 12;
    internal.Births = internal.N / 75;
    this.setUser(user);
  }
  sir.prototype.setUser = function(user) {
    var internal = this.internal;
    getUser(user, "beta", internal, null, 24, null, null, false);
    getUser(user, "I0", internal, null, 1, null, null, false);
    internal.initial_I = internal.I0;
    internal.initial_S = internal.N - internal.I0;
    this.updateMetadata();
  };
  sir.prototype.rhs = function(t, state, dstatedt) {
    var internal = this.internal;
    var S = state[0];
    var I = state[1];
    var R = state[2];
    dstatedt[1] = internal.beta * S * I / internal.N - (internal.b + internal.sigma) * I;
    dstatedt[2] = internal.sigma * I - internal.b * R - internal.delta * R;
    dstatedt[0] = internal.Births - internal.b * S - internal.beta * S * I / internal.N + internal.delta * R;
  };
  sir.prototype.rhsEval = function(t, state) {
    var dstatedt = zeros(state.length);
    this.rhs(t, state, dstatedt);
    return dstatedt;
  };
  sir.prototype.initial = function(t) {
    var internal = this.internal;
    var state = zeros(3);
    state[0] = internal.initial_S;
    state[1] = internal.initial_I;
    state[2] = internal.initial_R;
    return state;
  };
  sir.prototype.run = function(times, y0) {
    return integrateOdin(this, times, y0);
  };
  sir.prototype.coef = {beta: {has_default: false, default: 24, rank: 0, min: -Infinity, max: Infinity, integer: false}, I0: {has_default: false, default: 1, rank: 0, min: -Infinity, max: Infinity, integer: false}};
  sir.prototype.updateMetadata = function() {
    this.metadata = {};
    this.metadata.ynames = ["t", "S", "I", "R"];
  };
  return sir;
}());
function odinInit() {
    var model = Object.values(odin)[0];
    var pars = model.prototype.coef;
    var container = document.getElementById("odin_parameters");
    Object.keys(pars).forEach(function(name) {
        odinControlAdd(name, pars, container)
    });
    odinPlot();
}

function odinPlot() {
    var graph = document.getElementById("odin_graph");
    var t0 = parseFloat(document.getElementById("t0").value);
    var t1 = parseFloat(document.getElementById("t1").value);
    var width = parseInt(graph.style.width, 10);
    var dt = 1.0 * (t1 - t0) / width;
    var t = [];
    for (var i = 0; i < width; ++i) {
        t.push(t0 + i * dt);
    }

    var model = Object.values(odin)[0];
    var user = odinParameters(model);
    var mod = new model(user);
    var res = mod.run(t);
    new Dygraph(graph, res.y, {"labels": res.names});
}

function odinParameters(model) {
    var user = {};
    Object.keys(model.prototype.coef).forEach(function(name) {
        var el = document.getElementById(odinParameterId(name));
        user[name] = parseFloat(el.value);
    });
    return user;
}


function odinControlAdd(name, data, container) {
    var label = document.createElement("code");
    label.textContent = name;

    var input = document.createElement("input");
    input.type = "text";
    input.id = odinParameterId(name);
    if (data[name]["default"] !== null) {
        input.value = data[name]["default"];
    }

    container.appendChild(label);
    container.appendChild(input);
    container.appendChild(document.createElement("br"));
}

function odinParameterId(name) {
    return "odin_parameter_" + name;
}
