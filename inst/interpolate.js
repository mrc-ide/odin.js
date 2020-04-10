// --- odin bits
function interpolateCheckY(dimArg, dimTarget, nameArg, nameTarget) {
    var rank = dimTarget.length - 1;
    if (rank === 0) {
        if (dimArg[0] !== dimTarget[0]) {
            throw Error("Expected " + nameArg + " to have length " +
                        dimArg[0] + " (for " + nameTarget + ")");
        }
    } else {
        for (var i = 0; i < dimTarget.length; ++i) {
            if (dimArg[i] !== dimTarget[i]) {
                throw Error("Expected dimension " + (i + 1) + " of " +
                            nameArg + " to have size " + dimArg[i] +
                            " (for " + nameTarget + ")");
            }
        }
    }
}


function interpolateCheckT(times, interpolateTimes) {
    // TODO: ordinarily with the C solver we'd also have a critical
    // time to prevernt the solver overshooting; I don't think that I
    // support this in dopri-js and that will block getting spline
    // interpolation implemented...
    if (times[0] < interpolateTimes.min) {
        throw Error("Integration times do not span interpolation range; " +
                    "min: " + interpolateTimes.min);
    }
    if (times[times.length - 1] > interpolateTimes.max) {
        throw Error("Integration times do not span interpolation range; " +
                    "max: " + interpolateTimes.max);
    }
}


// --- implementation
function interpolateAlloc(type, x, y, failOnExtrapolate) {
    if (type === "spline") {
        throw Error("'spline' interpolation is not currently supported");
    }
    var n = x.length;
    var ny = y.length / n
    var evalFn = null;

    // Optimisation for the constant interpolation - this should be
    // moved into the C version of this too
    if (type === "constant") {
        var ylinear = y;
        y = [];
        for (var i = 0; i < n; ++i) {
            var yi = [];
            for (j = 0; j < ny; ++j) {
                yi.push(ylinear[j * n + i]);
            }
            y.push(yi);
        }
        evalFn = interpolateConstantEval;
    } else {
        y = y.slice();
        evalFn = interpolateLinearEval;
    }

    var ret = {
        type: type,
        n: n,
        ny: ny,
        i: 0,
        x: x.slice(),
        y: y,
        evalFn: evalFn,
        failOnExtrapolate: failOnExtrapolate
    };

    return ret;
}


function interpolateEval(x, obj) {
    return obj.evalFn(x, obj);
}


// Constatnt
function interpolateConstantEval(x, obj) {
    var i = interpolateSearch(x, obj);
    if (i < 0) {
        return interpolateEvalFail(x, obj);
    } else if (i == obj.n) { // off the rhs
        i = obj.n - 1;
    }
    // TODO: In general, I wonder if this should be dealt with in interpolate
    // search?
    //
    // NOTE: In the R function 'approx' there is an argument 'f' that
    // deals with the 'ties' case more gracefully.  This is like the
    // default f=0, omitting this becomes like the option f=1.
    if (i != obj.n - 1 && obj.x[i + 1] == x) {
        ++i;
    }

    return obj.y[i];
}


// Linear
function interpolateLinearEval(x, obj) {
    var i = interpolateSearch(x, obj);
    if (i < 0 || i == obj.n) { // off the lhs or rhs
        return interpolateEvalFail(x, obj, y);
    }

    var x0 = obj.x[i], x1 = obj.x[i + 1];
    var scal = (x - x0) / (x1 - x0);

    // In the C version we do this in-place; we could do that here
    // too, though it interacts poorly with the optimisation for the
    // constant version.
    var y = new Array(obj.ny);

    for (var j = 0; j < obj.ny; ++j) {
        var k = i + j * obj.n;
        var y0 = obj.y[k], y1 = obj.y[k + 1];
        y[j] = y0 + (y1 - y0) * scal;
    }

    return y;
}


function interpolateEvalFail(x, obj) {
    if (obj.failOnExtrapolate) {
        throw Error("Interpolation failed as " + x + " is out of range");
    }
    var y = new Array(obj.ny);
    for (var j = 0; j < obj.ny; ++j) {
        y[j] = null;
    }
    return y;
}


function interpolateSearch(target, obj) {
    var i0 = obj.i, i1 = obj.i, inc = 1;
    var n = obj.n;
    var x = obj.x;

    if (x[i0] <= target) { // advance up until we hit the top
        if (i0 >= n - 1) { // guess is already *at* the top.
            // This exit is left in here to avoid the possibility of an
            // infinite loop or reading out of range, but should not be
            // necessary unless the object has been tampered with because we
            // always set the guess to the lower bound of our guess for 'i'.
            // This bit of code is derived from something in `ring`, where
            // this was dynamic, but it makes for a fairly cheap safety
            // check.
            return n;
        }
        i1 = i0 + inc;
        while (x[i1] < target) {
            i0 = i1;
            inc *= 2;
            i1 += inc;
            if (i1 >= n) { // off the end of the buffer
                i1 = n - 1;
                if (x[i1] < target) {
                    return n;
                }
                break;
            }
        }
    } else { // advance down
        if (i0 == 0) { // guess is already at the bottom
            return -1;
        }
        i0 = i0 - inc;
        while (x[i0] > target) {
            i1 = i0;
            inc *= 2;
            if (i0 < inc) {
                i0 = 0;
                if (x[i0] > target) {
                    return -1;
                }
                break;
            }
            i0 -= inc;
        }
    }

    while (i1 - i0 > 1) {
        var i2 = (i1 + i0) / 2;
        if (x[i2] < target) {
            i0 = i2;
        } else {
            i1 = i2;
        }
    }

    obj.i = i0;
    return i0;
}
