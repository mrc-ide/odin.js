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
    if (obj.metadata.interpolateTimes !== null) {
        interpolateCheckT(times, obj.metadata.interpolateTimes);
    }
    if (isMissing(y0)) {
      y0 = obj.initial(times[0]);
    }
    var rhs = function(t, y, dy) {
        obj.rhs(t, y, dy);
    };
    var sol = null;
    if (typeof obj.output === "function") {
        var output = function(t, y) {
            return obj.output(t, y);
        }
        sol = dopri.integrate(rhs, y0, t0, t1, {}, output);
    } else {
        sol = dopri.integrate(rhs, y0, t0, t1, {});
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
    if (!(typeof value === "object" && "data" in value && "dim" in value)) {
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


// Ranks 2..8 created by scripts/create_support_sum_js.R
function odinSum1(x, from, to) {
    var tot = 0.0;
    for (var i = from; i < to; ++i) {
        tot += x[i];
    }
    return tot;
}
