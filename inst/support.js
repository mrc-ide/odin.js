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
    }
    var sol = dopri.integrate(rhs, y0, t0, t1);
    return sol(times);
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
    return x === undefined || x === null
}
