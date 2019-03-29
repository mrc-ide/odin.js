// V8 does not support Array.fill from the look of it
function zeros(n) {
    var ret = new Array(n);
    for (var i = 0; i < n; ++i) {
	ret[i] = 0;
    }
    return ret;
}

function integrate_run(rhs, y0, times) {
    var t0 = times[0];
    var t1 = times[times.length - 1];
    var sol = global.dopri.integrate(rhs, y0, t0, t1);
    return sol(times);
}

function integrate_odin(obj, times, y0) {
    var t0 = times[0];
    var t1 = times[times.length - 1];
    if (y0 === undefined || y0 === null) {
      y0 = obj.initial(times[0]);
    }
    var rhs = function(t, y, dy) {
	obj.rhs(t, y, dy);
    }
    var sol = dopri.integrate(rhs, y0, t0, t1);
    return sol(times);
}


function get_user(user, name, internal, size, default_value,
		  min, max, is_integer) {
    var value = user[name];
    if (size !== null) {
	throw Error("Arrays not yet supported");
    }
    if (is_missing(value)) {
	if (is_missing(internal[name])) {
	    if (default_value === null) {
		throw Error("Expected a value for '" + name + "'");
	    } else {
		internal[name] = default_value;
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


function is_missing(x) {
    return x === undefined || x === null
}
