function interpolateSplineEval(x, obj) {
    var i = interpolateSearch(x, obj);
    if (i < 0 || i == obj.n) { // off the lhs or rhs
        return interpolateEvalFail(x, obj, y);
    }

    var y = new Array(obj.ny);
    for (var j = 0; j < obj.ny; ++j) {
        y[j] = splineEval(i, x, obj.x, obj.y[j], obj.k[j]);
    }

    return y;
}


function splineEval(i, x, xs, ys, ks) {
  var t = (x - xs[i]) / (xs[i + 1] - xs[i]);
  var a =  ks[i] * (xs[i + 1] - xs[i]) - (ys[i + 1] - ys[i]);
  var b = -ks[i + 1] * (xs[i + 1] - xs[i]) + (ys[i + 1] - ys[i]);
  return (1 - t) * ys[i] + t * ys[i + 1] + t * (1 - t) * (a * (1 - t) + b * t);
}


function splineCalcA(x) {
    var A0 = new Array(n);
    var A1 = new Array(n);
    var A2 = new Array(n);
    var n = x.length;
    var nm1 = n - 1;

    A0[0] = 0; // will be ignored
    A1[0] = 2 / (x[1] - x[0]);
    A2[0] = 1 / (x[1] - x[0]);
    for (var i = 1; i < nm1; ++i) {
        A0[i] = 1 / (x[i] - x[i - 1]);
        A1[i] = 2 * (1 / (x[i] - x[i - 1]) + 1 / (x[i + 1] - x[i]));
        A2[i] = 1 / (x[i + 1] - x[i]);
    }
    A0[nm1] = 1 / (x[nm1] - x[nm1-1]);
    A1[nm1] = 2 / (x[nm1] - x[nm1-1]);
    A2[nm1] = 0; // will be ignored

    return [A0, A1, A2];
}


function splineCalcB(x, y) {
    var n = x.length;
    var nm1 = n - 1;
    var B = [];
    for (var j = 0; j < y.length; ++j) {
        var Bj = new Array(n);
        var yj = y[j];
        Bj[0] = 3 * (yj[1] - yj[0]) / ((x[1] - x[0]) * (x[1] - x[0]));
        for (var i = 1; i < nm1; ++i) {
            Bj[i] = 3 *
                ((yj[i]   - yj[i-1]) / ((x[i  ] - x[i-1]) * (x[i  ] - x[i-1])) +
                 (yj[i+1] - yj[i  ]) / ((x[i+1] - x[i  ]) * (x[i+1] - x[i  ])));
        }
        Bj[nm1] = 3 *
            (yj[nm1] - yj[nm1-1]) / ((x[nm1] - x[nm1-1]) * (x[nm1] - x[nm1-1]));
        B.push(Bj);
    }
    return B;
}


function splineCalcK(A, B) {
    var a = A[0], b = A[1], c = A[2];
    var n = a.length;
    for (var i = 0; i < B.length; ++i) {
        solveTridiagonal(n, a, b, c, B[i]);
    }
}


function solveTridiagonal(n, a, b, c, x) {
    // Eliminate:
    for (var i = 1; i < n; ++i) {
        if (b[i - 1] === 0) {
            throw Error("solve failed due to lack of diagonal dominance");
        }
        var fac = a[i] / b[i - 1];
        b[i] -= fac * c[i - 1];
        x[i] -= fac * x[i - 1];
    }

    // Back-substitute:
    if (b[n - 1] === 0) {
        throw Error("solve failed due to singular matrix");
    }
    x[n - 1] /= b[n - 1];
    for (var i = n - 2; i >= 0; i--) {
        if (b[i] === 0) {
            throw Error("solve failed due to singular matrix");
        }
        x[i] = (x[i] - c[i] * x[i + 1]) / b[i];
    }
    return x;
}
