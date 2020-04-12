The tests odin-run-* are directly ported over from the odin test suite, with some minor changes.  As these changes decrease we may merge this back into the main package.

```
git diff --no-index odin/tests/testthat/run/test-run-basic.R odin.js/tests/testthat/test-run-basic.R
git diff --no-index odin/tests/testthat/run/test-run-discrete.R odin.js/tests/testthat/test-run-discrete.R
git diff --no-index odin/tests/testthat/run/test-run-general.R odin.js/tests/testthat/test-run-general.R
git diff --no-index odin/tests/testthat/run/test-run-library.R odin.js/tests/testthat/test-run-library.R
git diff --no-index odin/tests/testthat/run/test-run-interpolation.R odin.js/tests/testthat/test-run-interpolation.R
git diff --no-index odin/tests/testthat/run/test-run-regression.R odin.js/tests/testthat/test-run-regression.R
git diff --no-index odin/tests/testthat/run/test-run-stochastic.R odin.js/tests/testthat/test-run-stochastic.R
```
