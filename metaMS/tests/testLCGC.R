if (require("testthat", quietly = TRUE)){
  test_package("metaMS")
} else {
  warning("Cannot run tests: please install package testthat")
}
