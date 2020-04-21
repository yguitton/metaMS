## Auxiliary function for plotting a particular pseudospectrum. M/z values are in the first column of the matrix, and an
## intensity measure (either maxo, into or something else) in the second. The third column is disregarded, usually contains
## retention time information
plotPseudoSpectrum <- function(psspc, ...) {
    plot(psspc[, 1:2], type = "h", col = 4, xlab = "m/z", ylab = "I", ...)
}

