printString <- function(..., screenwidth = 72) {
    prstring <- paste(..., collapse = " ")
    prntString <- paste("< --------", prstring, paste(rep("-", screenwidth - nchar(prstring) - 12), collapse = ""), ">")
    
    message(prntString)
}


## different aesthetics ...  In this and the next function we use '...' to allow for multiple pieces of text. This means we do
## not have to use paste in the function calling printInfo or printWarning.
printInfo <- function(...) {
    prstring <- paste(..., collapse = " ")
    prntString <- paste("\t", prstring)
    
    message(prntString)
}

## for the warnings

printWarning <- function(...) {
    prstring <- paste(..., collapse = " ")
    prntString <- paste("!!\n", prstring, "\n!!")
    
    message(prntString)
}



