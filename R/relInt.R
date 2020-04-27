## pat and refpat are two-column matrices (if a third column exists it is ignored).
relInt <- function(pat, refpat) {
    common.masses <- pat[pat[, 1] %in% refpat[, 1], 1]
    expI <- pat[pat[, 1] %in% refpat[, 1], 2]
    DBI <- refpat[refpat[, 1] %in% pat[, 1], 2]
    
    if (length(expI) == 0) {
        ## can happen when comparing unknowns...
        return(0)
    }
    
    if (length(expI) == 1) {
        ## only one mass in common, should not happen very often
        expI/DBI
    } else {
        if (length(expI) < 5) {
            ## here we should use weighted regression! Agreement in high masses is much more important than agreement in low masses.
            ## Unweighted regression usually works fine but in some isolated cases negative coefficients can occur. We should not have
            ## that...  relI <- coef(lm(expI ~ DBI))[2]
            relI <- coef(lm(expI ~ DBI, weights = sqrt(common.masses)))[2]
        } else {
            ## if the robust estimator breaks down, we use the regular one... this sometimes happens when the two vectors are really very
            ## similar
            relI <- try(coef(lmrob(expI ~ DBI))[2], silent = TRUE)
            if (is(relI)[1] == "try-error") 
                relI <- coef(lm(expI ~ DBI, weights = sqrt(common.masses)))[2]
            ## relI <- coef(rlm(expI ~ DBI, weights = sqrt(common.masses), method = 'MM', acc = 1))[2] ## high breakdown point...
        }
        
        if (relI < 0) {
            # warning('Found negative relative I; returning 0') # <<<<<<<<<<
            return(0)
        }
        
        relI
    }
}

