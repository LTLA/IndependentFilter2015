library(edgeR)

.get_lib_sizes <- function(counts, force.equal=TRUE) { 
    lib.sizes <- colSums(counts)
    if (force.equal) {
        return(rep(mean(lib.sizes), ncol(counts)))
    } else {
        return(lib.sizes)
    }
}

detectDiff <- function(counts, design, coef, lib.size=NULL)
# Uses QL edgeR to compute p-values for a given count matrix and design.
{
    y <- DGEList(counts, lib.size=lib.size)
    y <- estimateDisp(y, design)
    fit <- glmQLFit(y, design, robust=TRUE)
    res <- glmQLFTest(fit, coef=coef)
    return(list(common.dispersion=y$common.dispersion, PValue=res$table$PValue))
}

plotAlpha <- function(pvals, ylab="Observed/specified", xlab="Specified", xlim=NULL, ylim=c(0.5, 1.5), ...) 
# Plots the observed type I error rate against the specified threshold, 
# for a variety of threshold values.
{
    for (i in seq_along(pvals)) { 
        cur.p <- pvals[[i]]
        exp <- (seq_along(cur.p) - 0.5)/length(cur.p)
        n <- findInterval(exp, sort(cur.p))
        obs <- n/length(cur.p)
        if (is.null(xlim)) { # Stable at 20 observations.
            xlim <- c(exp[which(n >= 20)[1]], 1)
        }
        if (i==1L) {
            plot(exp, obs/exp, log="xy", xlim=xlim, type="l", ylim=ylim, xlab=xlab, ylab=ylab, ...)
        } else {
            lines(exp, obs/exp, ...)
        }
    }
    invisible(NULL)
}

generator <- function(means, disp, ngenes=100000) 
# Generates a count matrix with the specified means.
{
    nlibs <- length(means)
    matrix(rnbinom(nlibs*ngenes, mu=means, size=1/disp), ncol=nlibs, byrow=TRUE)
}

meanFilter <- function(counts, lib.size=NULL, prop=0.1) 
# Applies the mean-based filter.
{
    filter <- aveLogCPM(counts, lib.size=lib.size)
    rank(-filter) <= nrow(counts)*prop
}

makePlots <- function(means, disp, design, coef=ncol(design), main="", equal.libs=TRUE) 
# Makes plots of the type I error rate, after mean-filtering and random filtering.
{
    null.p <- ref.p <- list()
    for (it in 1:10) {
        counts <- generator(means, disp)
        lib.size <- .get_lib_sizes(counts, force.equal=equal.libs)

        keep <- meanFilter(counts, lib.size=lib.size)
        res <- detectDiff(counts[keep,], design, lib.size=lib.size, coef=coef)
        null.p[[it]] <- res$PValue

        top.set <- seq_len(sum(keep))
        ref <- detectDiff(counts[top.set,], design, lib.size=lib.size, coef=coef)
        ref.p[[it]] <- ref$PValue
    }

    plotAlpha(null.p, main=main)
    legend("topright", bty="n", legend="Mean-filtered")
    plotAlpha(ref.p, main="", col="red")
    legend("topright", bty="n", legend="Random")

    invisible(NULL)
}


