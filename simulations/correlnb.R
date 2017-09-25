# This tests the behaviour of filtering on correlated tests with the NB mean filter.

ngenes <- 100000
design <- model.matrix(~gl(2, 3))
nlib <- nrow(design)

source("functions.R")
dir.create("results-misc", showWarning=FALSE)

# Testing the NB.
set.seed(634294)
mu <- 10
disp <- 0.1
filtrate <- 0.1

pdf("results-misc/correl.pdf", width=10, height=5)
par(mfrow=c(1,2))
for (nshared in 1:4) { 
    nleft <- 5 - nshared

    pval.mean <- pval.sign <- list()
    for (it in 1:10) { 
        # Using an additive approach to induce correlations between tests for the same samples.
        shared <- matrix(rnbinom(ngenes*nlib, mu=mu*nshared, size=nshared/disp), nrow=ngenes)
        unique1 <- matrix(rnbinom(ngenes*nlib, mu=mu*nleft, size=nleft/disp), nrow=ngenes)
        unique2 <- matrix(rnbinom(ngenes*nlib, mu=mu*nleft, size=nleft/disp), nrow=ngenes)

        insta1 <- shared + unique1
        insta2 <- shared + unique2
        y1 <- DGEList(insta1)
        y2 <- DGEList(insta2)

        # First filtering by mean.
        criterion <- pmin(aveLogCPM(y1), aveLogCPM(y2))
        keep.mean <- rank(-criterion) < ngenes*filtrate
        
        y1m <- estimateDisp(y1[keep.mean,], design)
        y2m <- estimateDisp(y2[keep.mean,], design)
        fit1m <- glmQLFit(y1m, design, robust=TRUE)
        fit2m <- glmQLFit(y2m, design, robust=TRUE)
        result1m <- glmQLFTest(fit1m)
        result2m <- glmQLFTest(fit2m)
        pval.mean[[it]] <- c(result1m$table$PValue, result2m$table$PValue) # Symmetry, so it doesn't really matter.
        
        # Now filtering by sign.
        fit1 <- glmFit(y1, design, dispersion=0.05)
        fit2 <- glmFit(y2, design, dispersion=0.05)
        keep.sign <- (fit1$coefficients[,2] > 0)==(fit2$coefficients[,2] > 0)
        keep.sign <- sample(which(keep.sign), filtrate*ngenes) # Picking a subset for speed.

        y1s <- estimateDisp(y1[keep.sign,], design)
        y2s <- estimateDisp(y2[keep.sign,], design)
        fit1s <- glmQLFit(y1s, design, robust=TRUE)
        fit2s <- glmQLFit(y2s, design, robust=TRUE)
        result1s <- glmQLFTest(fit1s)
        result2s <- glmQLFTest(fit2s)
        pval.sign[[it]] <- c(result1s$table$PValue, result2s$table$PValue)
    }

    plotAlpha(pval.mean, main=paste0(nshared, " shared blocks"))
    legend("topright", bty="n", legend="Mean-filtered")
    plotAlpha(pval.sign, main="", col="red")
    legend("topright", bty="n", legend="Sign-filtered")

}

dev.off()

# The sign-based filter fails with correlations.
# In this case, it's not so bad, but when the filter becomes more stringent (e.g., more than just two tests), you could imagine it would involve more problems.
