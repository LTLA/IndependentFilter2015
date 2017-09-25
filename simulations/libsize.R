# This script tests the behaviour of the mean-based filter with different library sizes.
# We load in the relevant functions to perform the testing.

source("functions.R")
dir.create("results-libsize", showWarning=FALSE)

# Pairwise comparisons with different library sizes.

design <- model.matrix(~rep(0:1, each=3))
nlibs <- nrow(design)

pdf("results-libsize/twogroup.pdf")
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) {
        for (fc in c(2, 5)) { 
            means <- rep(mean, nlibs)
            means[design[,2]==1] <- mean*fc
            makePlots(means, disp, design, equal.libs=FALSE, main=paste0("Mean = ", mean, ", dispersion = ", disp, ", FC = ", fc))
    }
}
dev.off()

# Additive comparison with different library sizes.

design <- model.matrix(~factor(rep(0:2, each=2))+rep(0:1, 3))
nlibs <- nrow(design)

pdf("results-libsize/additive.pdf")
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) {
        for (fc in c(2, 5)) { 
            means <- rep(mean, nlibs)
            means[design[,3]==1] <- mean*fc
            makePlots(means, disp, design, equal.libs=FALSE, main=paste0("Mean = ", mean, ", dispersion = ", disp, ", FC = ", fc))
 
    }
}
dev.off()


