# This script tests the behaviour of the mean-based filter with an additive design.
# We load in the relevant functions to perform the testing.

source("functions.R")
dir.create("results-additive", showWarning=FALSE)

# Test for an effect with no batch effect.

design <- model.matrix(~factor(rep(0:2, each=2))+rep(0:1, 3))
nlibs <- nrow(design)

pdf("results-additive/simple.pdf")
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) {
        makePlots(rep(mean, nlibs), disp, design, main=paste0("Mean = ", mean, ", dispersion = ", disp))
    }
}
dev.off()

# Test for an effect with a batch effect.

pdf("results-additive/batch.pdf")
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) {
        for (fc in c(2, 5)) {
            means <- rep(c(mean, mean*fc, mean*fc*2), each=2)
            makePlots(means, disp, design, main=paste0("Mean = ", mean, ", dispersion = ", disp, ", FC = ", fc))
    }
}
dev.off()

