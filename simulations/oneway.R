# This script tests the behaviour of the mean-based filter with different one-way layouts.
# We load in the relevant functions to perform the testing.

source("functions.R")
dir.create("results-oneway", showWarning=FALSE)

# Pairwise comparison between groups.    

design <- model.matrix(~rep(0:1, each=3))
nlibs <- nrow(design)

pdf("results-oneway/twogroup.pdf", width=10, height=5)
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) { 
        makePlots(rep(mean, nlibs), disp, design, main=paste0("Mean = ", mean, ", dispersion = ", disp))

    }
}
dev.off()

# Pairwise comparison with multiple groups
# Equal means in all groups.

design <- model.matrix(~factor(rep(0:2, each=3)))
nlibs <- nrow(design)

pdf("results-oneway/threegroup_equal.pdf")
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) { 
        makePlots(rep(mean, nlibs), disp, design, main=paste0("Mean = ", mean, ", dispersion = ", disp))
    }
}
dev.off()

# Unequal means in the untested group.

pdf("results-oneway/threegroup_unequal.pdf")
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) { 
        for (fc in c(0.5, 2)) {
            means <- rep(mean, nlibs)
            means[design[,3]==1] <- mean*fc # Last group won't be involved in testing.

            makePlots(means, disp, design, coef=2, main=paste0("Mean = ", mean, ", dispersion = ", disp, ", FC = ", fc))
        }
    }
}
dev.off()

# Multiple groups, ANODEV

pdf("results-oneway/threegroup_anodev.pdf")
par(mfrow=c(1,2))
for (mean in c(20, 50, 100)) {
    for (disp in c(0.01, 0.05, 0.2)) { 
        makePlots(rep(mean, nlibs), disp, design, coef=2:3, main=paste0("Mean = ", mean, ", dispersion = ", disp))
    }
}
dev.off()

