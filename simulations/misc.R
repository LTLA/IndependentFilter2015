# We load in the relevant functions to perform the testing.

source("functions.R")
dir.create("results-misc", showWarning=FALSE)

# This script tests the behaviour of the mean-based filter with variable dispersions.

design <- model.matrix(~gl(2,3))
nlibs <- nrow(design)
mean <- 20

pdf("results-misc/hetero.pdf", width=10, height=5)
par(mfrow=c(1,2))
for (disp in c(0.01, 0.05, 0.2)) {
    for (df in c(5, 10, 20)) {
        all.disp <- disp*df/rchisq(formals(generator)$ngenes, df)
        makePlots(rep(mean, nlibs), all.disp, design, main=paste0("Dispersion = ", disp, ", prior d.f. = ", df))
    }
}
dev.off()

