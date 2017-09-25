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

# Applying a stringent mean filter will select for higher dispersions.
# This is because high-dispersion features are more likely to achieve large sample means.
# The result is to encourage inflation of the dispersion estimate, which results in some distortion of the p-value distribution.
# However, this seems like an edge case, as it is minor if the dispersion is low or the prior d.f. is high.
# (It gets a bit funky at low prior d.f. anyway, as the QL model handles variability in the QL dispersions not NB dispersions.)
# 
# In practice, this is not much of an issue.
# For ChIP-seq data, the prior degrees of freedom is usually quite high such that there is not much variability in the dispersions.
# For RNA-seq data, the filter boundary is not dense so the specifics of filtering doesn't matter.
# The same effect is present in the other filters anyway (in addition to their poor performance on the constant dispersion case),
#    as high dispersions make it more likely to get one or two peaks above the threshold.
