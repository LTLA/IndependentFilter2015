# This checks the derivations of the distributions in the filtering chapter.

isOnline <- function(mat, point, tol) {
	ninst <- nrow(mat)
	adj <- t(t(mat) - point)
	is.max <- max.col(adj)
	is.max <- (is.max - 1) * ninst + 1:ninst
	is.min <- max.col(-adj)
	is.min <- (is.min - 1) * ninst + 1:ninst
	stats <- mat[is.max] - mat[is.min]
	return(stats < tol)
}

getGamma <- function(mat, point) { rowMeans(mat) - mean(point) }

##################
# First, regarding the distribution of points on each z-line.

simNcheck <- function(nlibs, my.sd, my.means, point, tol=0.1) { 
	ninst <- 1e6
	my.norm <- matrix(rnorm(nlibs*ninst, mean=my.means, sd=my.sd), ncol=nlibs, byrow=TRUE)
	
	keep <- isOnline(my.norm, point, tol=tol)
	my.dist <- getGamma(my.norm[keep,], point)
	cat(sprintf("Using %i points, we have a mean of %.4f and a variance of %.4f\n", 
			sum(keep), mean(my.dist), var(my.dist)))

	# Following the z-line calculations:
	cat(sprintf("Intended mean should be %.4f, variance should be %.4f\n", 
			mean(my.means) - mean(point), my.sd^2/nlibs))
	return(invisible(NULL))
}

set.seed(237685)

simNcheck(4, 0.5, c(0,1,0,0), c(1,1,0,1))

simNcheck(4, 0.5, c(0,1,0,0), c(1,0,0,1))

simNcheck(4, 0.5, c(0,1,0,0), c(1,2,0,1))

simNcheck(4, 0.5, c(0,0.5,0,0), c(1,1,0,1))

simNcheck(4, 0.2, c(0,1,0,0), c(1,1,0,1))

simNcheck(4, 1, c(0,1,0,0), c(1,1,0,1))

simNcheck(3, 1, c(0,1,0), c(1,1,0))

simNcheck(2, 1, c(0,1), c(1,1))

##################
# Second, regarding the distribution of points on each Z-plane.

simNcheck2 <- function(nlibs, my.sd, 
		my.sd.1, A.1, B.1,
		my.sd.2, A.2, B.2,
		point.1, point.2, tol=0.1) {

	ninst <- 1e6
	my.means.x <- matrix(rnorm(nlibs*ninst, mean=0, sd=my.sd), ncol=nlibs, byrow=TRUE)
	my.norm.1 <- matrix(rnorm(nlibs*ninst, mean=my.means.x*A.1 + B.1, sd=my.sd.1), ncol=nlibs, byrow=TRUE)
	my.norm.2 <- matrix(rnorm(nlibs*ninst, mean=my.means.x*A.2 + B.2, sd=my.sd.2), ncol=nlibs, byrow=TRUE)

	keep.1 <- isOnline(my.norm.1, point.1, tol=tol)
	keep.2 <- isOnline(my.norm.2, point.2, tol=tol)
	keep <- keep.1 & keep.2
	my.dist.1 <- getGamma(my.norm.1[keep,], point.1)
	my.dist.2 <- getGamma(my.norm.2[keep,], point.2)

	cat(sprintf("Using %i points, observed means are %.4f and %.4f\n", 
		sum(keep), mean(my.dist.1), mean(my.dist.2)))
	cat(sprintf("Intended means are %.4f and %.4f\n\n", 
		mean(B.1) - mean(point.1), mean(B.2) - mean(point.2)))

	cat(sprintf("Observed variances are %.4f and %.4f (covariance of %.4f)\n", 
		var(my.dist.1), var(my.dist.2), cov(my.dist.1, my.dist.2)))
	
	R <- 1/my.sd^2 + (A.1/my.sd.1)^2 + (A.2/my.sd.2)^2
	inv1 <- nlibs/R * (R - (A.1/my.sd.1)^2) / my.sd.1^2 
	inv2 <- nlibs/R * (R - (A.2/my.sd.2)^2) / my.sd.2^2 
	inv12 <- nlibs/R * -A.1 * A.2 / (my.sd.1^2*my.sd.2^2)

	mat <- rbind(c(inv1, inv12), 
		     c(inv12, inv2))
	mat <- solve(mat)
	cat(sprintf("Intended variances are %.4f and %.4f (covariance of %.4f)\n",
		mat[1], mat[4], mat[2]))
	return(invisible(NULL))
}

set.seed(13585)

simNcheck2(4, my.sd=0.5, 
	my.sd.1=0.5, A.1=2, B.1=c(1,1,1,1),
	my.sd.2=0.5, A.2=2, B.2=c(2,2,2,2),
	point.1=c(0,0,0,1), point.2=c(0,1,0,0))

simNcheck2(4, my.sd=0.5, 
	my.sd.1=0.5, A.1=1, B.1=c(1,1,1,1),
	my.sd.2=0.5, A.2=2, B.2=c(2,2,2,2),
	point.1=c(0,0,0,1), point.2=c(0,1,0,0))

simNcheck2(4, my.sd=0.2, 
	my.sd.1=0.5, A.1=1, B.1=c(1,1,1,1),
	my.sd.2=0.5, A.2=2, B.2=c(2,2,2,2),
	point.1=c(0,0,0,1), point.2=c(0,1,0,0))

simNcheck2(4, my.sd=0.2, 
	my.sd.1=1, A.1=1, B.1=c(1,1,1,1),
	my.sd.2=0.5, A.2=2, B.2=c(2,2,2,2),
	point.1=c(0,0,0,1), point.2=c(0,1,0,0))

simNcheck2(4, my.sd=0.5, 
	my.sd.1=0.5, A.1=1, B.1=c(1,0,1,0),
	my.sd.2=0.5, A.2=2, B.2=c(2,1,2,1),
	point.2=c(2,1,2,1), point.1=c(1,0,1,0), tol=0.01)

simNcheck2(3, my.sd=0.2, 
	my.sd.1=0.2, A.1=1, B.1=c(1,1,1),
	my.sd.2=0.2, A.2=2, B.2=c(2,2,2),
	point.1=c(1,1,1), point.2=c(2,2,2))

##################

