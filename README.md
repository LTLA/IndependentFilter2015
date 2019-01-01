# Independent filtering in 'omics data analyses

This repository summarizes some work from my PhD thesis describing the independence of the NB mean as a filter statistic.
The aim is to allow others (and myself!) to examine and update this work without having to modify the code in the thesis repository.
There are several components available:

- `theory`: This contains a [proof](https://ltla.github.io/IndependentFilter2015/theory/single_norm.html) of the independence of the sample mean to the _p_-values under normality.
It [extends](https://ltla.github.io/IndependentFilter2015/theory/multi_norm.html) this idea to the case of correlated tests under normality.
(However, I am told that the notation could use some work, so YMMV.)
It also contains some [comments](https://ltla.github.io/IndependentFilter2015/theory/nb_counts.html) about the use of the overall NB mean for filtering.
- `simulations`: This contains extensive simulations to demonstrate the independence of the _NB mean_ from _p_-values computed using _edgeR_.
The use of the NB mean is (very weakly!) justified by analogy to the normal case, using the maximum likelihood estimate of the mean in both cases.
This is necessary as an equivalent proof for the independence of the NB mean does not seem to be possible.
