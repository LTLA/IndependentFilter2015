# IndependentFilter2015

This repository summarizes some work from my PhD thesis describing the independence of the NB mean as a filter statistic.
The aim is to allow others (and myself!) to examine and update this work without having to modify with the code in the thesis repository.
There are several components available:

- `proof`: This contains a proof of the independence of the sample mean to the _p_-values under normality.
It is an alternative to the succint but rather impenetrable proof provided by Bourgon _et al._ (https://dx.doi.org/10.1073/pnas.0914005107).
It also contains an extension to demonstrate independence on the presence of correlated tests.
- `simulaions`: This contains extensive simulations to demonstrate the independence of the _NB mean_ from _p_-values computed using _edgeR_.
The use of the NB mean is (weakly) justified by analogy to the normal case, using the maximum likelihood estimate of the mean in both cases.
This is necessary as an equivalent proof for the independence of the NB mean does not seem to be possible.
