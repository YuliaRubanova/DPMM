# Dirichlet Process Mixture Model

Dirichlet Process Mixture Model for choosing active mutational signatures in cancer genome. This work is a course project for CSC2506.

### Background
Cell processes leave a unique signature of mutation types in cancer genome. Using the mutational signatures, it is possible to infer the fraction of mutations contributed by each cell process. Mutational signatures are represented as multinomial distributions over 96 mutation types. However, the question of choosing the active mutational signatures is still an open area of research.

### Model
I propose to use the variation of Dirichlet process mixture model to find the active signatures. In this model, the set of active signatures is sampled from Dirichlet distribution and then is fitted using mixture of multinomials. The model is fit through Gibbs sampling.

### Code
The model is located in lda/dpmm.py. In this preliminary state, the work is not intended for public use.

