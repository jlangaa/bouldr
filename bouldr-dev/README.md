## bouldr README
The `bouldr` package is a toolkit for running Receiver Operator Characteristic (ROC) Curve analyses using a simple, formula interface. It also allows for intuitive visualization and statistical comparison of the curves. The statistical core of the package is `pROC`, a package developed by Xavier Robin and colleagues (see below). Many ROC packages focus on machine learning and classification use-cases. However, ROC is useful in psychology as well -- particularly in developing assessment instruments. A continuous score on a test can be ROC-ed against a target diagnosis to test the diagnostic efficiency of the test. ROC is the preferred approach because it assesses sensativity and specificity across the range of the scale, and can be used to identify optimal cut-scores. This package relies on pROC to perform the computation.

## Requirements
- `R` version 3.5.0 or greater
- `tidyr`
- `dplyr`
- `magrittr`
- `ggplot2`
- `pROC`
- `RcppAlgos`
- `broom`
- `stats`

## Installation

`bouldr` is not currently on CRAN, so you'll need `devtools` to install:

```
  devtools::install_github('jlangaa/bouldr')
```

## Examples

Use `?bouldr` and `?generate_data` to see examples.

## References

Xavier Robin, Natacha Turck, Alexandre Hainard, Natalia Tiberti, Frédérique Lisacek, Jean-Charles Sanchez and Markus Müller (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. *BMC Bioinformatics*, 12, p. 77.  DOI:10.1186/1471-2105-12-77 <http://www.biomedcentral.com/1471-2105/12/77/>
