[![DOI](https://zenodo.org/badge/438468062.svg)](https://zenodo.org/badge/latestdoi/438468062)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


# Sources and consequences of mismatch between leaf disc and whole-leaf leaf mass per area (LMA)

*Phisamai Maenpuen,
Masatoshi Katabuchi,
Yusuke Onoda,
Cong Zhou,
Jiao-Lin Zhang,
Ya-Jun Chen*

Code repository to run the analysis and generate the manuscript for Maenpuen et al. "Sources and consequences of mismatch between leaf disc and whole-leaf leaf mass per area (LMA)".

## Reproduce the results

Codes (R and STAN) and workflow are managed with the R package `targets` (https://github.com/ropensci/targets).

### Running code on local

To run analysis:

```bash
# To install R packages for the first run
# Rscript -e "renv::restore()"
Rscript run.R
```

To generate the manuscript:

```bash
make
```

Requirements:

- cmdstan 2.29.2
- pandoc
- pandoc-crossref
- latexdiff
- R (4.1.3)
	- renv (`renv::restore()` will install all the R packages)

### Running code in Apptainer (Linux)

First, change `RENV_PATHS_CACHE` in `radian.def` and `tinytex.def` to your path (i.e.,
`
RENV_PATHS_CACHE=<your_path>"
`
).

To build Apptainer containers:

```bash
sudo apptainer build radian.sif radian.def
sudo apptainer build tinytex.sif tinytex.def
```

To run analysis:

```bash
# To install R packages for the first run
# apptainer exec radian.sif Rscript -e "renv::restore()"
apptainer exec radian.sif Rscript run.R
```

To generate the manuscript:

```bash
# To install R packages for the first run
# Rscript -e "renv::restore()"
apptainer exec tinytex.sif make
```

Requirements:

- Apptainer (or Singularity)
- cmdstan 2.29.2 (radian.sif does not contain cmdstan)
