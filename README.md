[![DOI:xxxx](https://zenodo.org/badge/DOI/xxx/xxx.svg)](https://doi.org/10.1111/xxx)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


# Sources and consequences of mismatch between leaf disc and whole-leaf leaf mass per area (LMA)

*Phisamai Maenpuen,
Masatoshi Katabuchi,
Yusuke Onoda,
Cong Zhou,
Jiao-Lin Zhang,
Ya-Jun Chen*

# Usage

```bash
git clone https://github.com/mattocci27/leaf-disc
cd leaf-disc
```

## Running code on local

To run analysis:

```bash
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
	- renv (this will install all the R packages)

## Running code in Singularity

First, change `RENV_PATHS_CACHE` in `singularity.def` and `tinytex.def` to your path (i.e.,
`
RENV_PATHS_CACHE=<your_path>" >> /usr/local/lib/R/etc/Renviron.site
`
)

To build singularity containers:

```bash
sudo singularity build singularity.sif singularity.def
sudo singularity build tinytex.sif tinytex.def
```

To run analysis:

```bash
singularity exec singularity.sif Rscript run.R
```

To generate the manuscript:

```bash
singularity exec tinytex.sif make
```

Requirements:

- singularity
- cmdstan 2.29.2
