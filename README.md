[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# LMA comparison between leaf disc and whole-leaf estimates

# Usage

```bash
git clone https://github.com/mattocci27/leaf-disc
cd leaf-disc
```

### Running code on local

To run analysis.

```bash
Rscript run.R
```

To generate manuscript.

```bash
make
```

### Running code in Docker

To make everything.

```bash
bash scripts/docker-env.sh
docker-compose up
```

## Requirements

If you are not using docker, you will need the following dependencies.

- pandoc
- pandoc-crossref
- latexdiff
- R (4.1.3)
	- renv (this will install all the dependencies)
	- (or folloiwng packages)
	- DT
	- [mattocci27/ggpubr](https://github.com/mattocci27/ggpubr)
	- [mattocci27/ggsma](https://github.com/mattocci27/ggsma)
	- clustermq
	- extrafont
	- janitor
	- jsonlite
	- kableExtra
	- lmtest
	- modelr
	- patchwork
	- smatr
	- tarchetypes
	- targets
	- tictoc
	- tidyverse
	- tinytex
