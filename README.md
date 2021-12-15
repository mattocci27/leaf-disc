[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# LMA comparison between leaf disc and whole-leaf estimates

# Usage

```bash
git clone https://github.com/mattocci27/leaf-disc
cd leaf-disc
```

### Running code on local

To make everything.

```bash
make
```

To make specific files.

For example:

```bash
make analysis
```

```bash
make docs/figs.html
```

### Running code in Docker

To make everything.

```bash
bash scripts/docker-env.sh
docker-compose up
```

## Requirements

If you are not using docker, you will need the following dependencies.

- R (4.1.2)
	- DT
	- [mattocci27/ggpubr](https://github.com/mattocci27/ggpubr)
	- [mattocci27/ggsma](https://github.com/mattocci27/ggsma)
	- kableExtra
	- patchwork
	- smatr
	- tictoc
	- tidyverse
	- tinytex
  - lmtest
- pandoc
- pandoc-crossref
- latexdiff
