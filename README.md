# dsci-310-group-15
# Predicting Whether a Steam Game is Free

**CONTRIBUTORS:** Buyang Daffa, The Quach, Elaine Tao, Jinghan Zhang

___

## Overview

Steam is a gaming storefront published by Valve and hosts well over 30,000 games created by hundreds of different game developers. Games can either be purchased at varying price points, or installed for free. Our project aims to use consumer sentiment, developer, genre, release year, and supported platforms to determine whether a game will be free or not. We then use these features to perform supervised machine learning on these features using logistic regression.

Our analysis determined that games that implemented family sharing, were single- or multiplayer, had Steam leaderboards, and were demos were most likely to be free games. By far the most influential category was whether the game had implemented family sharing.

## Project Structure
```text
dsci-310-group-15/
|-- .github/
|   `-- workflows/
|       `-- publish_docker_image.yml
|-- R/
|   |-- extract_values.R
|   |-- io_validation_utils.R
|   |-- plot_class_imbalance.R
|   |-- plot_functions.R
|   |-- plot_numeric_distributions.R
|   `-- train_test_split_data.R
|-- data/
|   |-- raw/
|   `-- games_sample.json
|-- renv/
|-- reports/
|   |-- references.bib
|   `-- steam_full_analysis.qmd
|-- results/
|-- scripts/
|   |-- 01_download-data.R
|   |-- 02_data-preprocessing.R
|   |-- 03_class-imbalance-check.R
|   |-- 04_numeric-features-distribution.R
|   |-- 05_additional-target-summary-plots.R
|   |-- 06_categorical-feature-plots.R
|   `-- 07_train-test-model.R
|-- src/
|   |-- steam_eda.Rmd
|   `-- steam_full_analysis.ipynb
|-- tests/
|   |-- testthat.R
|   `-- testthat/
|       |-- helper-source-r.R
|       |-- helper-toy-data.R
|       |-- helper-train_test_data.R
|       |-- test-03-class-imbalance.R
|       |-- test-04-numeric-features.R
|       |-- test-extract_values.R
|       |-- test-plot_values.R
|       |-- test-test_data.R
|       `-- test-train_data.R
|-- CODE_OF_CONDUCT.md
|-- CONTRIBUTING.md
|-- Dockerfile
|-- LICENSE
|-- Makefile
|-- README.md
`-- renv.lock
```


### Running the Data Analysis

There are two ways to run this analysis.

#### 1. Using Docker

This project uses Docker to make the computational environment reproducible. The Docker image is defined by the `Dockerfile` in the root of the repository, and the image is automatically built and pushed to Docker Hub using the GitHub Actions workflow in `.github/workflows/publish_docker_image.yml`.

First, clone this GitHub repository and navigate to the root directory:

```bash
git clone https://github.com/UBC-DSCI-310-2025W2/dsci-310-group-15.git
cd dsci-310-group-15
```

Then, build the docker image: 
```bash
docker build -t dsci310-project .
```

Then, launch the container:
```bash
docker run --rm -it -p 8888:8888 -v "$(pwd)":/home/rstudio/dsci-310-group-15 dsci310-project
```

After launching the container, open Jupyter Lab in your browser through the link in the terminal: `http://localhost:8888`


#### 2. Without using Docker

To replicate the analysis, clone this GitHub repository, install the dependencies listed below, and run the analysis from the root directory of this project.

###### Dependencies
R programming language:
* R (4.5.2)

R packages:
* tidyverse (2.0.0)
* jsonlite (2.0.0)
* lubridate (1.9.5)
* vip (0.4.5)
* scales (1.4.0)
* ggcorrplot (0.1.4.1)
* patchwork (1.3.2)
* purrr (1.1.0)
* caret (7.0.1)
* janitor (2.2.1)
* pROC (1.19.0.1)
* docopt (0.7.2)
* testthat (3.3.2)

### Data Analysis Pipeline With GNU Make
This project's `Makefile` automates the data analysis. To render the outputs, make sure that you have cloned the repository and navigated to the root directory:

``` bash
git clone https://github.com/UBC-DSCI-310-2025W2/dsci-310-group-15.git
cd dsci-310-group-15
```

Then, update the environment according to the docker instructions or running `renv::restore()`.

To generate the report (in html and pdf formats) and all the included figures, in the terminal, run:
```bash
make all 
```
This command generates an html and pdf of the report, found in `reports/`, as well as all the figures and tables included in the report, found in `results/`. 

To clear all the outputs (figures; html and pdf reports), run:
```bash
make clean
```

### Running tests
Unit tests are implemented with `testthat` in `tests/testthat/`.

Run all tests:
```bash
make test
```

Or run directly:
```bash
Rscript tests/testthat.R
```


## License
This project uses the **MIT License**. See `LICENSE` for more information.
