# dsci-310-group-15
# Predicting Whether a Steam Game is Free

**CONTRIBUTORS:** Buyang Daffa, The Quach, Elaine Tao, Jinghan Zhang

---

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

---

## Running the Analysis

There are two ways to run this analysis: using Docker (recommended) or installing dependencies locally.

### Method 1: Using Docker (Recommended)

Docker ensures the analysis runs in a fully reproducible environment with all dependencies pre-installed.

**Prerequisites:** [Docker Desktop](https://www.docker.com/products/docker-desktop/) must be installed and running.

**Step 1 — Clone the repository:**

```bash
git clone https://github.com/UBC-DSCI-310-2025W2/dsci-310-group-15.git
cd dsci-310-group-15
```

**Step 2 — Pull the pre-built image from Docker Hub:**

```bash
docker pull thequach/dsci-310-group-15:latest
```

> Alternatively, build the image locally from the `Dockerfile`:
> ```bash
> docker build -t thequach/dsci-310-group-15:latest .
> ```
> Note: this can take 10–20 minutes as R packages are compiled from source.

**Step 3 — Launch the container, mounting the repository into it:**

```bash
docker run --rm -it \
  -p 8888:8888 \
  -v "$(pwd)":/home/rstudio/project \
  -w /home/rstudio/project \
  thequach/dsci-310-group-15:latest \
  bash
```

> **Windows (Command Prompt):** replace `$(pwd)` with `%cd%`  
> **Windows (PowerShell):** replace `$(pwd)` with `${PWD}`

This drops you into a `bash` shell inside the container, with the repository available at `/home/rstudio/project`.

**Step 4 — Run the full analysis pipeline inside the container:**

```bash
make all
```

This generates:
- All figures and tables in `results/`
- The HTML and PDF reports in `reports/`

**Step 5 — Exit the container when done:**

```bash
exit
```

---

### Method 2: Running Locally (Without Docker)

**Step 1 — Clone the repository:**

```bash
git clone https://github.com/UBC-DSCI-310-2025W2/dsci-310-group-15.git
cd dsci-310-group-15
```

**Step 2 — Install R and all required packages.**

Required R version: **4.5.2**  
Download from: <https://cran.r-project.org/>

Each pipeline step can also be run directly from the project root:

```bash
Rscript scripts/01_download-data.R <input_url> <output_data_dir>
Rscript scripts/02_data-preprocessing.R <input_data_dir> <output_data_dir> <table_output_dir>
Rscript scripts/03_class-imbalance-check.R <input_data_dir> <plot_object_dir> <figure_dir>
Rscript scripts/04_numeric-features-distributions.R <input_data_dir> <plot_object_dir> <figure_dir>
Rscript scripts/05_additional-target-summary-plots.R <input_data_dir> <plot_object_dir> <figure_dir>
Rscript scripts/06_categorical-features-plots.R <input_data_dir> <plot_object_dir> <figure_dir>
Rscript scripts/07_train-test-model.R <input_data_dir> <results_dir>
```

To clear all the outputs (figures; html and pdf reports), run:
```bash
make clean
Required R packages (exact versions):

| Package | Version |
|---|---|
| tidyverse | 2.0.0 |
| jsonlite | 2.0.0 |
| lubridate | 1.9.5 |
| vip | 0.4.5 |
| scales | 1.4.0 |
| ggcorrplot | 0.1.4.1 |
| patchwork | 1.3.2 |
| purrr | 1.1.0 |
| caret | 7.0.1 |
| janitor | 2.2.1 |
| pROC | 1.19.0.1 |
| docopt | 0.7.2 |
| knitr | 1.50 |
| here | 1.0.1 |
| testthat | 3.3.2 |

The fastest way to install all packages at the correct versions is to restore the project's `renv` lockfile:

```r
install.packages("renv")
renv::restore()
```

**Step 3 — Run the full analysis pipeline:**

```bash
make all
```

---

## Makefile Targets

| Target | Description |
|---|---|
| `make all` | Download data, run all scripts, render HTML and PDF reports |
| `make clean` | Remove all generated outputs (results, rendered reports) |
| `make test` | Run the full `testthat` test suite |

---

## Running Tests

Unit tests are implemented with `testthat` in `tests/testthat/`.

Run all tests via Make:

```bash
make test
```

Or run them directly from R:

```bash
Rscript tests/testthat.R
```

---

## Script Reference

Each script is run automatically by `make all`. The table below documents each script's arguments in case you want to run them individually. All paths must be **relative to the project root** and must end with a trailing `/`.

| Script | Arguments | Example |
|---|---|---|
| `01_download-data.R` | `<output_dir>` — directory to save `games_sample.RDS` | `Rscript scripts/01_download-data.R data/` |
| `02_data-preprocessing.R` | `<input_dir>` `<output_dir>` `<csv_dir>` — directories for input RDS, output RDS, and output CSV | `Rscript scripts/02_data-preprocessing.R data/ data/ data/` |
| `03_class-imbalance-check.R` | `<input_dir>` `<rds_dir>` `<figure_dir>` | `Rscript scripts/03_class-imbalance-check.R data/ results/ results/` |
| `04_numeric-features-distributions.R` | `<input_dir>` `<rds_dir>` `<figure_dir>` | `Rscript scripts/04_numeric-features-distributions.R data/ results/ results/` |
| `05_additional-target-summary-plots.R` | `<input_dir>` `<rds_dir>` `<figure_dir>` | `Rscript scripts/05_additional-target-summary-plots.R data/ results/ results/` |
| `06_categorical-features-plots.R` | `<input_dir>` `<rds_dir>` `<figure_dir>` | `Rscript scripts/06_categorical-features-plots.R data/ results/ results/` |
| `07_train-test-model.R` | `<input_dir>` `<output_dir>` — directory containing `wrangled_table.RDS`, and directory to save results | `Rscript scripts/07_train-test-model.R data/ results/` |

---

## License

This project uses the **MIT License**. See `LICENSE` for more information.
