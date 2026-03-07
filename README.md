# dsci-310-group-15

## Usage

There are two ways to run this analysis.

### 1. Using Docker


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


### 2. Without using Docker

To replicate the analysis, clone this GitHub repository, install the dependencies listed below, and run the analysis from the root directory of this project.

Dependencies
R version 
* R 4.4.2 

R packages:
* tidyverse
* jsonlite
* lubridate
* vip
* scales
* ggcorrplot
* patchwork
* purrr



