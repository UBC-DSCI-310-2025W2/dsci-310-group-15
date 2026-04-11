FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    git-lfs \
    cmake \
    curl \
    gdebi-core \
    libzmq3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    pkg-config \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libwebp-dev \
    make \
    && rm -rf /var/lib/apt/lists/*

RUN git lfs install --system

RUN ARCH=$(dpkg --print-architecture) && \
    curl -LO "https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.40/quarto-1.6.40-linux-${ARCH}.deb" && \
    gdebi --non-interactive "quarto-1.6.40-linux-${ARCH}.deb" && \
    rm "quarto-1.6.40-linux-${ARCH}.deb"

RUN install2.r --error --skipinstalled --ncpus -1 remotes

RUN R -q -e "remotes::install_version('tidyverse',  version = '2.0.0',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('jsonlite',   version = '2.0.0',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('lubridate',  version = '1.9.5',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('vip',        version = '0.4.5',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('scales',     version = '1.4.0',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('ggcorrplot', version = '0.1.4.1',  repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('patchwork',  version = '1.3.2',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('purrr',      version = '1.1.0',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('checkmate',  version = '2.3.2',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('caret',      version = '7.0.1',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('janitor',    version = '2.2.1',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('pROC',       version = '1.19.0.1', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('docopt',     version = '0.7.2',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('knitr',      version = '1.50',     repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('here',       version = '1.0.1',    repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('tinytex',    version = '0.59',     repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('testthat',   version = '3.3.2',    repos = 'https://cloud.r-project.org')" $$ \
    R -q -e "remotes::install_version('pointblank',   version = '0.12.3',    repos = 'https://cloud.r-project.org')"

RUN apt-get update && apt-get install -y --no-install-recommends \
    texlive-xetex \
    texlive-fonts-recommended \
    texlive-latex-recommended \
    texlive-latex-extra \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/rstudio/project

CMD ["bash"]
