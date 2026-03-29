FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    git-lfs \
    python3 \
    python3-pip \
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
    && rm -rf /var/lib/apt/lists/*

RUN git lfs install --system

RUN pip3 install --no-cache-dir --break-system-packages jupyterlab

RUN install2.r --error --skipinstalled --ncpus -1 remotes

RUN R -q -e "remotes::install_version('tidyverse', version = '2.0.0', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('jsonlite', version = '2.0.0', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('lubridate', version = '1.9.5', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('vip', version = '0.4.5', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('scales', version = '1.4.0', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('ggcorrplot', version = '0.1.4.1', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('patchwork', version = '1.3.2', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('purrr', version = '1.1.0', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('caret', version = '7.0.1', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('janitor', version = '2.2.1', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('pROC', version = '1.19.0.1', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('pROC', version = '1.19.0.1', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('IRkernel', version = '1.3.2', repos = 'https://cloud.r-project.org')" && \
    R -q -e "remotes::install_version('testthat', version = '3.3.2', repos = 'https://cloud.r-project.org')" 

WORKDIR /home/rstudio/dsci-310-group-15

COPY src/steam_full_analysis.ipynb .
COPY data/ data/

RUN R -q  -e "IRkernel::installspec(user = FALSE)"

EXPOSE 8888

CMD ["jupyter", "lab", "--ip=0.0.0.0", "--port=8888", "--no-browser", "--allow-root"]
