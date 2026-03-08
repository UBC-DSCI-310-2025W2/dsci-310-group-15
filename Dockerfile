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

RUN install2.r --error --skipinstalled --ncpus -1 \
    tidyverse \
    jsonlite \
    lubridate \
    vip \
    scales \
    ggcorrplot \
    patchwork \
    purrr \
    IRkernel \
    caret \
    janitor \
    pROC

WORKDIR /home/rstudio/dsci-310-group-15

COPY steam_full_analysis.ipynb .
COPY data/ data/

RUN R -q  -e "IRkernel::installspec(user = FALSE)"

EXPOSE 8888

CMD ["jupyter", "lab", "--ip=0.0.0.0", "--port=8888", "--no-browser", "--allow-root"]
