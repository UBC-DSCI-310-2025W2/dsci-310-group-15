FROM rocker/r-ver:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 \
    python3-pip \
    libzmq3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    jupyter-core \
    jupyter-client \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install --no-cache-dir jupyterlab

RUN install2.r --error --skipinstalled --ncpus -1 \
  tidyverse \
  jsonlite \
  lubridate \
  vip \
  scales \
  ggcorrplot \
  patchwork \
  purrr \
  IRkernel  \
&& rm -rf /tmp/downloaded_packages

RUN R -e "IRkernel::installspec(user = FALSE)"

WORKDIR /home/rstudio/dsci-310-group-15

EXPOSE 8888

CMD ["jupyter", "lab", "--ip=0.0.0.0", "--port=8888", "--no-browser", "--allow-root"]
