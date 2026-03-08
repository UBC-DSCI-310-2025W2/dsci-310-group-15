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

RUN R -q -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

WORKDIR /home/rstudio/dsci-310-group-15

COPY renv.lock renv.lock 

RUN R -q -e "renv::restore(lockfile = 'renv.lock', prompt = FALSE)"

RUN R -q  -e "IRkernel::installspec(user = FALSE)"

COPY steam_full_analysis.ipynb .
COPY data/ data/

EXPOSE 8888

CMD ["jupyter", "lab", "--ip=0.0.0.0", "--port=8888", "--no-browser", "--allow-root"]
