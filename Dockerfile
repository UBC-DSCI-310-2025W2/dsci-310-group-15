FROM rocker/r-ver:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    git-lfs \
    python3 \
    python3-pip \
    python3-venv \
    libzmq3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN git lfs install --system

RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir jupyterlab

RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

WORKDIR /home/rstudio/dsci-310-group-15

COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile .Rprofile

RUN R -e "renv::restore(prompt = FALSE)"

RUN R -e "IRkernel::installspec(user = FALSE)"

COPY . .

EXPOSE 8888

CMD ["jupyter", "lab", "--ip=0.0.0.0", "--port=8888", "--no-browser", "--allow-root"]
