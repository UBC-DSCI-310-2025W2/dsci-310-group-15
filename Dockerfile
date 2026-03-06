FROM rocker/r-ver:4.4.2

RUN install2.r --error --skipinstalled --ncpus -1 \
  tidyverse \
  jsonlite \
  lubridate \
  vip \
  scales \
  ggcorrplot \
  patchwork \
  purrr \
&& rm -rf /tmp/downloaded_packages