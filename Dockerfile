# RStudio Server для лабораторных работ по курсу
# «Продвинутые технологии работы с большими данными»
#
# Базовый образ rocker/rstudio уже содержит:
#   R 4.6.1, RStudio Server, tidyverse (dplyr, ggplot2, tidyr, broom и др.),
#   rmarkdown + pandoc, devtools.
FROM rocker/rstudio:4.6.1

# Системные библиотеки для компиляции R-пакетов из исходников:
# cmake — nloptr; libcurl/libxml2 — R-пакеты curl, XML и зависимые.
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
      cmake \
      libcurl4-openssl-dev \
      libxml2-dev \
      libuv1-dev \
  && rm -rf /var/lib/apt/lists/*

# Пакеты, необходимые для лабораторных работ (задачи 2 и 3).
# Скрипт сам выбирает доступное зеркало CRAN (см. docker/install_pkgs.R)
# и падает с ошибкой, если что-то не установилось.
COPY docker/install_pkgs.R /tmp/install_pkgs.R
RUN Rscript /tmp/install_pkgs.R

# Зеркало CRAN по умолчанию для установки пакетов из RStudio
RUN echo 'options(repos = c(CRAN = "https://cran.ma.imperial.ac.uk"), download.file.method = "libcurl")' \
      >> /usr/local/lib/R/etc/Rprofile.site

# Порт RStudio Server
EXPOSE 8787
