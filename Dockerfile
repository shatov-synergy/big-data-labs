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
      pkg-config \
      libcurl4-openssl-dev \
      libxml2-dev \
      libuv1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfreetype6-dev \
      libpng-dev \
      libtiff-dev \
      libwebp-dev \
      libfontconfig1-dev \
  && rm -rf /var/lib/apt/lists/*

# Пакеты, необходимые для лабораторных работ (задачи 2 и 3).
# Скрипт сам выбирает доступное зеркало CRAN (см. docker/install_pkgs.R)
# и падает с ошибкой, если что-то не установилось.
COPY docker/install_pkgs.R /tmp/install_pkgs.R
RUN Rscript /tmp/install_pkgs.R

# Зеркало CRAN по умолчанию для установки пакетов из RStudio
RUN echo 'options(repos = c(CRAN = "https://cran.ma.imperial.ac.uk"), download.file.method = "libcurl")' \
      >> /usr/local/lib/R/etc/Rprofile.site

# Копия материалов практикума. Bind-mount репозитория в Docker Desktop
# из этой сети иногда приходит пустым; скрипты остаются доступны в образе.
COPY labs/lab-02 /opt/lab/labs/lab-02
COPY data /opt/lab/data

# rsession линкуется с libR.so напрямую и не наследует LD_LIBRARY_PATH
# из обёртки /usr/local/bin/R. В rocker/rstudio:4.6.1 каталог библиотеки
# не прописан в ldconfig, поэтому сессия не стартует и в браузере
# бесконечно крутится «R is taking longer to start than usual».
# RStudio Server 2026.09 (discoverR) читает R_DOC_DIR из окружения процесса.
# Базовый образ задаёт только R_HOME. Без R_DOC_DIR rsession сразу
# завершается, а браузер остаётся на «R is taking longer to start than usual».
# rserver подхватывает переменные из /etc/rstudio/env-vars.
RUN echo "/usr/local/lib/R/lib" > /etc/ld.so.conf.d/R.conf && ldconfig \
  && printf '%s\n' 'R_DOC_DIR=/usr/local/lib/R/doc' > /etc/rstudio/env-vars
ENV R_DOC_DIR=/usr/local/lib/R/doc

# Порт RStudio Server
EXPOSE 8787
