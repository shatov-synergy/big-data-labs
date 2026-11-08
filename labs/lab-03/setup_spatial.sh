#!/bin/bash
# Установка пространственного стека (sf, spdep, spatialreg, geosphere)
# в контейнер big-data-labs-rstudio для лабораторного практикума 3.
set -e
echo "== Системные библиотеки GDAL/GEOS/PROJ =="
apt-get update -qq
apt-get install -y --no-install-recommends \
  libgdal-dev libgeos-dev libproj-dev libudunits2-dev libsqlite3-dev \
  libssl-dev libxml2-dev libcurl4-openssl-dev
echo "== R-пакеты =="
Rscript -e '
options(repos = c(CRAN = "https://cran.ma.imperial.ac.uk"),
        timeout = 1800, Ncpus = max(1, parallel::detectCores() - 1))
pkgs <- c("sf", "spdep", "spatialreg", "geosphere")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat("Установка:", p, "\n")
    install.packages(p)
  } else cat("Уже установлен:", p, "\n")
}
ok <- sapply(pkgs, requireNamespace, quietly = TRUE)
cat("Итог:", paste(names(ok), ok, sep = "=", collapse = " "), "\n")
if (!all(ok)) stop("Не все пакеты установились")
'
echo "== Готово =="
