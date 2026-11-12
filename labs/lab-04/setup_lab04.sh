#!/bin/bash
# Установка igraph в контейнер big-data-labs-rstudio для практикума 4
# (движок graph в arulesViz). Используется только до пересборки образа:
# пакет уже добавлен в docker/install_pkgs.R и попадёт в образ при
# следующем docker compose build.
set -e
Rscript -e '
options(repos = c(CRAN = "https://cran.ma.imperial.ac.uk"), timeout = 1800)
if (!requireNamespace("igraph", quietly = TRUE)) {
  install.packages("igraph")
}
cat("igraph установлен:", requireNamespace("igraph", quietly = TRUE), "\n")
'
echo "== Готово =="
