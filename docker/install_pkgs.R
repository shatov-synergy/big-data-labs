# Установка R-пакетов для лабораторных работ.
# P3M (p3m.dev) из сетей РФ/ ряда провайдеров часто недоступен из Docker-контейнеров,
# поэтому перебираем зеркала CRAN и берём первое доступное.

mirrors <- c(
  "https://cran.ma.imperial.ac.uk",
  "https://mirrors.tuna.tsinghua.edu.cn/CRAN",
  "https://mirror.las.iastate.edu/CRAN",
  "https://cloud.r-project.org"
)

for (m in mirrors) {
  ok <- tryCatch({
    db <- available.packages(repos = m)
    nrow(db) > 1000
  }, error = function(e) FALSE)
  if (ok) {
    options(repos = c(CRAN = m))
    cat("Используется зеркало CRAN:", m, "\n")
    break
  }
}

pkgs <- c("plotly", "quantmod", "forecast", "tseries", "moments",
          "lmtest", "rugarch", "isotree", "dbscan", "lazyeval",
          "zoo", "FinTS", "outliers", "htmlwidgets", "broom",
          # Лабораторный практикум 2: кластеризация Wine и регрессия Boston
          "tidyverse", "factoextra", "patchwork", "caret", "rattle",
          "mlbench", "randomForest", "gbm", "e1071",
          # Лабораторный практикум 3: пространственный стек (sf, spdep,
          # spatialreg, geosphere) ставится ad hoc скриптом
          # labs/lab-03/setup_spatial.sh — требует системные GDAL/GEOS/PROJ,
          # которых нет в образе.
          # Лабораторный практикум 4: ассоциативные правила и прогнозирование
          # (igraph — движок графов в arulesViz, в rocker/rstudio отсутствует;
          # пакет holidays НЕ нужен: prophet содержит собственные данные
          # праздников для add_country_holidays, а с CRAN он недоступен
          # для этой версии R)
          "arules", "arulesViz", "igraph", "prophet", "yardstick",
          "readxl")

install.packages(pkgs, Ncpus = 2)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) {
  stop("НЕ установлены пакеты: ", paste(missing, collapse = ", "))
}
cat("Все пакеты успешно установлены.\n")
