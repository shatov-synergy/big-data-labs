# ============================================================
# Лабораторный практикум 2 — Задание 1
# Кластерный анализ набора данных Wine (K-means, PAM, CLARA)
# Скрипт запускается из каталога labs/lab-02
# ============================================================

# Пакеты и библиотеки
# (в Docker-образе пакеты уже установлены; ставятся только при отсутствии)
pkgs <- c("tidyverse", "cluster", "factoextra", "patchwork", "RColorBrewer",
          "mclust", "caret", "rattle")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new) > 0) install.packages(new)

library(tidyverse)
library(cluster)
library(factoextra)
library(patchwork)
library(RColorBrewer)
library(mclust)
library(caret)
library(rattle)
library(dplyr)

out_dir <- "output"
data_dir <- if (dir.exists("../../data")) "../../data" else "data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# Данные Wine.
# Основной путь — data(wine) из пакета rattle, как в коде преподавателя.
# Если набора в пакете нет, читаем кэш data/wine.csv либо UCI.
load_wine <- function() {
  items <- tryCatch(
    data(package = "rattle")$results[, "Item"],
    error = function(e) character()
  )
  if ("wine" %in% items) {
    data("wine", package = "rattle", envir = .GlobalEnv)
    if (exists("wine", envir = .GlobalEnv) && is.data.frame(wine) &&
        nrow(wine) > 0 && "Type" %in% names(wine)) {
      cat("Набор wine загружен из пакета rattle:", nrow(wine), "наблюдений\n")
      return(get("wine", envir = .GlobalEnv))
    }
  }

  cache_file <- file.path(data_dir, "wine.csv")
  cols <- c("Type", "Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium",
            "Phenols", "Flavanoids", "Nonflavanoids", "Proanthocyanins",
            "Color", "Hue", "Dilution", "Proline")

  if (file.exists(cache_file)) {
    cat("Читаю кэшированный набор Wine:", cache_file, "\n")
    cached <- read.csv(cache_file)
    cached$Type <- factor(cached$Type)
    return(cached)
  }

  urls <- c(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
    "https://archive.ics.uci.edu/static/public/109/wine/wine.data"
  )
  tmp <- tempfile()
  ok <- FALSE
  for (url in urls) {
    got <- tryCatch({
      download.file(url, tmp, quiet = TRUE, mode = "wb", method = "libcurl")
      file.info(tmp)$size > 1000
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(got)) {
      ok <- TRUE
      break
    }
  }
  if (!ok) {
    stop("Набор Wine недоступен: в пакете rattle нет data(wine), ",
         "кэш data/wine.csv отсутствует, загрузка UCI не удалась")
  }

  wine_dl <- read.csv(tmp, header = FALSE, col.names = cols)
  wine_dl$Type <- factor(wine_dl$Type)
  write.csv(wine_dl, cache_file, row.names = FALSE)
  cat("Wine загружен и сохранён в кэш:", cache_file, "\n")
  wine_dl
}

wine <- load_wine()
head(wine)
str(wine)

df <- wine[, -1]
df_scaled <- scale(df)
true_class <- wine$Type

cat("Пропусков в признаках:", sum(is.na(df)), "\n")
cat("Истинные классы:\n")
print(table(true_class))

# Число кластеров и палитра
k <- 3
pal <- brewer.pal(8, "Set2")

# Истинные классы
true_class <- wine$Type

# Автоматический выбор k
set.seed(123)
# 1) Silhouette метод
p_sil <- fviz_nbclust(df_scaled, kmeans, method = "silhouette")
ggsave(file.path(out_dir, "task1_nbclust_silhouette.png"), p_sil,
       width = 7, height = 5, dpi = 150)

# 2) Gap Statistic
gap_stat <- clusGap(df_scaled, FUN = kmeans, nstart = 25,
                    K.max = 8, B = 50)
p_gap <- fviz_gap_stat(gap_stat)
ggsave(file.path(out_dir, "task1_gap_stat.png"), p_gap,
       width = 7, height = 5, dpi = 150)

sil_df <- as.data.frame(p_sil$data)
k_sil <- as.integer(as.character(sil_df$clusters[which.max(sil_df$y)]))
k_gap <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"])
cat("Оптимальное k по силуэту:", k_sil, "\n")
cat("Оптимальное k по gap statistic (firstSEmax):", k_gap, "\n")
cat("Колонки графика силуэта:", paste(names(sil_df), collapse = ", "), "\n")
print(sil_df)

# КЛАСТЕРИЗАЦИЯ
set.seed(123)
km <- kmeans(df_scaled, centers = k, nstart = 25)
pam_fit <- pam(df_scaled, k = k)
clara_fit <- clara(df_scaled, k = k)

cat("Размеры кластеров K-means:\n")
print(table(km$cluster))
cat("Размеры кластеров PAM:\n")
print(table(pam_fit$clustering))
cat("Размеры кластеров CLARA:\n")
print(table(clara_fit$clustering))

# PCA (2 главные компоненты)
pca <- prcomp(df_scaled)
pca_df <- as.data.frame(pca$x[, 1:2])
pca_imp <- summary(pca)$importance
cat("Доля дисперсии PC1:", pca_imp[2, 1], " PC2:", pca_imp[2, 2], "\n")

# функция для PCA-графиков
plot_pca <- function(clusters, title) {
  pca_df$cluster <- factor(clusters)

  ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_manual(values = pal) +
    theme_minimal() +
    labs(title = title)
}

p1 <- plot_pca(km$cluster, "PCA + K-means")
p2 <- plot_pca(pam_fit$clustering, "PCA + PAM")
p3 <- plot_pca(clara_fit$clustering, "PCA + CLARA")
p_pca <- p1 + p2 + p3
ggsave(file.path(out_dir, "task1_pca_panels.png"), p_pca,
       width = 12, height = 4.2, dpi = 150)
ggsave(file.path(out_dir, "task1_pca_kmeans.png"), p1,
       width = 6.5, height = 5, dpi = 150)
ggsave(file.path(out_dir, "task1_pca_pam.png"), p2,
       width = 6.5, height = 5, dpi = 150)
ggsave(file.path(out_dir, "task1_pca_clara.png"), p3,
       width = 6.5, height = 5, dpi = 150)

# SILHOUETTE ГРАФИКИ
p4 <- fviz_silhouette(silhouette(km$cluster, dist(df_scaled))) +
  ggtitle("Silhouette: K-means")

p5 <- fviz_silhouette(silhouette(pam_fit$clustering, dist(df_scaled))) +
  ggtitle("Silhouette: PAM")

p6 <- fviz_silhouette(silhouette(clara_fit$clustering, dist(df_scaled))) +
  ggtitle("Silhouette: CLARA")
p_sils <- p4 + p5 + p6
ggsave(file.path(out_dir, "task1_silhouette_panels.png"), p_sils,
       width = 12, height = 4.2, dpi = 150)
ggsave(file.path(out_dir, "task1_silhouette_kmeans.png"), p4,
       width = 6.5, height = 5, dpi = 150)
ggsave(file.path(out_dir, "task1_silhouette_pam.png"), p5,
       width = 6.5, height = 5, dpi = 150)
ggsave(file.path(out_dir, "task1_silhouette_clara.png"), p6,
       width = 6.5, height = 5, dpi = 150)

sil_width <- function(clusters) {
  mean(silhouette(clusters, dist(df_scaled))[, "sil_width"])
}
sil_km <- sil_width(km$cluster)
sil_pam <- sil_width(pam_fit$clustering)
sil_clara <- sil_width(clara_fit$clustering)

# ARI (сравнение с реальными классами)
ari_km <- adjustedRandIndex(km$cluster, true_class)
ari_pam <- adjustedRandIndex(pam_fit$clustering, true_class)
ari_clara <- adjustedRandIndex(clara_fit$clustering, true_class)

cat("ARI (чем ближе к 1, тем лучше):\n")
cat("K-means:", ari_km, "\n")
cat("PAM    :", ari_pam, "\n")
cat("CLARA  :", ari_clara, "\n")
cat("Средний силуэт K-means:", sil_km, "\n")
cat("Средний силуэт PAM    :", sil_pam, "\n")
cat("Средний силуэт CLARA  :", sil_clara, "\n")

cat("Таблица сопряжённости K-means:\n")
print(table(Истинный = true_class, Кластер = km$cluster))
cat("Таблица сопряжённости PAM:\n")
print(table(Истинный = true_class, Кластер = pam_fit$clustering))
cat("Таблица сопряжённости CLARA:\n")
print(table(Истинный = true_class, Кластер = clara_fit$clustering))

saveRDS(
  list(
    n = nrow(wine),
    p = ncol(df),
    n_na = sum(is.na(df)),
    class_counts = as.integer(table(true_class)),
    class_names = names(table(true_class)),
    k = k,
    k_sil = as.integer(k_sil),
    k_gap = as.integer(k_gap),
    ari_km = ari_km,
    ari_pam = ari_pam,
    ari_clara = ari_clara,
    sil_km = sil_km,
    sil_pam = sil_pam,
    sil_clara = sil_clara,
    pc1 = unname(pca_imp[2, 1]),
    pc2 = unname(pca_imp[2, 2]),
    km_sizes = as.integer(table(km$cluster)),
    pam_sizes = as.integer(table(pam_fit$clustering)),
    clara_sizes = as.integer(table(clara_fit$clustering))
  ),
  file.path(out_dir, "task1_results.rds")
)

cat("TASK1_OK\n")
