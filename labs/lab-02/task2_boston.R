# ============================================================
# Лабораторный практикум 2 — Задание 2
# Прогнозирование медианной стоимости жилья (BostonHousing)
# Скрипт запускается из каталога labs/lab-02
# ============================================================

# Загрузка библиотек
# (в Docker-образе пакеты уже установлены; ставятся только при отсутствии)
pkgs <- c("caret", "mlbench", "randomForest", "gbm", "e1071", "ggplot2")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new) > 0) install.packages(new)

library(caret)
library(mlbench)
library(gbm)
library(e1071)
library(ggplot2)

out_dir <- "output"
data_dir <- if (dir.exists("../../data")) "../../data" else "data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# BostonHousing из mlbench, как в коде преподавателя.
# Если набор изъят из пакета, берём официальный CSV StatLib и кэш data/boston.csv.
load_boston <- function() {
  loaded <- tryCatch({
    data("BostonHousing", package = "mlbench", envir = .GlobalEnv)
    exists("BostonHousing", envir = .GlobalEnv) &&
      is.data.frame(BostonHousing) && nrow(BostonHousing) > 0 &&
      "medv" %in% names(BostonHousing)
  }, error = function(e) FALSE)
  if (isTRUE(loaded)) {
    cat("Набор BostonHousing загружен из пакета mlbench:",
        nrow(BostonHousing), "наблюдений\n")
    return(get("BostonHousing", envir = .GlobalEnv))
  }

  cache_file <- file.path(data_dir, "boston.csv")
  cols <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis",
            "rad", "tax", "ptratio", "b", "lstat", "medv")

  read_boston_file <- function(path) {
    raw <- readLines(path, warn = FALSE)
    data_lines <- grep("^\\s*-?[0-9]", raw, value = TRUE)
    if (length(data_lines) == 0) {
      return(NULL)
    }
    # Файл StatLib может быть уже CSV (наш кэш) либо текстовой таблицей,
    # в которой наблюдение разорвано на две строки.
    if (grepl(",", data_lines[1]) && !grepl("^\\s*-?[0-9]", raw[1])) {
      df_csv <- tryCatch(read.csv(path), error = function(e) NULL)
      if (!is.null(df_csv) && "medv" %in% names(df_csv)) {
        df_csv$chas <- factor(df_csv$chas)
        return(df_csv)
      }
    }
    nums <- scan(text = paste(data_lines, collapse = " "), quiet = TRUE)
    if (length(nums) %% 14 != 0) {
      return(NULL)
    }
    mat <- matrix(nums, ncol = 14, byrow = TRUE)
    out <- as.data.frame(mat)
    names(out) <- cols
    out$chas <- factor(out$chas)
    out
  }

  if (file.exists(cache_file)) {
    cat("Читаю кэшированный набор Boston:", cache_file, "\n")
    cached <- read.csv(cache_file)
    if (!"medv" %in% names(cached)) {
      stop("Кэш ", cache_file, " не содержит столбца medv")
    }
    cached$chas <- factor(cached$chas)
    return(cached)
  }

  urls <- c(
    "https://lib.stat.cmu.edu/datasets/boston",
    "http://lib.stat.cmu.edu/datasets/boston"
  )
  tmp <- tempfile()
  df_dl <- NULL
  for (url in urls) {
    got <- tryCatch({
      download.file(url, tmp, quiet = TRUE, mode = "wb", method = "libcurl")
      file.info(tmp)$size > 1000
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(got)) next
    df_dl <- read_boston_file(tmp)
    if (!is.null(df_dl) && nrow(df_dl) > 0) break
    df_dl <- NULL
  }
  if (is.null(df_dl)) {
    stop("Набор BostonHousing недоступен: пакет mlbench его не отдал, ",
         "кэш data/boston.csv отсутствует, загрузка lib.stat.cmu.edu не удалась")
  }
  write.csv(df_dl, cache_file, row.names = FALSE)
  cat("Boston загружен и сохранён в кэш:", cache_file,
      "(", nrow(df_dl), "наблюдений )\n")
  df_dl
}

df <- load_boston()

# Структура и описательные статистики
str(df)
summary(df)

# Проверка пропусков
cat("Пропусков:", sum(is.na(df)), "\n")

# Распределение целевой переменной
png(file.path(out_dir, "task2_medv_hist.png"),
    width = 900, height = 600, res = 120)
hist(df$medv,
     breaks = 30,
     col = "steelblue",
     main = "Распределение медианной стоимости жилья",
     xlab = "medv (тыс. долл.)")
dev.off()

# Корреляционная матрица предикторов
cor_matrix <- cor(df[, sapply(df, is.numeric)])
cat("Корреляции с medv:\n")
print(round(cor_matrix[, "medv"], 2))

# Разделение
set.seed(123)
idx <- createDataPartition(df$medv, p = 0.8, list = FALSE)
idx <- as.integer(idx)
train_set <- df[idx, ]
test_set  <- df[-idx, ]
cat("Обучающая выборка:", nrow(train_set),
    " тестовая:", nrow(test_set), "\n")

# Контроль пересэмплирования
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  savePredictions = "final"
)

# Обучение моделей
models_list <- list()

# Линейная регрессия (базовая модель)
models_list$lm <- train(
  medv ~ .,
  data = train_set,
  method = "lm",
  trControl = ctrl
)

#  k-ближайших соседей
models_list$knn <- train(
  medv ~ .,
  data = train_set,
  method = "knn",
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneLength = 10
)

#  Случайный лес
models_list$rf <- train(
  medv ~ .,
  data = train_set,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5
)

#  Градиентный бустинг
models_list$gbm <- train(
  medv ~ .,
  data = train_set,
  method = "gbm",
  trControl = ctrl,
  tuneLength = 5,
  verbose = FALSE
)

# Сравнение моделей по кросс-валидации
resamples_result <- resamples(models_list)

# Сводка по метрикам RMSE, Rsquared, MAE
resample_summary <- summary(resamples_result)
print(resample_summary)

save_device_plot <- function(path, expr) {
  png(path, width = 900, height = 600, res = 120)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

# Визуальное сравнение
save_device_plot(file.path(out_dir, "task2_rmse_bwplot.png"),
                 print(bwplot(resamples_result, metric = "RMSE",
                              main = "Сравнение моделей по RMSE")))
save_device_plot(file.path(out_dir, "task2_rsq_bwplot.png"),
                 print(bwplot(resamples_result, metric = "Rsquared",
                              main = "Сравнение моделей по R²")))
save_device_plot(file.path(out_dir, "task2_rmse_dotplot.png"),
                 print(dotplot(resamples_result, metric = "RMSE")))

# Выбор лучшей модели по RMSE
rmse_values <- sapply(models_list, function(m) min(m$results$RMSE))
best_model_name <- names(which.min(rmse_values))
cat("Лучшая модель:", best_model_name, "\n")
cat("RMSE по сетке настроек:\n")
print(round(rmse_values, 4))

best_model <- models_list[[best_model_name]]

# Прогноз на тестовой выборке
pred_test <- predict(best_model, newdata = test_set)

# Метрики качества
test_metrics <- postResample(pred = pred_test, obs = test_set$medv)
print(test_metrics)

# Предсказанные против фактических
png(file.path(out_dir, "task2_pred_vs_actual.png"),
    width = 900, height = 600, res = 120)
plot(pred_test, test_set$medv,
     xlab = "Предсказанная стоимость",
     ylab = "Фактическая стоимость",
     main = "Качество прогноза на тестовой выборке",
     pch = 19, col = "steelblue")
abline(0, 1, col = "red", lty = 2, lwd = 2)
dev.off()

# График остатков
residuals_test <- test_set$medv - pred_test
png(file.path(out_dir, "task2_residuals.png"),
    width = 900, height = 600, res = 120)
plot(pred_test, residuals_test,
     xlab = "Предсказанная стоимость",
     ylab = "Остатки",
     main = "График остатков",
     pch = 19, col = "steelblue")
abline(h = 0, col = "red", lty = 2, lwd = 2)
dev.off()

cat("Средний остаток:", mean(residuals_test),
    " SD:", sd(residuals_test),
    " max |e|:", max(abs(residuals_test)), "\n")

# Важность переменных для случайного леса
varImp_rf <- varImp(models_list$rf)
print(varImp_rf)
png(file.path(out_dir, "task2_varimp_rf.png"),
    width = 900, height = 600, res = 120)
print(plot(varImp_rf, main = "Важность предикторов (Random Forest)"))
dev.off()

# Важность для бустинга
varImp_gbm <- varImp(models_list$gbm)
print(varImp_gbm)
png(file.path(out_dir, "task2_varimp_gbm.png"),
    width = 900, height = 600, res = 120)
print(plot(varImp_gbm, main = "Важность предикторов (GBM)"))
dev.off()

# Сравнение важности между моделями.
# В части версий caret нет метода varImp() для объекта resamples.
varImp_compare <- tryCatch(
  varImp(resamples_result, scale = FALSE),
  error = function(e) {
    message("varImp(resamples) недоступен: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(varImp_compare)) {
  png(file.path(out_dir, "task2_varimp_compare.png"),
      width = 900, height = 600, res = 120)
  print(plot(varImp_compare, main = "Сравнение важности предикторов"))
  dev.off()
} else {
  rf_imp <- varImp(models_list$rf, scale = TRUE)$importance
  gbm_imp <- varImp(models_list$gbm, scale = TRUE)$importance
  cmp <- data.frame(
    variable = rownames(rf_imp),
    RF = rf_imp[, 1],
    GBM = gbm_imp[rownames(rf_imp), 1]
  )
  p_cmp <- ggplot(cmp, aes(x = RF, y = GBM, label = variable)) +
    geom_point(color = "steelblue", size = 2.5) +
    geom_text(vjust = -0.6, size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "red") +
    theme_minimal() +
    labs(title = "Сравнение важности предикторов",
         x = "Random Forest", y = "GBM")
  ggsave(file.path(out_dir, "task2_varimp_compare.png"), p_cmp,
         width = 7, height = 6, dpi = 150)
  print(cmp)
}

cv_stats <- resample_summary$statistics
saveRDS(
  list(
    n = nrow(df),
    p = ncol(df) - 1,
    n_na = sum(is.na(df)),
    n_train = nrow(train_set),
    n_test = nrow(test_set),
    medv_mean = mean(df$medv),
    medv_median = median(df$medv),
    medv_sd = sd(df$medv),
    cor_medv = round(cor_matrix[, "medv"], 4),
    rmse_values = rmse_values,
    best_model_name = best_model_name,
    test_metrics = test_metrics,
    residual_mean = mean(residuals_test),
    residual_sd = sd(residuals_test),
    residual_max_abs = max(abs(residuals_test)),
    cv_rmse = cv_stats$RMSE,
    cv_rsq = cv_stats$Rsquared,
    cv_mae = cv_stats$MAE,
    imp_rf = varImp_rf$importance,
    imp_gbm = varImp_gbm$importance,
    varimp_compare_fallback = is.null(varImp_compare)
  ),
  file.path(out_dir, "task2_results.rds")
)

cat("TASK2_OK\n")
