# ============================================================
# Лабораторный практикум 1 — Задание 3
# Предобработка временных рядов (котировки Tesla)
# ============================================================

# Загрузка пакетов и библиотек
# (в Docker-образе пакеты уже установлены; ставятся только при отсутствии)
pkgs <- c("quantmod", "dplyr", "ggplot2", "plotly", "forecast", "tseries",
          "moments", "lmtest", "rugarch", "isotree", "dbscan", "lazyeval",
          "zoo", "FinTS", "outliers")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new) > 0) install.packages(new)

library(quantmod)
library(ggplot2)
library(plotly)
library(forecast)
library(tseries)
library(moments)
library(lmtest)
library(rugarch)
library(isotree)
library(dbscan)
library(FinTS)
library(outliers)
library(dplyr)
library(htmlwidgets)

# Папки: скрипт запускается из labs/lab-01
out_dir  <- "output"
data_dir <- if (dir.exists("../../data")) "../../data" else "data"
if (!dir.exists(out_dir))  dir.create(out_dir, recursive = TRUE)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
cache_file <- file.path(data_dir, "tsla.csv")

# --------------------------------------------------------------------
# ЧАСТЬ 1. Интерактивный анимированный график за последние 100 сессий
# --------------------------------------------------------------------

# Загрузка данных из yahoo (с кэшированием в data/tsla.csv)
get_tsla <- function(from = Sys.Date() - 5 * 365) {
  if (file.exists(cache_file)) {
    cat("Читаю кэшированные котировки:", cache_file, "\n")
    df <- read.csv(cache_file)
    df$Date <- as.Date(df$Date)
    return(df)
  }
  invisible(getSymbols("TSLA", src = "yahoo", from = from))
  tsla <- na.omit(TSLA)
  df <- data.frame(Date = index(tsla), coredata(tsla))
  names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  write.csv(df, cache_file, row.names = FALSE)
  cat("Котировки загружены и сохранены в кэш:", cache_file, "\n")
  df
}

df_full <- get_tsla()
df <- tail(df_full, 100)
df$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# Интерактивная визуализация
df <- df %>%
  accumulate_by(~ID)

p <- ggplot(df, aes(ID, Close, frame = frame)) +
  geom_line()

fig <- ggplotly(p) %>%
  layout(
    title = "TSLA: Last 100 days",
    yaxis = list(
      title = "Close",
      zeroline = F,
      tickprefix = "$"
    ),
    xaxis = list(
      title = "Day",
      zeroline = F,
      showgrid = F
    )
  ) %>%
  animation_opts(
    frame = 100,
    transition = 0,
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Day "
    )
  )

fig
saveWidget(as_widget(fig), file.path(out_dir, "task3_animated_close.html"),
           selfcontained = TRUE)

# --------------------------------------------------------------------
# ЧАСТЬ 2. Полный цикл анализа ряда за 5 лет
# --------------------------------------------------------------------

df <- df_full

# Предобработка: регулярная структура торговых дней
# (%u: 6 = суббота, 7 = воскресенье — не зависит от локали)
full_idx <- seq(min(df$Date), max(df$Date), by = "day")
full_idx <- full_idx[!format(full_idx, "%u") %in% c("6", "7")]
df <- merge(data.frame(Date = full_idx), df, by = "Date", all.x = TRUE)
df$Close  <- na.interp(df$Close)
df$Volume <- na.interp(df$Volume)
df$logClose <- log(df$Close)
df$ret <- c(NA, diff(df$logClose))

# Сохранение предобработанного ряда
write.csv(df, file.path(data_dir, "tsla_preprocessed.csv"), row.names = FALSE)

# Проверка стационарности
cat("\n=== ADF-тест, лог-цена ===\n");    print(adf.test(na.omit(df$logClose)))
cat("\n=== KPSS-тест, лог-цена ===\n");   print(kpss.test(na.omit(df$logClose)))
cat("\n=== PP-тест, лог-цена ===\n");     print(pp.test(na.omit(df$logClose)))
cat("\n=== ADF-тест, доходности ===\n");  print(adf.test(na.omit(df$ret)))
cat("\n=== KPSS-тест, доходности ===\n"); print(kpss.test(na.omit(df$ret)))
cat("\n=== PP-тест, доходности ===\n");   print(pp.test(na.omit(df$ret)))

# ACF / PACF
png(file.path(out_dir, "task3_acf_ret.png"), width = 1000, height = 500, res = 110)
acf(na.omit(df$ret), main = "ACF доходностей")
dev.off()
png(file.path(out_dir, "task3_pacf_ret.png"), width = 1000, height = 500, res = 110)
pacf(na.omit(df$ret), main = "PACF доходностей")
dev.off()

# STL-декомпозиция
ts_close <- ts(df$Close, frequency = 252)
stl_fit <- stl(ts_close, s.window = "periodic")
png(file.path(out_dir, "task3_stl.png"), width = 1000, height = 750, res = 110)
plot(stl_fit)
dev.off()
stl_df <- data.frame(Date = df$Date,
                     Trend     = as.numeric(stl_fit$time.series[, "trend"]),
                     Seasonal  = as.numeric(stl_fit$time.series[, "seasonal"]),
                     Remainder = as.numeric(stl_fit$time.series[, "remainder"]))
write.csv(stl_df, file.path(out_dir, "task3_stl_components.csv"), row.names = FALSE)

# Статистические методы выбросов
q1 <- quantile(df$ret, .25, na.rm = TRUE); q3 <- quantile(df$ret, .75, na.rm = TRUE)
iqr <- q3 - q1
out_iqr <- which(df$ret < q1 - 1.5*iqr | df$ret > q3 + 1.5*iqr)
z <- scale(df$ret); out_z <- which(abs(z) > 3)
mz <- 0.6745 * (df$ret - median(df$ret, na.rm = TRUE)) / mad(df$ret, na.rm = TRUE)
out_mz <- which(abs(mz) > 3.5)

# ML-методы выбросов
feat <- scale(na.omit(df[, c("Close","Volume","ret")]))
iso <- isolation.forest(feat, ntrees = 100, seed = 42)
score_iso <- predict(iso, feat)
out_iso <- which(score_iso > quantile(score_iso, .95))
lof <- lof(feat, k = 20)
out_lof <- which(lof > quantile(lof, .95))
dbc <- dbscan(feat, eps = 0.5, minPts = 5)
out_dbscan <- which(dbc$cluster == 0)

# Границы выбросов по IQR (для отчёта)
cat(sprintf("\nГраницы нормы по доходностям (IQR): [%.6f ; %.6f]\n",
            q1 - 1.5*iqr, q3 + 1.5*iqr))
cat(sprintf("Число аномалий: IQR = %d, z-score = %d, mod. z-score = %d, ISO = %d, LOF = %d, DBSCAN = %d\n",
            length(out_iqr), length(out_z), length(out_mz),
            length(out_iso), length(out_lof), length(out_dbscan)))

df$anomaly <- FALSE
df$anomaly[out_iqr] <- TRUE
df$anomaly[out_z] <- TRUE
df$anomaly[out_mz] <- TRUE
df$anomaly[out_iso] <- TRUE
df$anomaly[out_lof] <- TRUE
df$anomaly[out_dbscan] <- TRUE

# Итоговая визуализация аномалий
p_anom <- ggplot(df, aes(x = Date, y = Close)) +
  geom_line(color = "steelblue") +
  geom_point(data = subset(df, anomaly), aes(x = Date, y = Close),
             color = "red", size = 1.2) +
  labs(title = "TSLA: цена закрытия и аномальные сессии (красным)",
       x = "Дата", y = "Цена закрытия, $")
print(p_anom)
ggsave(file.path(out_dir, "task3_anomalies.png"), p_anom, width = 10, height = 5, dpi = 150)

cat(sprintf("Всего аномальных сессий (объединённый признак anomaly): %d из %d (%.1f%%)\n",
            sum(df$anomaly), nrow(df), 100 * mean(df$anomaly)))

# Сохранение итогового ряда с признаком anomaly
write.csv(df, file.path(out_dir, "task3_tsla_anomaly.csv"), row.names = FALSE)
