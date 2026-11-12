# Аудит Задачи 2, Проход 3: воспроизведение экспериментов Prophet,
# финальные модели, pre_new_year и праздники RU. Оригиналы не меняются.
# Запуск: docker exec -w /project/labs/lab-04 lab04 Rscript audit/pass3_z2_prophet.R
library(readxl); library(prophet); library(dplyr)

# --- данные (повторение подготовки работы, блоки 2-4, сверено в проходе 2) ---
Магазин <- read_excel("data/Magazin.xlsx")
df <- Магазин %>%
  filter(Product_Category == "Овощи - Фрукты") %>%
  mutate(ds = as.Date(Date), y = as.numeric(Sales)) %>%
  select(ds, y, Price, Temperature)
w_raw <- read.csv("data/weather_vdnh_2023_2025.csv", skip = 2, check.names = FALSE)
weather <- data.frame(ds = as.Date(w_raw[[1]]),
                      t_mean = as.numeric(w_raw[[4]]),
                      precip = as.numeric(w_raw[[5]]))
df_ext <- df %>% left_join(weather, by = "ds")
H <- 30; split_date <- max(df_ext$ds) - H
train_ext <- df_ext %>% filter(ds <= split_date)
test_ext  <- df_ext %>% filter(ds >  split_date)
years <- unique(format(df_ext$ds, "%Y"))
pre_new_year <- data.frame(holiday = "pre_new_year",
                           ds = as.Date(paste0(years, "-12-28")),
                           lower_window = 0, upper_window = 3)

# --- независимые метрики (не yardstick) ---
m_rmse <- function(y, yh) sqrt(mean((y - yh)^2))
m_mae  <- function(y, yh) mean(abs(y - yh))
m_mape <- function(y, yh) 100 * mean(abs((y - yh) / y))

# --- независимая обёртка (параметры как в tuning.csv) ---
fit_one <- function(ws, ncp, sps, cps, hps, pre_ny, regressors = NULL) {
  m <- prophet(yearly.seasonality = TRUE, weekly.seasonality = ws,
               daily.seasonality = FALSE,
               holidays = if (pre_ny) pre_new_year else NULL,
               holidays.prior.scale = hps,
               seasonality.prior.scale = sps,
               changepoint.prior.scale = cps,
               n.changepoints = ncp)
  m <- add_country_holidays(m, country_name = "RU")
  for (r in regressors) m <- add_regressor(m, r)
  cols <- c("ds", "y", regressors)
  m <- fit.prophet(m, train_ext %>% select(all_of(cols)))
  fc <- predict(m, test_ext %>% select(all_of(c("ds", regressors))))
  fc$yhat
}

cat("=== A. Перезапуск всех конфигураций подбора (tuning.csv) ===\n")
tuning <- read.csv("output/task2_prophet_tuning.csv")
cat("строк в tuning.csv:", nrow(tuning), "\n")
aud <- data.frame()
for (i in seq_len(nrow(tuning))) {
  r <- tuning[i, ]
  yh <- fit_one(r$weekly_seasonality, r$n_changepoints,
                r$seasonality_prior_scale, r$changepoint_prior_scale,
                r$holidays_prior_scale, r$pre_new_year)
  aud <- rbind(aud, data.frame(
    i = i, stage = r$stage,
    RMSE_aud = m_rmse(test_ext$y, yh),  RMSE_csv = r$RMSE,
    MAE_aud  = m_mae(test_ext$y, yh),   MAE_csv = r$MAE,
    MAPE_aud = m_mape(test_ext$y, yh),  MAPE_csv = r$MAPE))
}
aud$dRMSE <- round(aud$RMSE_aud - aud$RMSE_csv, 6)
aud$dMAPE <- round(aud$MAPE_aud - aud$MAPE_csv, 6)
print(aud[, c("i", "stage", "RMSE_aud", "RMSE_csv", "dRMSE", "dMAPE")],
      digits = 5, row.names = FALSE)
cat("max |dRMSE|:", max(abs(aud$dRMSE)), "| max |dMAPE|:", max(abs(aud$dMAPE)), "\n")

cat("\n--- победители этапов по CSV и по аудиту совпадают? ---\n")
for (st in unique(tuning$stage)) {
  idx <- which(tuning$stage == st)
  w_csv <- idx[which.min(tuning$RMSE[idx])]
  w_aud <- idx[which.min(aud$RMSE_aud[idx])]
  cat(st, ": CSV строка", w_csv, "| аудит строка", w_aud,
      "| совпадение:", w_csv == w_aud, "\n")
}

cat("\n=== B. Финальные модели: сверка прогнозов с predictions.csv ===\n")
bp <- list(ws = 3, ncp = 40, sps = 0.5, cps = 0.05, hps = 5, ny = TRUE)
p <- read.csv("output/task2_predictions.csv")
models <- list(
  "Prophet (base)" = fit_one(bp$ws, bp$ncp, bp$sps, bp$cps, bp$hps, bp$ny),
  "Prophet (+ погода)" = fit_one(bp$ws, bp$ncp, bp$sps, bp$cps, bp$hps, bp$ny,
                                  c("t_mean", "precip")),
  "Prophet (+ погода + цена)" = fit_one(bp$ws, bp$ncp, bp$sps, bp$cps, bp$hps,
                                        bp$ny, c("t_mean", "precip", "Price")))
for (nm in names(models)) {
  csv <- p[p$model == nm, ]
  ok <- identical(as.Date(csv$ds), test_ext$ds)
  dmax <- max(abs(csv$predicted - models[[nm]]))
  cat(nm, "| даты совпадают:", ok, "| max|Δ прогноза|:", round(dmax, 6),
      "| RMSE аудита:", round(m_rmse(test_ext$y, models[[nm]]), 4),
      "| RMSE csv:", round(m_rmse(csv$y, csv$predicted), 4), "\n")
}

cat("\n=== C. pre_new_year против праздников RU (дубли эффекта) ===\n")
m0 <- prophet(yearly.seasonality = TRUE, daily.seasonality = FALSE)
m0 <- add_country_holidays(m0, country_name = "RU")
ru <- as.data.frame(m0$train.holiday.names)
cat("праздники RU, известных prophet:\n"); print(ru)
# модель с обоими наборами праздников — проверить наложение имён/дат
m1 <- prophet(holidays = pre_new_year, yearly.seasonality = TRUE,
              daily.seasonality = FALSE)
m1 <- add_country_holidays(m1, country_name = "RU")
m1 <- fit.prophet(m1, df_ext %>% select(ds, y))
hd <- predict(m1, df_ext %>% select(ds))
cat("\nдаты, где действует pre_new_year (28-31.12):",
    sum(format(df_ext$ds, "%m-%d") %in% c("12-28", "12-29", "12-30", "12-31")), "дней\n")
# имена праздников в модели
cat("train.holiday.names модели:", paste(m1$train.holiday.names, collapse = ", "), "\n")

cat("\nАУДИТ-3 ЗАВЕРШЁН\n")
