# =====================================================================
# Лабораторный практикум 4. Задача 2.
# Прогнозирование продаж по товарной категории «Алкоголь» (Magazin.xlsx).
# Пять моделей: Prophet base, Prophet + weather, ETS, SARIMA и ансамбль
# Prophet + ETS; сравнение по RMSE, MAE и MAPE на тестовом периоде
# последних 30 дней.
#
# Базовый код преподавателя (assignments/lab-04/Skript koda ...
# Magazin Alko ispr.docx) сохранён дословно в блоках 2, 4, 5, 7, 8, 9,
# 10, 12, 13. Добавления и технические правки (по требованию задания —
# экспериментальный подбор параметров; документируются здесь):
#   1) чтение данных: добавлен вызов read_excel("data/Magazin.xlsx")
#      (в базовом коде объект «Магазин» предполагался уже созданным);
#   2) блок 3а — разведочный анализ: график исходного ряда с разбиением
#      train/test и недельный профиль продаж (день недели восстановлен
#      из даты: колонка Weekday файла не соответствует календарю);
#   3) блок 5а — параметризованная функция fit_prophet_tuned() и
#      вспомогательная metrics_row();
#   4) блок 6а — поэтапная экспериментальная настройка Prophet:
#      этап A (weekly.seasonality 3/5/10/15), этап B (n.changepoints
#      15/25/40), этап C (seasonality.prior.scale 0,5/1 ×
#      changepoint.prior.scale 0,01/0,05 × holidays.prior.scale 1/5/10)
#      и абляция — с праздником pre_new_year и без него;
#   5) блок 7а — сравнение спецификаций ETS: авто-подбор ets() и
#      ручные model = "AAA", "MAM", "MNM";
#   6) графики сохраняются в PNG в output/, метрика MAPE — в процентах;
#   7) блок 14 — экспорт CSV/rds-кэша результатов для отчёта.
# =====================================================================

# 1. Установка пакетов (если еще не установлены) ----------------------
# (holidays не нужен: prophet хранит данные праздников РФ внутри себя)
pkgs <- c("readxl", "prophet", "dplyr", "ggplot2",
          "yardstick", "tidyr", "stringr", "forecast", "zoo")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(readxl)
library(prophet)
library(dplyr)
library(ggplot2)
library(yardstick)
library(tidyr)
library(stringr)
library(forecast)
library(zoo)

options(scipen = 999)

out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 2. Данные ------------------------------------------------------------
CATEGORY <- "Алкоголь"

Магазин <- read_excel("data/Magazin.xlsx")   # правка: явное чтение файла

df <- Магазин %>%
  filter(Product_Category == CATEGORY) %>%
  mutate(
    ds = as.Date(Date),
    y  = as.numeric(Sales),
    Temperature = ifelse(
      is.na(suppressWarnings(as.numeric(Temperature))),
      suppressWarnings(as.numeric(str_extract(as.character(Temperature),
                                              "[-+]?\\d+\\.?\\d*"))),
      suppressWarnings(as.numeric(Temperature))
    )
  ) %>%
  select(ds, y, Temperature) %>%
  complete(ds = seq(min(ds), max(ds), by = "day")) %>%
  arrange(ds) %>%
  mutate(
    y           = zoo::na.approx(y,           na.rm = FALSE, rule = 1),
    Temperature = zoo::na.approx(Temperature, na.rm = FALSE, rule = 1)
  )

# 3. Train / Test ------------------------------------------------------
H          <- 30
split_date <- max(df$ds) - H
train <- df %>% filter(ds <= split_date)
test  <- df %>% filter(ds >  split_date)

use_weather <- "Temperature" %in% names(test) && all(!is.na(test$Temperature))

# 3а. Разведочный анализ (добавлено) -----------------------------------
# Длина ряда, календарные границы, статистика, недельный профиль.
cat("Ряд:", CATEGORY, "| дней:", nrow(df),
    "| с", format(min(df$ds)), "по", format(max(df$ds)), "\n")
cat("Train:", nrow(train), "дней (по", format(split_date), ") |",
    "Test:", nrow(test), "дней (с", format(min(test$ds)), ")\n")
cat("Средние продажи:", round(mean(df$y), 1),
    "| SD:", round(sd(df$y), 1),
    "| мин:", min(df$y), "| макс:", max(df$y), "\n")

p_eda <- ggplot(df, aes(ds, y)) +
  geom_line(color = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = split_date + 0.5,
             linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("rect", xmin = split_date + 1, xmax = max(df$ds),
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "red") +
  annotate("text", x = split_date - 120, y = max(df$y) * 0.97,
           label = "train", hjust = 0.5, color = "grey30", size = 4.5) +
  annotate("text", x = max(df$ds) - 15, y = max(df$y) * 0.97,
           label = "test (30 дн.)", hjust = 1, color = "red", size = 4.5) +
  labs(title = paste("Дневные продажи: категория «", CATEGORY, "»", sep = ""),
       subtitle = paste0(format(min(df$ds)), " — ", format(max(df$ds)),
                         ", вертикальная линия — граница train/test"),
       x = "Дата", y = "Продажи, шт.") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "task2_eda_series.png"), p_eda,
       width = 10, height = 5, dpi = 200)

# Недельный профиль (день недели восстановлен из даты).
# format(ds, "%u") даёт 1=понедельник ... 7=воскресенье независимо от локали
# (weekdays() в контейнере с локалью C вернул бы английские имена).
dow_levels <- c("Понедельник", "Вторник", "Среда", "Четверг",
                "Пятница", "Суббота", "Воскресенье")
df$dow <- factor(dow_levels[as.integer(format(df$ds, "%u"))],
                 levels = dow_levels)
weekly_profile <- df %>%
  group_by(dow) %>%
  summarise(mean_sales = mean(y), .groups = "drop")

p_weekly <- ggplot(weekly_profile, aes(dow, mean_sales)) +
  geom_col(fill = "steelblue") +
  labs(title = "Средние продажи по дням недели",
       subtitle = paste("Категория «", CATEGORY, "» (профиль построен по датам)", sep = ""),
       x = "День недели", y = "Средние продажи, шт.") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out_dir, "task2_weekly_profile.png"), p_weekly,
       width = 8, height = 5, dpi = 200)

cat("Недельный профиль (средние):",
    paste(sprintf("%s=%.0f", weekly_profile$dow, weekly_profile$mean_sales),
          collapse = ", "), "\n")

# 4. Кастомный предновогодний праздник --------------------------------
years <- unique(format(df$ds, "%Y"))
pre_new_year <- data.frame(
  holiday      = "pre_new_year",
  ds           = as.Date(paste0(years, "-12-28")),
  lower_window = 0,
  upper_window = 3
)

# 5. Функция для Prophet (преподавателя, дословно) ----------------------
fit_prophet_model <- function(train_data, future_data,
                              use_regressor = FALSE,
                              holidays_prior = 7,
                              label = "") {

  m <- prophet(
    yearly.seasonality       = TRUE,
    weekly.seasonality       = 10,
    daily.seasonality        = FALSE,
    holidays                 = pre_new_year,
    holidays.prior.scale     = holidays_prior,
    seasonality.prior.scale  = 0.5,
    changepoint.prior.scale  = 0.05,
    n.changepoints           = 40
  )

  m <- add_country_holidays(m, country_name = "RU")

  if (use_regressor) m <- add_regressor(m, "Temperature")

  train_fit <- if (use_regressor) train_data %>% select(ds, y, Temperature)
  else               train_data %>% select(ds, y)
  m <- fit.prophet(m, train_fit)

  fut <- if (use_regressor) future_data %>% select(ds, Temperature)
  else               future_data %>% select(ds)
  fc <- predict(m, fut)

  list(
    model = m,
    pred  = fc %>% select(ds, predicted = yhat) %>%
      left_join(future_data %>% select(ds, y), by = "ds") %>%
      mutate(model = label)
  )
}

# 5а. Параметризованная функция для экспериментов (добавлено) -----------
fit_prophet_tuned <- function(train_data, future_data,
                              use_regressor = FALSE,
                              weekly_seas   = 10,
                              n_chg         = 40,
                              seas_prior    = 0.5,
                              chg_prior     = 0.05,
                              hol_prior     = 10,
                              use_pre_ny    = TRUE,
                              label         = "") {

  m <- prophet(
    yearly.seasonality       = TRUE,
    weekly.seasonality       = weekly_seas,
    daily.seasonality        = FALSE,
    holidays                 = if (use_pre_ny) pre_new_year else NULL,
    holidays.prior.scale     = hol_prior,
    seasonality.prior.scale  = seas_prior,
    changepoint.prior.scale  = chg_prior,
    n.changepoints           = n_chg
  )

  m <- add_country_holidays(m, country_name = "RU")

  if (use_regressor) m <- add_regressor(m, "Temperature")

  train_fit <- if (use_regressor) train_data %>% select(ds, y, Temperature)
  else               train_data %>% select(ds, y)
  m <- fit.prophet(m, train_fit)

  fut <- if (use_regressor) future_data %>% select(ds, Temperature)
  else               future_data %>% select(ds)
  fc <- predict(m, fut)

  list(
    model = m,
    pred  = fc %>% select(ds, predicted = yhat) %>%
      left_join(future_data %>% select(ds, y), by = "ds") %>%
      mutate(model = label)
  )
}

metrics_row <- function(y, pred) {
  c(RMSE = rmse_vec(y, pred),
    MAE  = mae_vec(y, pred),
    MAPE = mape_vec(y, pred))   # yardstick mape_vec уже возвращает проценты
}

# 6а. Экспериментальная настройка Prophet (добавлено) -------------------
# База для этапа A — параметры базового кода преподавателя.
tune_default <- list(weekly_seas = 10, n_chg = 40,
                     seas_prior = 0.5, chg_prior = 0.05, hol_prior = 10,
                     use_pre_ny = TRUE)

run_stage <- function(stage, configs) {
  res <- lapply(configs, function(cfg) {
    fit <- fit_prophet_tuned(train, test, use_regressor = FALSE,
                             weekly_seas = cfg$weekly_seas,
                             n_chg       = cfg$n_chg,
                             seas_prior  = cfg$seas_prior,
                             chg_prior   = cfg$chg_prior,
                             hol_prior   = cfg$hol_prior,
                             use_pre_ny  = cfg$use_pre_ny)
    met <- metrics_row(test$y, fit$pred$predicted)
    data.frame(stage = stage,
               weekly_seasonality = cfg$weekly_seas,
               n_changepoints     = cfg$n_chg,
               seasonality_prior_scale = cfg$seas_prior,
               changepoint_prior_scale = cfg$chg_prior,
               holidays_prior_scale    = cfg$hol_prior,
               pre_new_year        = cfg$use_pre_ny,
               RMSE = met[["RMSE"]], MAE = met[["MAE"]], MAPE = met[["MAPE"]],
               stringsAsFactors = FALSE)
  })
  bind_rows(res)
}

# Этап A: порядок Фурье недельной сезонности (3-5 «ровные», 10-15 «пики»)
stageA <- run_stage("A: weekly.seasonality", lapply(
  c(3, 5, 10, 15), function(w) modifyList(tune_default, list(weekly_seas = w))))

# Этап B: число точек изменения тренда (15-25 стабильные, 40 волатильные)
best_A <- stageA[which.min(stageA$RMSE), ]
stageB <- run_stage("B: n.changepoints", lapply(
  c(15, 25, 40), function(n) modifyList(tune_default,
    list(weekly_seas = best_A$weekly_seasonality, n_chg = n))))

# Этап C: prior scale'ы (полный перебор 2 x 2 x 3)
best_B <- stageB[which.min(stageB$RMSE), ]
grid_C <- expand.grid(seas_prior = c(0.5, 1),
                      chg_prior  = c(0.01, 0.05),
                      hol_prior  = c(1, 5, 10))
stageC <- run_stage("C: prior scales", lapply(
  seq_len(nrow(grid_C)), function(i) modifyList(tune_default,
    list(weekly_seas = best_B$weekly_seasonality,
         n_chg       = best_B$n_changepoints,
         seas_prior  = grid_C$seas_prior[i],
         chg_prior   = grid_C$chg_prior[i],
         hol_prior   = grid_C$hol_prior[i]))))

# Абляция: с праздником pre_new_year и без него
best_C <- stageC[which.min(stageC$RMSE), ]
stageNY <- run_stage("NY: pre_new_year", list(
  modifyList(tune_default,
    list(weekly_seas = best_C$weekly_seasonality,
         n_chg       = best_C$n_changepoints,
         seas_prior  = best_C$seasonality_prior_scale,
         chg_prior   = best_C$changepoint_prior_scale,
         hol_prior   = best_C$holidays_prior_scale,
         use_pre_ny  = TRUE)),
  modifyList(tune_default,
    list(weekly_seas = best_C$weekly_seasonality,
         n_chg       = best_C$n_changepoints,
         seas_prior  = best_C$seasonality_prior_scale,
         chg_prior   = best_C$changepoint_prior_scale,
         hol_prior   = best_C$holidays_prior_scale,
         use_pre_ny  = FALSE))
))

prophet_tuning <- bind_rows(stageA, stageB, stageC, stageNY)
print(prophet_tuning, digits = 4)

best_tune <- stageNY[which.min(stageNY$RMSE), ]
best_params <- list(
  weekly_seasonality       = best_tune$weekly_seasonality,
  n_changepoints           = best_tune$n_changepoints,
  seasonality_prior_scale  = best_tune$seasonality_prior_scale,
  changepoint_prior_scale  = best_tune$changepoint_prior_scale,
  holidays_prior_scale     = best_tune$holidays_prior_scale,
  use_pre_ny               = best_tune$pre_new_year
)
cat("\nЛучшие параметры Prophet (по RMSE):\n")
str(best_params)

# 6. Запуск моделей Prophet (финальные, с лучшими параметрами) ----------
prophet_base <- fit_prophet_tuned(
  train, test, use_regressor = FALSE,
  weekly_seas = best_params$weekly_seasonality,
  n_chg       = best_params$n_changepoints,
  seas_prior  = best_params$seasonality_prior_scale,
  chg_prior   = best_params$changepoint_prior_scale,
  hol_prior   = best_params$holidays_prior_scale,
  use_pre_ny  = best_params$use_pre_ny,
  label       = "Prophet (base + pre-NY)")

prophet_weather <- fit_prophet_tuned(
  train, test, use_regressor = use_weather,
  weekly_seas = best_params$weekly_seasonality,
  n_chg       = best_params$n_changepoints,
  seas_prior  = best_params$seasonality_prior_scale,
  chg_prior   = best_params$changepoint_prior_scale,
  hol_prior   = best_params$holidays_prior_scale,
  use_pre_ny  = best_params$use_pre_ny,
  label       = if (use_weather) "Prophet (+ weather + pre-NY)"
  else            "Prophet (weather OFF)")

pred_models <- list(prophet_base$pred, prophet_weather$pred)

# Лучшая из двух Prophet-моделей (для компонент и диагностики)
prophet_rmse <- sapply(pred_models,
                       function(p) rmse_vec(p$y, p$predicted))
prophet_best_idx <- which.min(prophet_rmse)
m_prophet_best   <- list(prophet_base, prophet_weather)[[prophet_best_idx]]$model
prophet_best_label <- pred_models[[prophet_best_idx]]$model[1]
cat("Лучшая Prophet-модель:", prophet_best_label,
    "(RMSE =", round(min(prophet_rmse), 2), ")\n")

# 7. SARIMA и ETS --------------------------------------------------------
y_train_ts <- ts(train$y, frequency = 7)

sarima_fit <- auto.arima(y_train_ts, seasonal = TRUE, D = 1,
                         stepwise = TRUE, approximation = FALSE)

pred_sarima <- data.frame(
  ds        = test$ds,
  y         = test$y,
  predicted = as.numeric(forecast(sarima_fit, h = H)$mean),
  model     = "SARIMA"
)

# 7а. ETS: авто-подбор и ручные спецификации (добавлено) ---------------
ets_auto <- ets(y_train_ts)
ets_manual <- list(
  "AAA" = tryCatch(ets(y_train_ts, model = "AAA"), error = function(e) NULL),
  "MAM" = tryCatch(ets(y_train_ts, model = "MAM"), error = function(e) NULL),
  "MNM" = tryCatch(ets(y_train_ts, model = "MNM"), error = function(e) NULL)
)

ets_variants <- data.frame(
  spec  = c("auto", names(ets_manual)),
  RMSE  = NA, MAE = NA, MAPE = NA,
  model_string = NA, stringsAsFactors = FALSE
)
ets_fits <- list(auto = ets_auto)
for (i in seq_len(nrow(ets_variants))) {
  spec <- ets_variants$spec[i]
  fit <- if (spec == "auto") ets_auto else ets_manual[[spec]]
  ets_fits[[spec]] <- fit
  if (is.null(fit)) next
  fc <- as.numeric(forecast(fit, h = H)$mean)
  met <- metrics_row(test$y, fc)
  ets_variants$RMSE[i]  <- met[["RMSE"]]
  ets_variants$MAE[i]   <- met[["MAE"]]
  ets_variants$MAPE[i]  <- met[["MAPE"]]
  ets_variants$model_string[i] <- fit$method
}
print(ets_variants, digits = 4)

best_ets_spec <- ets_variants$spec[which.min(ets_variants$RMSE)]
ets_fit <- ets_fits[[best_ets_spec]]
ets_label <- paste0("ETS (", best_ets_spec, ": ",
                    gsub("ETS\\((.*)\\)", "\\1", ets_fit$method), ")")
cat("Лучшая ETS-спецификация:", ets_label, "\n")

pred_ets <- data.frame(
  ds        = test$ds,
  y         = test$y,
  predicted = as.numeric(forecast(ets_fit, h = H)$mean),
  model     = ets_label
)

# 8. Ансамбль (Prophet + ETS) -------------------------------------------
# В базовом коде усредняются прогнозы Prophet(+weather) и ETS; здесь —
# лучшая Prophet-модель и лучшая ETS (при допустимой погоде совпадает
# с базовым кодом).
ensemble_pred <- bind_rows(pred_models[[prophet_best_idx]], pred_ets) %>%
  group_by(ds) %>%
  summarise(
    y         = first(y),
    predicted = mean(predicted, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(model = "Ensemble (Prophet + ETS)")

# 9. Сводная таблица -----------------------------------------------------
all_preds <- bind_rows(pred_models, pred_sarima, pred_ets, ensemble_pred) %>%
  filter(!is.na(predicted))

comparison <- all_preds %>%
  group_by(model) %>%
  summarise(
    RMSE = rmse_vec(y, predicted),
    MAE  = mae_vec(y, predicted),
    MAPE = mape_vec(y, predicted),   # mape_vec уже в процентах
    .groups = "drop"
  ) %>%
  arrange(RMSE)

cat("\n=== Сравнение метрик ===\n")
print(comparison)

# 10. График сравнения ---------------------------------------------------
# правка: prophet возвращает ds как POSIXct, ETS/SARIMA — как Date;
# приводим к единому типу Date, иначе coord_cartesian(xlim = Date) падает
all_preds$ds <- as.Date(all_preds$ds)
model_colors <- list(
  "Факт"                       = "black",
  "Prophet (base + pre-NY)"    = "grey45",
  "Prophet (+ weather + pre-NY)" = "#FF7F00",
  "Prophet (weather OFF)"      = "#FF7F00",
  "SARIMA"                     = "#1F78B4",
  "Ensemble (Prophet + ETS)"   = "#9467BD"
)
# фактическая метка ETS может отличаться — доопределяем цвет
model_colors[[ets_label]] <- "#33A02C"

p_compare <- ggplot(all_preds, aes(x = ds)) +
  geom_line(aes(y = y, color = "Факт"), linewidth = 1.2) +
  geom_line(aes(y = predicted, color = model),
            linetype = "dashed", linewidth = 0.8) +
  labs(title    = "Сравнение моделей прогнозирования (Тест 30 дней)",
       subtitle = paste(CATEGORY, "с предновогодним ажиотажем"),
       x = "Дата", y = "Продажи", color = "") +
  scale_color_manual(values = unlist(model_colors)) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "task2_comparison.png"), p_compare,
       width = 11, height = 6, dpi = 200)

# Тот же график с zoom на период test + 60 дней до него (для отчёта)
zoom_start <- split_date - 60
p_compare_zoom <- p_compare +
  coord_cartesian(xlim = c(zoom_start, max(all_preds$ds) + 1)) +
  labs(title = "Сравнение моделей: тестовый период 30 дней (приближение)",
       subtitle = paste(CATEGORY, "— декабрь 2025 г. с предновогодним пиком"))
ggsave(file.path(out_dir, "task2_comparison_zoom.png"), p_compare_zoom,
       width = 11, height = 6, dpi = 200)

# 11. Компоненты Prophet -------------------------------------------------
# правка: если лучшая модель — с регрессором, для predict нужны и значения
# регрессора; используем историю ряда (компоненты оцениваются по ней) —
# будущих значений температуры для make_future_dataframe нет.
png(file.path(out_dir, "task2_prophet_components.png"),
    width = 2000, height = 1200, res = 170)
if ("Temperature" %in% names(m_prophet_best$extra_regressors)) {
  fut_comp <- df %>% select(ds, Temperature)
} else {
  fut_comp <- make_future_dataframe(m_prophet_best, periods = H)
}
prophet_plot_components(m_prophet_best, predict(m_prophet_best, fut_comp))
dev.off()

# 12. Встроенный график прогноза Prophet ---------------------------------
df_clean_local <- df %>% filter(ds >= as.Date("2020-01-01"))

# правка: параметры — подобранные (в базовом коде weekly.seasonality = 10)
m_forecast <- prophet(
  yearly.seasonality      = TRUE,
  weekly.seasonality      = best_params$weekly_seasonality,
  daily.seasonality       = FALSE,
  holidays                = if (best_params$use_pre_ny) pre_new_year else NULL,
  holidays.prior.scale    = best_params$holidays_prior_scale,
  seasonality.prior.scale = best_params$seasonality_prior_scale,
  changepoint.prior.scale = best_params$changepoint_prior_scale,

  n.changepoints          = best_params$n_changepoints
)
m_forecast <- add_country_holidays(m_forecast, country_name = "RU")
m_forecast <- fit.prophet(m_forecast, df_clean_local %>% select(ds, y))

future_dates <- seq.Date(from = max(df_clean_local$ds) + 1,
                         by = "day", length.out = 30)
future_df    <- data.frame(ds = future_dates)

future_full   <- bind_rows(df_clean_local %>% select(ds), future_df)
forecast_full <- predict(m_forecast, future_full)

p_builtin <- plot(m_forecast, forecast_full) +
  labs(title    = "Прогноз Prophet на 30 дней вперед",
       subtitle = paste(CATEGORY, "(встроенный график Prophet)"),
       x = "Дата", y = "Продажи") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))   # <-- исправлена скобка
ggsave(file.path(out_dir, "task2_prophet_builtin.png"), p_builtin,
       width = 10, height = 5, dpi = 200)

# 13. Диагностика остатков для лучшей модели -----------------------------
# Критерий отбора — RMSE (для запасов важна абсолютная ошибка, а тестовый
# период содержит предновогодний пик, который RMSE чувствует лучше MAPE).
best_model <- comparison %>% arrange(RMSE) %>% slice(1) %>% pull(model)
cat("\n=== Лучшая модель по RMSE:", best_model, "===\n")

resid_prophet <- function(m) {
  # правка: для модели с регрессором в new data нужна и сама колонка
  newdat <- train %>% select(ds)
  if ("Temperature" %in% names(m$extra_regressors)) {
    newdat <- train %>% select(ds, Temperature)
  }
  fit <- predict(m, newdat)
  ts(train$y - fit$yhat, frequency = 7)
}

residuals_all <- list()
lb_p <- c()

# остатки и Ljung-Box для всех финальных моделей (числа — в отчёт);
# fitdf = 0 — единый критерий «есть ли автокорреляция остатков вообще»
lb_for <- function(r, fitdf = 0) {
  Box.test(r, lag = 14, type = "Ljung-Box", fitdf = fitdf)$p.value
}
residuals_all[["Prophet (base + pre-NY)"]]     <- resid_prophet(prophet_base$model)
residuals_all[["Prophet (+ weather + pre-NY)"]] <- resid_prophet(prophet_weather$model)
residuals_all[[ets_label]]     <- residuals(ets_fit)
residuals_all[["SARIMA"]]      <- residuals(sarima_fit)
r_ens_p <- as.numeric(resid_prophet(
  list(prophet_base, prophet_weather)[[prophet_best_idx]]$model))
residuals_all[["Ensemble (Prophet + ETS)"]] <-
  ts((r_ens_p + as.numeric(residuals(ets_fit))) / 2, frequency = 7)

lb_p["Prophet (base + pre-NY)"]      <- lb_for(residuals_all[["Prophet (base + pre-NY)"]])
lb_p["Prophet (+ weather + pre-NY)"] <- lb_for(residuals_all[["Prophet (+ weather + pre-NY)"]])
lb_p[ets_label]        <- lb_for(residuals_all[[ets_label]])
lb_p["SARIMA"]         <- lb_for(residuals_all[["SARIMA"]])
lb_p["Ensemble (Prophet + ETS)"] <- lb_for(residuals_all[["Ensemble (Prophet + ETS)"]])
cat("Ljung-Box p-value (lag 14) по моделям:\n")
print(round(lb_p, 4))

png(file.path(out_dir, "task2_checkresiduals_best.png"),
    width = 2000, height = 1400, res = 170)
if (best_model == ets_label) {
  checkresiduals(ets_fit)
} else if (best_model == "SARIMA") {
  checkresiduals(sarima_fit)
} else if (grepl("weather", best_model)) {
  checkresiduals(residuals_all[[best_model]])
} else if (grepl("Prophet", best_model)) {
  checkresiduals(residuals_all[[best_model]])
} else if (grepl("Ensemble", best_model)) {
  checkresiduals(residuals_all[[best_model]])
}
dev.off()

# 14. Экспорт результатов (добавлено) ------------------------------------
write.csv(prophet_tuning, file.path(out_dir, "task2_prophet_tuning.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ets_variants, file.path(out_dir, "task2_ets_variants.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(comparison, file.path(out_dir, "task2_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(all_preds, file.path(out_dir, "task2_predictions.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(weekly_profile, file.path(out_dir, "task2_weekly_profile.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

task2_summary <- list(
  category      = CATEGORY,
  n_days        = nrow(df),
  date_min      = min(df$ds),
  date_max      = max(df$ds),
  split_date    = split_date,
  H             = H,
  use_weather   = use_weather,
  mean_sales    = mean(df$y),
  sd_sales      = sd(df$y),
  min_sales     = min(df$y),
  max_sales     = max(df$y),
  weekly_profile = weekly_profile,
  best_params   = best_params,
  prophet_tuning = prophet_tuning,
  ets_variants  = ets_variants,
  best_ets_spec = best_ets_spec,
  ets_label     = ets_label,
  sarima_string = sarima_fit$method,
  comparison    = comparison,
  best_model    = best_model,
  lb_p_values   = lb_p,
  prophet_best_label = prophet_best_label
)
saveRDS(task2_summary, file.path(out_dir, "task2_summary.rds"))
cat("Кэш отчёта: output/task2_summary.rds\n")
cat("Готово.\n")
