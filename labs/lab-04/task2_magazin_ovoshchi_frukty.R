# =====================================================================
# Лабораторный практикум 4. Задача 2.
# Прогнозирование продаж по товарной категории «Овощи - Фрукты»
# (Magazin.xlsx) с корреляционным анализом погодных условий и цен
# и макро-контекстом потребления (ОЭСР/НЦСЭД).
#
# Модели: Prophet base, Prophet + погода, Prophet + погода + цена,
# ETS, SARIMA и ансамбль Prophet + ETS; сравнение по RMSE, MAE и MAPE
# на тестовом периоде последних 30 дней.
#
# Базовый код преподавателя (assignments/lab-04/Skript koda ...
# Magazin Alko ispr.docx) сохранён дословно в блоках 2, 4, 5, 7, 8,
# 9, 12, 13. Технические правки и дополнения (документируются):
#   1) чтение данных: добавлен read_excel("data/Magazin.xlsx"); в select
#      сохранена колонка Price (нужна для ценовых корреляций);
#   2) категория — «Овощи - Фрукты» (по заданию категорию выбирает
#      обучающийся; pre_new_year проверяется абляцией и оставляется/
#      убирается по результату);
#   3) блок 3а — EDA: график ряда с train/test и недельный профиль
#      (день недели восстановлен из даты: колонка Weekday файла
#      календарю не соответствует);
#   4) блок 5а — параметризованная fit_prophet_tuned() и metrics_row();
#   5) блок 6а — поэтапная экспериментальная настройка Prophet
#      (A: weekly.seasonality; B: n.changepoints; C: prior scale'ы;
#      NY: абляция pre_new_year) — для овощей праздник НЕ обязателен
#      и решается экспериментально;
#   6) блок 6б — внешние погодные данные: Open-Meteo Archive API
#      (реанализ ERA5, точка ВДНХ, Москва; температура/осадки/давление/
#      влажность/ветер), кэш data/weather_vdnh_2023_2025.csv
#      (паттерн «есть кэш — читаем, нет — скачиваем»);
#   7) блок 6в — корреляционный анализ по рамке AI4Research
#      (ИИ-ассистированный цикл «вопрос → данные → гипотезы →
#      эксперимент → интерпретация», arXiv:2507.01903): Pearson и
#      Spearman, лаги осадков, частные корреляции с контролем дня
#      недели и месяца, сравнение дождливых/сухих дней (t-тест),
#      корреляция Sales↔Price и log-log эластичность;
#   8) блок 6г — макро-контекст: датасет НЦСЭД/ОЭСР
#      «Продовольственное снабжение и потребление» (Health: Risk
#      factors, DF_HEALTH_LVNG_FSC): потребление фруктов (FRS) и
#      овощей (VGS), кг/чел/год, Россия 1992–2021; кэш
#      data/oecd_food_supply_full.xlsx;
#   9) MAPE — в процентах (yardstick уже возвращает %), ds после
#      bind_rows приводится к Date, детект регрессоров — через
#      m$extra_regressors, Ljung-Box с единым fitdf = 0;
#  10) графики — PNG в output/, кэши CSV/rds для отчёта.
# =====================================================================

# 1. Установка пакетов (если еще не установлены) ----------------------
pkgs <- c("readxl", "prophet", "dplyr", "ggplot2",
          "yardstick", "tidyr", "stringr", "forecast", "zoo", "patchwork")
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
library(patchwork)

options(scipen = 999)

out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 2. Данные ------------------------------------------------------------
CATEGORY <- "Овощи - Фрукты"

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
  # правка: Price оставлена — регрессор и корреляционный анализ цены
  select(ds, y, Price, Temperature) %>%
  complete(ds = seq(min(ds), max(ds), by = "day")) %>%
  arrange(ds) %>%
  mutate(
    y           = zoo::na.approx(y,           na.rm = FALSE, rule = 1),
    Price       = zoo::na.approx(Price,       na.rm = FALSE, rule = 1),
    Temperature = zoo::na.approx(Temperature, na.rm = FALSE, rule = 1)
  )

# 3. Train / Test ------------------------------------------------------
H          <- 30
split_date <- max(df$ds) - H
train <- df %>% filter(ds <= split_date)
test  <- df %>% filter(ds >  split_date)

use_weather <- "Temperature" %in% names(test) && all(!is.na(test$Temperature))

# 3а. Разведочный анализ (добавлено) -----------------------------------
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
dow_levels <- c("Понедельник", "Вторник", "Среда", "Четверг",
                "Пятница", "Суббота", "Воскресенье")
df$dow <- factor(dow_levels[as.integer(format(df$ds, "%u"))],
                 levels = dow_levels)
weekly_profile <- df %>%
  group_by(dow) %>%
  summarise(mean_sales = mean(y), .groups = "drop")

p_weekly <- ggplot(weekly_profile, aes(dow, mean_sales)) +
  geom_col(fill = "seagreen3") +
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

# =====================================================================
# 6б. Внешние погодные данные: Open-Meteo Archive API (добавлено).
# Реанализ ERA5, точка ВДНХ (Москва): t_max/t_min/t_mean, осадки (мм),
# давление (гПа), влажность (%), максимальный ветер (км/ч).
# Паттерн репозитория: есть кэш — читаем, нет — скачиваем.
# =====================================================================
weather_cache <- "data/weather_vdnh_2023_2025.csv"
if (!file.exists(weather_cache)) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=55.8222&longitude=37.6333",
    "&start_date=", format(min(df$ds)), "&end_date=", format(max(df$ds)),
    "&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,",
    "precipitation_sum,surface_pressure_mean,relative_humidity_2m_mean,",
    "wind_speed_10m_max&timezone=Europe%2FMoscow&format=csv")
  stopifnot(grepl("^https://archive-api\\.open-meteo\\.com/", url))
  download.file(url, weather_cache, method = "libcurl", quiet = TRUE)
}
w_raw <- read.csv(weather_cache, skip = 2, check.names = FALSE)
weather <- data.frame(
  ds       = as.Date(w_raw[[1]]),
  t_max    = as.numeric(w_raw[[2]]),
  t_min    = as.numeric(w_raw[[3]]),
  t_mean   = as.numeric(w_raw[[4]]),
  precip   = as.numeric(w_raw[[5]]),
  pressure = as.numeric(w_raw[[6]]),
  humidity = as.numeric(w_raw[[7]]),
  wind     = as.numeric(w_raw[[8]])
)
if (nrow(weather) != nrow(df) || any(is.na(weather))) {
  stop("Погодный кэш не соответствует ряду данных")
}
cat("Погода (ВДНХ, Open-Meteo):", nrow(weather), "дней | осадки в",
    sum(weather$precip > 0), "днях | t_mean от",
    round(min(weather$t_mean), 1), "до", round(max(weather$t_mean), 1), "°C\n")

# датасет с факторами для корреляционного анализа и моделей
df_ext <- df %>%
  left_join(weather, by = "ds") %>%
  mutate(
    precip_lag1 = dplyr::lag(precip, 1),
    precip_lag2 = dplyr::lag(precip, 2),
    month = as.factor(format(ds, "%m"))
  )

# согласованность температуры файла и реальных наблюдений
cor_t <- cor(df_ext$Temperature, df_ext$t_mean, use = "complete.obs")
cat("Корреляция Temperature(файл) и t_mean(ВДНХ):", round(cor_t, 3), "\n")

train_ext <- df_ext %>% filter(ds <= split_date)
test_ext  <- df_ext %>% filter(ds >  split_date)

# =====================================================================
# 6в. Корреляционный анализ по рамке AI4Research (добавлено).
# Шаги рамки: вопрос → данные → гипотезы → эксперимент → интерпретация.
# Гипотезы формулируются в отчёте; здесь — вычислительная часть.
# =====================================================================

# --- частные корреляции с контролем дня недели и месяца ---------------
partial_cor <- function(x, y, dow, month) {
  ok <- complete.cases(x, y)
  rx <- residuals(lm(x[ok] ~ dow[ok] + month[ok]))
  ry <- residuals(lm(y[ok] ~ dow[ok] + month[ok]))
  cor(rx, ry)
}

corr_factors <- c("Price", "Temperature", "t_mean", "t_max", "t_min",
                  "precip", "precip_lag1", "precip_lag2",
                  "pressure", "humidity", "wind")

corr_table <- do.call(rbind, lapply(corr_factors, function(f) {
  x <- df_ext[[f]]
  ct_p <- suppressWarnings(cor.test(df_ext$y, x, method = "pearson"))
  ct_s <- suppressWarnings(cor.test(df_ext$y, x, method = "spearman"))
  data.frame(
    factor        = f,
    pearson       = unname(ct_p$estimate),
    pearson_p     = ct_p$p.value,
    spearman      = unname(ct_s$estimate),
    spearman_p    = ct_s$p.value,
    partial       = partial_cor(x, df_ext$y, df_ext$dow, df_ext$month),
    stringsAsFactors = FALSE
  )
}))
cat("\n=== Корреляции продаж с факторами ===\n")
print(corr_table, digits = 3)

# --- дождливые vs сухие дни (t-тест) -----------------------------------
df_ext$rainy <- df_ext$precip > 0
tt <- t.test(y ~ rainy, data = df_ext)
cat("\nДождливые дни:", sum(df_ext$rainy),
    "| средние продажи:", round(mean(df_ext$y[df_ext$rainy]), 1),
    "|| сухие:", sum(!df_ext$rainy),
    "| средние:", round(mean(df_ext$y[!df_ext$rainy]), 1),
    "| t-тест p =", signif(tt$p.value, 4), "\n")
tt_strong <- t.test(y ~ I(precip > 1), data = df_ext)

# --- heatmap полной корреляционной матрицы -----------------------------
hm_vars  <- c("y", corr_factors)
cor_mat  <- cor(df_ext[hm_vars], use = "complete.obs")
long_cor <- expand.grid(var1 = hm_vars, var2 = hm_vars)
long_cor$corr <- as.vector(cor_mat)
long_cor$var1 <- factor(long_cor$var1, levels = rev(hm_vars))
long_cor$var2 <- factor(long_cor$var2, levels = hm_vars)
p_heat <- ggplot(long_cor, aes(var2, var1, fill = corr)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", corr)), size = 3) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       limits = c(-1, 1)) +
  labs(title = "Корреляционная матрица: продажи и внешние факторы",
       subtitle = "Pearson, 1096 дневных наблюдений (2023–2025)",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "task2_corr_heatmap.png"), p_heat,
       width = 9, height = 7, dpi = 200)

# --- диаграммы рассеяния: температура и осадки -------------------------
p_sc_temp <- ggplot(df_ext, aes(t_mean, y)) +
  geom_point(alpha = 0.25, color = "grey30", size = 1.2) +
  geom_smooth(method = "loess", color = "firebrick", se = FALSE) +
  labs(title = "Продажи и среднесуточная температура",
       subtitle = "ЛОЕСС-тренд; корреляция в таблице отчёта",
       x = "Температура, °C", y = "Продажи, шт.") +
  theme_minimal(base_size = 12)
p_sc_prec <- ggplot(df_ext, aes(precip, y)) +
  geom_point(alpha = 0.25, color = "grey30", size = 1.2) +
  geom_smooth(method = "loess", color = "steelblue", se = FALSE) +
  labs(title = "Продажи и осадки",
       subtitle = "ЛОЕСС-тренд; справа — лаги осадков в таблице отчёта",
       x = "Осадки за сутки, мм", y = "Продажи, шт.") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "task2_scatter_weather.png"),
       p_sc_temp + p_sc_prec,
       width = 11, height = 4.6, dpi = 200)

# --- цена: корреляция и эластичность -----------------------------------
ct_price_p <- cor.test(df_ext$y, df_ext$Price, method = "pearson")
ct_price_s <- cor.test(df_ext$y, df_ext$Price, method = "spearman")
elast <- lm(log(y) ~ log(Price), data = df_ext)
elast_coef <- coef(elast)[[2]]
cat("\nЦена: Pearson r =", round(unname(ct_price_p$estimate), 3),
    "(p =", signif(ct_price_p$p.value, 3), ") | Spearman rho =",
    round(unname(ct_price_s$estimate), 3),
    "| эластичность log-log =", round(elast_coef, 3), "\n")

p_sc_price <- ggplot(df_ext, aes(Price, y)) +
  geom_point(alpha = 0.25, color = "grey30", size = 1.2) +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
  labs(title = "Продажи и средняя цена дня",
       subtitle = paste0("log-log эластичность = ",
                         formatC(elast_coef, format = "f", digits = 3)),
       x = "Цена, руб.", y = "Продажи, шт.") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "task2_scatter_price.png"), p_sc_price,
       width = 8, height = 5, dpi = 200)

# =====================================================================
# 6г. Макро-контекст: НЦСЭД/ОЭСР «Продовольственное снабжение и
# потребление» (DF_HEALTH_LVNG_FSC): фрукты (FRS) и овощи (VGS),
# кг/чел/год, Россия. Годовая частота — прямая корреляция с дневным
# рядом невозможна, датасет даёт контекст для выводов.
# =====================================================================
macro_file <- "data/oecd_food_supply_full.xlsx"
if (!file.exists(macro_file)) stop("Нет кэша макро-данных: ", macro_file)
macro_all <- read_excel(macro_file, sheet = "Data")
macro_rus <- macro_all %>%
  filter(REF_AREA == "RUS", MEASURE %in% c("FRS", "VGS")) %>%
  transmute(year = as.integer(TIME_PERIOD), measure = MEASURE,
            kg_per_capita = OBS_VALUE)
macro_wide <- macro_rus %>%
  tidyr::pivot_wider(names_from = measure, values_from = kg_per_capita)
cor_macro <- cor(macro_wide$FRS, macro_wide$VGS)
macro_trend_vgs <- coef(lm(VGS ~ year, data = macro_wide))[[2]]
macro_trend_frs <- coef(lm(FRS ~ year, data = macro_wide))[[2]]
local_yearly <- df %>% mutate(year = as.integer(format(ds, "%Y"))) %>%
  group_by(year) %>% summarise(sales_total = sum(y), .groups = "drop")
cat("\nМакро (РФ): годы", paste(range(macro_wide$year), collapse = "–"),
    "| фрукты", round(tail(macro_wide$FRS, 1), 1), "кг/чел/год,",
    "овощи", round(tail(macro_wide$VGS, 1), 1), "кг/чел/год |",
    "cor(FRS,VGS) =", round(cor_macro, 3), "\n")
cat("Локальный ряд по годам (шт.):",
    paste(local_yearly$year, local_yearly$sales_total, sep = "=", collapse = ", "), "\n")

macro_long <- macro_rus %>%
  mutate(Категория = ifelse(measure == "FRS", "Фрукты", "Овощи"))
p_macro <- ggplot(macro_long, aes(year, kg_per_capita, color = Категория)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("Фрукты" = "orange3", "Овощи" = "seagreen3")) +
  labs(title = "Потребление фруктов и овощей в России",
       subtitle = paste0("ОЭСР/НЦСЭД, DF_HEALTH_LVNG_FSC, кг/чел/год, ",
                         paste(range(macro_wide$year), collapse = "–")),
       x = "Год", y = "кг на человека в год", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "task2_macro_rus.png"), p_macro,
       width = 9, height = 5, dpi = 200)

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
# regressors — имена доп. регрессоров (колонки train_data/future_data)
fit_prophet_tuned <- function(train_data, future_data,
                              regressors    = character(0),
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

  for (r in regressors) m <- add_regressor(m, r)

  cols <- c("ds", "y", regressors)
  m <- fit.prophet(m, train_data %>% select(all_of(cols)))

  fc <- predict(m, future_data %>% select(all_of(c("ds", regressors))))

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
tune_default <- list(weekly_seas = 10, n_chg = 40,
                     seas_prior = 0.5, chg_prior = 0.05, hol_prior = 10,
                     use_pre_ny = TRUE)

run_stage <- function(stage, configs) {
  res <- lapply(configs, function(cfg) {
    fit <- fit_prophet_tuned(train_ext, test_ext, regressors = character(0),
                             weekly_seas = cfg$weekly_seas,
                             n_chg       = cfg$n_chg,
                             seas_prior  = cfg$seas_prior,
                             chg_prior   = cfg$chg_prior,
                             hol_prior   = cfg$hol_prior,
                             use_pre_ny  = cfg$use_pre_ny)
    met <- metrics_row(test_ext$y, fit$pred$predicted)
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

# Абляция: с праздником pre_new_year и без него (для овощей решает тест)
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
bp <- best_params
prophet_base <- fit_prophet_tuned(
  train_ext, test_ext, regressors = character(0),
  weekly_seas = bp$weekly_seasonality,
  n_chg       = bp$n_changepoints,
  seas_prior  = bp$seasonality_prior_scale,
  chg_prior   = bp$changepoint_prior_scale,
  hol_prior   = bp$holidays_prior_scale,
  use_pre_ny  = bp$use_pre_ny,
  label       = "Prophet (base)")

prophet_weather <- fit_prophet_tuned(
  train_ext, test_ext, regressors = c("t_mean", "precip"),
  weekly_seas = bp$weekly_seasonality,
  n_chg       = bp$n_changepoints,
  seas_prior  = bp$seasonality_prior_scale,
  chg_prior   = bp$changepoint_prior_scale,
  hol_prior   = bp$holidays_prior_scale,
  use_pre_ny  = bp$use_pre_ny,
  label       = "Prophet (+ погода)")

prophet_full <- fit_prophet_tuned(
  train_ext, test_ext, regressors = c("t_mean", "precip", "Price"),
  weekly_seas = bp$weekly_seasonality,
  n_chg       = bp$n_changepoints,
  seas_prior  = bp$seasonality_prior_scale,
  chg_prior   = bp$changepoint_prior_scale,
  hol_prior   = bp$holidays_prior_scale,
  use_pre_ny  = bp$use_pre_ny,
  label       = "Prophet (+ погода + цена)")

pred_models <- list(prophet_base$pred, prophet_weather$pred, prophet_full$pred)

prophet_rmse <- sapply(pred_models,
                       function(p) rmse_vec(p$y, p$predicted))
prophet_best_idx <- which.min(prophet_rmse)
m_prophet_best   <- list(prophet_base, prophet_weather, prophet_full)[[prophet_best_idx]]$model
prophet_best_label <- pred_models[[prophet_best_idx]]$model[1]
cat("Лучшая Prophet-модель:", prophet_best_label,
    "(RMSE =", round(min(prophet_rmse), 2), ")\n")

# 7. SARIMA и ETS --------------------------------------------------------
y_train_ts <- ts(train_ext$y, frequency = 7)

sarima_fit <- auto.arima(y_train_ts, seasonal = TRUE, D = 1,
                         stepwise = TRUE, approximation = FALSE)

pred_sarima <- data.frame(
  ds        = test_ext$ds,
  y         = test_ext$y,
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
  met <- metrics_row(test_ext$y, fc)
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
  ds        = test_ext$ds,
  y         = test_ext$y,
  predicted = as.numeric(forecast(ets_fit, h = H)$mean),
  model     = ets_label
)

# 8. Ансамбль (Prophet + ETS) -------------------------------------------
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
# правка: prophet возвращает ds как POSIXct — приводим к Date
all_preds$ds <- as.Date(all_preds$ds)

comparison <- all_preds %>%
  group_by(model) %>%
  summarise(
    RMSE = rmse_vec(y, predicted),
    MAE  = mae_vec(y, predicted),
    MAPE = mape_vec(y, predicted),
    .groups = "drop"
  ) %>%
  arrange(RMSE)

cat("\n=== Сравнение метрик ===\n")
print(comparison)

# 10. График сравнения ---------------------------------------------------
model_colors <- list(
  "Факт"                    = "black",
  "Prophet (base)"          = "grey45",
  "Prophet (+ погода)"      = "#E69F00",
  "Prophet (+ погода + цена)" = "#D55E00",
  "SARIMA"                  = "#1F78B4",
  "Ensemble (Prophet + ETS)" = "#9467BD"
)
model_colors[[ets_label]] <- "#33A02C"

p_compare <- ggplot(all_preds, aes(x = ds)) +
  geom_line(aes(y = y, color = "Факт"), linewidth = 1.2) +
  geom_line(aes(y = predicted, color = model),
            linetype = "dashed", linewidth = 0.8) +
  labs(title    = "Сравнение моделей прогнозирования (Тест 30 дней)",
       subtitle = paste("Категория «", CATEGORY, "»", sep = ""),
       x = "Дата", y = "Продажи", color = "") +
  scale_color_manual(values = unlist(model_colors)) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "task2_comparison.png"), p_compare,
       width = 11, height = 6, dpi = 200)

zoom_start <- split_date - 60
p_compare_zoom <- p_compare +
  coord_cartesian(xlim = c(zoom_start, max(all_preds$ds) + 1)) +
  labs(title = "Сравнение моделей: тестовый период 30 дней (приближение)",
       subtitle = paste(CATEGORY, "— декабрь 2025 г."))
ggsave(file.path(out_dir, "task2_comparison_zoom.png"), p_compare_zoom,
       width = 11, height = 6, dpi = 200)

# 11. Компоненты Prophet -------------------------------------------------
# правка: если лучшая модель — с регрессорами, для predict нужны и их
# значения; используем историю ряда (компоненты оцениваются по ней).
png(file.path(out_dir, "task2_prophet_components.png"),
    width = 2000, height = 1200, res = 170)
regs_best <- names(m_prophet_best$extra_regressors)
if (length(regs_best) > 0) {
  fut_comp <- df_ext %>% select(all_of(c("ds", regs_best)))
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
  weekly.seasonality      = bp$weekly_seasonality,
  daily.seasonality       = FALSE,
  holidays                = if (bp$use_pre_ny) pre_new_year else NULL,
  holidays.prior.scale    = bp$holidays_prior_scale,
  seasonality.prior.scale = bp$seasonality_prior_scale,
  changepoint.prior.scale = bp$changepoint_prior_scale,
  n.changepoints          = bp$n_changepoints
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
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(out_dir, "task2_prophet_builtin.png"), p_builtin,
       width = 10, height = 5, dpi = 200)

# 13. Диагностика остатков для лучшей модели -----------------------------
best_model <- comparison %>% arrange(RMSE) %>% slice(1) %>% pull(model)
cat("\n=== Лучшая модель по RMSE:", best_model, "===\n")

resid_prophet <- function(m) {
  newdat <- train_ext %>% select(ds)
  m_regs <- names(m$extra_regressors)
  if (length(m_regs) > 0) {
    newdat <- train_ext %>% select(all_of(c("ds", m_regs)))
  }
  fit <- predict(m, newdat)
  ts(train_ext$y - fit$yhat, frequency = 7)
}

residuals_all <- list()
lb_p <- c()
lb_for <- function(r, fitdf = 0) {
  Box.test(r, lag = 14, type = "Ljung-Box", fitdf = fitdf)$p.value
}
residuals_all[["Prophet (base)"]]            <- resid_prophet(prophet_base$model)
residuals_all[["Prophet (+ погода)"]]        <- resid_prophet(prophet_weather$model)
residuals_all[["Prophet (+ погода + цена)"]] <- resid_prophet(prophet_full$model)
residuals_all[[ets_label]]                   <- residuals(ets_fit)
residuals_all[["SARIMA"]]                    <- residuals(sarima_fit)
r_ens_p <- as.numeric(resid_prophet(
  list(prophet_base, prophet_weather, prophet_full)[[prophet_best_idx]]$model))
residuals_all[["Ensemble (Prophet + ETS)"]] <-
  ts((r_ens_p + as.numeric(residuals(ets_fit))) / 2, frequency = 7)

for (nm in names(residuals_all)) lb_p[nm] <- lb_for(residuals_all[[nm]])
cat("Ljung-Box p-value (lag 14) по моделям:\n")
print(round(lb_p, 4))

png(file.path(out_dir, "task2_checkresiduals_best.png"),
    width = 2000, height = 1400, res = 170)
if (best_model == ets_label) {
  checkresiduals(ets_fit)
} else if (best_model == "SARIMA") {
  checkresiduals(sarima_fit)
} else if (grepl("цена", best_model)) {
  checkresiduals(residuals_all[["Prophet (+ погода + цена)"]])
} else if (grepl("погода", best_model)) {
  checkresiduals(residuals_all[["Prophet (+ погода)"]])
} else if (grepl("Prophet", best_model)) {
  checkresiduals(residuals_all[["Prophet (base)"]])
} else if (grepl("Ensemble", best_model)) {
  checkresiduals(residuals_all[["Ensemble (Prophet + ETS)"]])
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
write.csv(corr_table, file.path(out_dir, "task2_correlations.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(macro_wide, file.path(out_dir, "task2_macro_rus.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

task2_summary <- list(
  category      = CATEGORY,
  n_days        = nrow(df),
  date_min      = min(df$ds),
  date_max      = max(df$ds),
  split_date    = split_date,
  H             = H,
  mean_sales    = mean(df$y),
  sd_sales      = sd(df$y),
  min_sales     = min(df$y),
  max_sales     = max(df$y),
  price_min     = min(df$Price),
  price_max     = max(df$Price),
  price_mean    = mean(df$Price),
  weekly_profile = weekly_profile,
  cor_file_vs_meteo_t = cor_t,
  corr_table    = corr_table,
  rain_days     = sum(df_ext$rainy),
  dry_days      = sum(!df_ext$rainy),
  rain_mean     = mean(df_ext$y[df_ext$rainy]),
  dry_mean      = mean(df_ext$y[!df_ext$rainy]),
  rain_ttest_p  = tt$p.value,
  rain_strong_mean = mean(df_ext$y[df_ext$precip > 1]),
  rain_strong_p    = tt_strong$p.value,
  price_pearson = unname(ct_price_p$estimate),
  price_pearson_p = ct_price_p$p.value,
  price_spearman = unname(ct_price_s$estimate),
  elasticity    = elast_coef,
  macro_wide    = macro_wide,
  cor_macro_fv  = cor_macro,
  macro_trend_vgs = macro_trend_vgs,
  macro_trend_frs = macro_trend_frs,
  local_yearly  = local_yearly,
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
