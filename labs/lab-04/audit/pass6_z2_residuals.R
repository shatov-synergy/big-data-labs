# Аудит Задачи 2, Проход 6: остатки, checkresiduals, Ljung-Box, ACF.
# Запуск: docker exec -w /project/labs/lab-04 lab04 Rscript audit/pass6_z2_residuals.R
library(readxl); library(prophet); library(dplyr); library(forecast)

# --- данные и модели (как в работе; блоки проверены проходами 2-4) ---
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
years <- unique(format(df_ext$ds, "%Y"))
pre_new_year <- data.frame(holiday = "pre_new_year",
                           ds = as.Date(paste0(years, "-12-28")),
                           lower_window = 0, upper_window = 3)
bp <- list(ws = 3, ncp = 40, sps = 0.5, cps = 0.05, hps = 5, ny = TRUE)

fit_p <- function(regressors = character(0)) {
  m <- prophet(yearly.seasonality = TRUE, weekly.seasonality = bp$ws,
               daily.seasonality = FALSE,
               holidays = if (bp$ny) pre_new_year else NULL,
               holidays.prior.scale = bp$hps,
               seasonality.prior.scale = bp$sps,
               changepoint.prior.scale = bp$cps,
               n.changepoints = bp$ncp)
  m <- add_country_holidays(m, country_name = "RU")
  for (r in regressors) m <- add_regressor(m, r)
  m <- fit.prophet(m, train_ext %>% select(all_of(c("ds", "y", regressors))))
  m
}
m_base <- fit_p(); m_w <- fit_p(c("t_mean", "precip"))
m_full <- fit_p(c("t_mean", "precip", "Price"))

resid_p <- function(m) {
  regs <- names(m$extra_regressors)
  nd <- if (length(regs)) train_ext %>% select(all_of(c("ds", regs)))
        else train_ext %>% select(ds)
  train_ext$y - predict(m, nd)$yhat
}
y_ts <- ts(train_ext$y, frequency = 7)
ets_fit <- ets(y_ts, model = "AAA")
sar <- auto.arima(y_ts, seasonal = TRUE, D = 1,
                  stepwise = TRUE, approximation = FALSE)

r_base <- resid_p(m_base); r_w <- resid_p(m_w); r_full <- resid_p(m_full)
r_ets  <- as.numeric(residuals(ets_fit))
r_sar  <- as.numeric(residuals(sar))
r_ens_formula <- (r_base + r_ets) / 2          # формула работы
fit_ens <- 0.5 * (train_ext$y - r_base) + 0.5 * (train_ext$y - r_ets)
r_ens_direct <- train_ext$y - fit_ens          # прямой пересчёт

cat("=== A. Происхождение остатков ===\n")
cat("длины: base", length(r_base), "| weather", length(r_w),
    "| full", length(r_full), "| ets", length(r_ets),
    "| sarima", length(r_sar), "\n")
cat("NA: base", sum(is.na(r_base)), "| ets", sum(is.na(r_ets)),
    "| sarima", sum(is.na(r_sar)), "\n")
cat("формула ансамбля: max|(0.5(rP+rE)) - (y - 0.5(yPfit+yEfit))| =",
    max(abs(r_ens_formula - r_ens_direct)), "\n")
cat("период: train", format(min(train_ext$ds)), "—", format(max(train_ext$ds)),
    "| частота объявлена 7\n")

cat("\n=== B. ACF lag 1-14 и значимые лаги ===\n")
sig <- function(r, n) {
  a <- acf(r, lag.max = 14, plot = FALSE)$acf[-1]
  thr <- qnorm(0.975) / sqrt(n)
  c(sum_abs7 = sum(abs(a[1:7])), n_sig = sum(abs(a) > thr),
    lag1 = a[1], lag7 = a[7])
}
n_tr <- length(r_base)
tab <- data.frame(
  model = c("Prophet (base)", "Prophet (+погода)", "Prophet (+погода+цена)",
            "ETS (AAA)", "SARIMA", "Ensemble"),
  t(sapply(list(r_base, r_w, r_full, r_ets, r_sar, r_ens_formula),
           function(r) sig(r[!is.na(r)], sum(!is.na(r))))))
print(tab, digits = 3, row.names = FALSE)
cat("порог значимости ACF: ±", round(qnorm(0.975) / sqrt(n_tr), 3), "\n")

cat("\n=== C. Ljung-Box lag 14, fitdf 0 — сверка с rds ===\n")
s <- readRDS("output/task2_summary.rds")
for (nm in names(s$lb_p_values)) {
  r <- switch(nm,
    "Prophet (base)" = r_base, "Prophet (+ погода)" = r_w,
    "Prophet (+ погода + цена)" = r_full,
    "ETS (AAA: A,Ad,A)" = r_ets, "SARIMA" = r_sar,
    "Ensemble (Prophet + ETS)" = r_ens_formula)
  p_aud <- Box.test(r[!is.na(r)], lag = 14, type = "Ljung-Box")$p.value
  cat(nm, ": p аудита =", signif(p_aud, 4), "| p работы =", signif(s$lb_p_values[[nm]], 4),
      "| Δ =", abs(p_aud - s$lb_p_values[[nm]]), "\n")
}

cat("\n=== D. checkresiduals для победителя (Ensemble): фактический вход ===\n")
r_ens_ts <- ts(r_ens_formula, frequency = 7)
out <- capture.output(checkresiduals(r_ens_ts, plot = FALSE))
cat(paste(out, collapse = "\n"), "\n")

cat("\nАУДИТ-6 ЗАВЕРШЁН\n")
