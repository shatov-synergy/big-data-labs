# Аудит Задачи 2, Проход 4: ETS, SARIMA, сопоставимость train.
# Запуск: docker exec -w /project/labs/lab-04 lab04 Rscript audit/pass4_z2_ets_sarima.R
library(readxl); library(dplyr); library(forecast)

m_rmse <- function(y, yh) sqrt(mean((y - yh)^2))
m_mae  <- function(y, yh) mean(abs(y - yh))
m_mape <- function(y, yh) 100 * mean(abs((y - yh) / y))

# --- данные (как в работе; зерно проверено в проходе 2) ---
Магазин <- read_excel("data/Magazin.xlsx")
df <- Магазин %>%
  filter(Product_Category == "Овощи - Фрукты") %>%
  mutate(ds = as.Date(Date), y = as.numeric(Sales)) %>%
  select(ds, y, Price, Temperature)
H <- 30; split_date <- max(df$ds) - H
train <- df %>% filter(ds <= split_date)
test  <- df %>% filter(ds >  split_date)
y_ts <- ts(train$y, frequency = 7)

# --- A. Сопоставимость train для всех семейств ---
cat("=== A. train у семейств ===\n")
cat("train дней:", nrow(train), "| test дней:", nrow(test),
    "| пересечение:", length(intersect(train$ds, test$ds)), "\n")

# --- B. ETS: перезапуск 4 вариантов ---
cat("\n=== B. ETS: независимый перезапуск ===\n")
csv <- read.csv("output/task2_ets_variants.csv")
print(csv)
aud <- data.frame()
for (spec in c("auto", "AAA", "MAM", "MNM")) {
  fit <- if (spec == "auto") ets(y_ts) else
         tryCatch(ets(y_ts, model = spec), error = function(e) NULL)
  if (is.null(fit)) { cat(spec, ": не сошёлся\n"); next }
  yh <- as.numeric(forecast(fit, h = H)$mean)
  aud <- rbind(aud, data.frame(
    spec = spec, model_string = fit$method,
    RMSE = m_rmse(test$y, yh), MAE = m_mae(test$y, yh),
    MAPE = m_mape(test$y, yh)))
  cat(sprintf("%-4s %-14s RMSE=%.4f MAE=%.4f MAPE=%.4f\n",
              spec, fit$method, tail(aud$RMSE, 1), tail(aud$MAE, 1),
              tail(aud$MAPE, 1)))
}
cat("\nсверка с ets_variants.csv (max |Δ|):\n")
for (spec in aud$spec) {
  a <- aud[aud$spec == spec, ]; c <- csv[csv$spec == spec, ]
  cat(spec, ": dRMSE =", round(abs(a$RMSE - c$RMSE), 6),
      "| dMAPE =", round(abs(a$MAPE - c$MAPE), 6), "\n")
}
cat("минимум RMSE: у работы —", csv$spec[which.min(csv$RMSE)],
    "| у аудита —", aud$spec[which.min(aud$RMSE)], "\n")

# --- C. SARIMA ---
cat("\n=== C. SARIMA ===\n")
sar <- auto.arima(y_ts, seasonal = TRUE, D = 1,
                  stepwise = TRUE, approximation = FALSE)
cat("метод:", sar$method, "\n")
cat("заказы: arimaorder =", paste(arimaorder(sar), collapse = ","),
    "| frequency =", frequency(y_ts), "| D =", sar$arma[3 + 1 != 0], "\n")
# корректное извлечение сезонных заказов
cat("arma vector:", paste(sar$arma, collapse = ","), "\n")
yh_sar <- as.numeric(forecast(sar, h = H)$mean)
p <- read.csv("output/task2_predictions.csv")
csv_sar <- p[p$model == "SARIMA", ]
cat("даты совпадают:", identical(as.Date(csv_sar$ds), test$ds),
    "| max|Δ прогноза|:", max(abs(csv_sar$predicted - yh_sar)),
    "| RMSE аудита:", round(m_rmse(test$y, yh_sar), 4),
    "| RMSE csv:", round(m_rmse(csv_sar$y, csv_sar$predicted), 4), "\n")

# --- D. Постобработка/шкала ---
cat("\n=== D. шкала и постобработка ===\n")
cat("минимум predicted по моделям (отрицательные после обрезки?):\n")
print(aggregate(predicted ~ model, p, min))

cat("\nАУДИТ-4 ЗАВЕРШЁН\n")
