# Аудит Задачи 2, Проход 2: данные, EDA-факты, train/test, утечки.
# Проверочный скрипт: читает оригиналы (data/Magazin.xlsx, погода),
# НЕ изменяет их; выводит контрольные значения в консоль.
# Запуск: docker exec -w /project/labs/lab-04 lab04
#         Rscript audit/pass2_z2_data.R

library(readxl)
library(dplyr)
library(tidyr)

cat("=== A. Магазин.xlsx: структура ===\n")
d <- read_excel("data/Magazin.xlsx")
cat("dim:", paste(dim(d), collapse = " x "), "\n")
cat("cols:", paste(names(d), collapse = ", "), "\n")
cat("типы:", paste(sapply(d, class), collapse = ", "), "\n")

cat("\n--- зерно: пара (Date, Product_Category) уникальна? ---\n")
dup_keys <- sum(duplicated(d[, c("Date", "Product_Category")]))
cat("дубликатов пары:", dup_keys, "\n")
rows_per_date <- d %>% count(Date)
cat("строк на дату: min", min(rows_per_date$n), "max", max(rows_per_date$n), "\n")

cat("\n--- температура общая на дату? ---\n")
t_per_date <- d %>% group_by(Date) %>% summarise(n_t = n_distinct(Temperature), .groups = "drop")
cat("дат с >1 уникальной температурой:", sum(t_per_date$n_t > 1), "\n")

cat("\n=== B. Категория «Овощи - Фрукты» ===\n")
v <- d %>% filter(Product_Category == "Овощи - Фрукты")
cat("строк:", nrow(v), "| уникальных дат:", n_distinct(v$Date), "\n")
cat("диапазон:", format(min(v$Date)), "—", format(max(v$Date)), "\n")
all_days <- seq(min(v$Date), max(v$Date), by = "day")
cat("дней в календаре:", length(all_days),
    "| отсутствует дней:", sum(!all_days %in% v$Date), "\n")
cat("NA: Sales", sum(is.na(v$Sales)), "| Price", sum(is.na(v$Price)),
    "| Temperature", sum(is.na(v$Temperature)), "\n")
cat("нули Sales:", sum(v$Sales == 0), "| отрицательные:", sum(v$Sales < 0), "\n")
cat("Price: min", min(v$Price), "max", max(v$Price),
    "| нулей/отрицательных:", sum(v$Price <= 0), "\n")
cat("отсортирован по дате:", identical(v$Date, sort(v$Date)), "\n")

cat("\n--- выбросы Sales (1/99 перцентили, инфо) ---\n")
qs <- quantile(v$Sales, c(0, .01, .5, .99, 1))
print(round(qs, 1))

cat("\n=== C. Погода (кэш Open-Meteo) ===\n")
w <- read.csv("data/weather_vdnh_2023_2025.csv", skip = 2, check.names = FALSE)
cat("строк:", nrow(w), "| колонок:", ncol(w), "\n")
wds <- as.Date(w[[1]])
cat("диапазон:", format(min(wds)), "—", format(max(wds)), "\n")
cat("дубликатов дат:", sum(duplicated(wds)),
    "| отсутствует дней:", sum(!seq(min(wds), max(wds), by = "day") %in% wds), "\n")
na_w <- colSums(is.na(w[, -1]))
cat("NA по колонкам:", paste(na_w, collapse = ","), "\n")

cat("\n--- join факт x погода (как в скрипте работы) ---\n")
vw <- v %>% transmute(ds = as.Date(Date), y = as.numeric(Sales)) %>%
  left_join(data.frame(ds = wds), by = "ds")
cat("строк после join:", nrow(vw), "(ожидалось", nrow(v), "— размножение отсутствует, если равно)\n")

cat("\n=== D. Train/test ===\n")
ds <- sort(as.Date(v$Date))
H <- 30
split_date <- max(ds) - H
train <- ds[ds <= split_date]; test <- ds[ds > split_date]
cat("split_date:", format(split_date), "| train:", length(train),
    "дней (", format(min(train)), "—", format(max(train)), ")",
    "| test:", length(test), "дней (", format(min(test)), "—", format(max(test)), ")\n")
cat("пересечение train/test:", length(intersect(train, test)), "\n")
cat("test включает 28–31 декабря 2025:",
    all(as.Date(c("2025-12-28", "2025-12-29", "2025-12-30", "2025-12-31")) %in% test), "\n")

cat("\n--- pre_new_year в train (даты праздника, доступные обучению) ---\n")
ny <- as.Date(c("2023-12-28", "2024-12-28"))  # верх окно lower=0, upper=3 => 28-31.12
in_train <- sapply(ny, function(x) x %in% train)
cat("28.12.2023 в train:", in_train[1], "| 28.12.2024 в train:", in_train[2], "\n")

cat("\n--- na.approx/complete: изменяют ли ряд категории? ---\n")
# Повторяем логику блока 2 работы: complete() + na.approx
df_rep <- v %>%
  transmute(ds = as.Date(Date), y = as.numeric(Sales),
            Price = Price,
            Temperature = as.numeric(Temperature)) %>%
  complete(ds = seq(min(ds), max(ds), by = "day")) %>%
  arrange(ds)
cat("строк после complete():", nrow(df_rep),
    "| добавленных:", nrow(df_rep) - nrow(v), "\n")

cat("\n--- Weekday: используется ли в моделях (статич. grep скрипта) ---\n")
src <- readLines("task2_magazin_ovoshchi_frukty.R")
uses <- grep("Weekday", src, value = TRUE)
cat("упоминаний Weekday в скрипте:", length(uses), "\n")
if (length(uses)) print(head(uses, 3))

cat("\n=== E. predictions.csv: факт и даты по моделям ===\n")
p <- read.csv("output/task2_predictions.csv")
cat("моделей:", length(unique(p$model)), "| строк на модель:\n")
print(table(p$model))
cat("уникальных дат на модель:", paste(unique(tapply(p$ds, p$model,
      function(x) length(unique(x)))), collapse = ","), "\n")
y_by_date <- tapply(p$y, paste(p$ds, p$model), identity)
fact_uniq <- tapply(p$y, p$ds, function(x) length(unique(x)))
cat("дат с расхождением факта между моделями:", sum(fact_uniq != 1), "\n")
cat("диапазон дат прогнозов:", format(min(as.Date(p$ds))), "—",
    format(max(as.Date(p$ds))), "\n")

cat("\nАУДИТ-2 ЗАВЕРШЁН\n")
