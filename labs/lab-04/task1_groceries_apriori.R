# =====================================================================
# Лабораторный практикум 4. Задача 1.
# Анализ потребительской корзины (Groceries) — ассоциативные правила.
#
# Код преподавателя (assignments/lab-04/Skript koda ... Zadacha 1_Groceries
# Apriori.docx) сохранён максимально дословно. Технические правки
# (задокументированы по требованию стандарта репозитория):
#   1) графики base R / arulesViz сохраняются в PNG в output/
#      (png()/dev.off() вокруг plot()-вызовов);
#   2) папка экспорта Groceries_results создана внутри output/
#      (output/Groceries_results), чтобы не засорять корень;
#   3) в cat()-вызовах восстановлены переводы строк "\n"
#      (в docx-исходнике часть "\n" потеряна и заменена пустой строкой);
#   4) добавлен блок 12 — сохранение кэша результатов (rds) для отчёта.
# =====================================================================

# 1. Библиотеки
if (!requireNamespace("arules", quietly = TRUE))    install.packages("arules")
if (!requireNamespace("arulesViz", quietly = TRUE)) install.packages("arulesViz")
library(arules)
library(arulesViz)
library(ggplot2)

# Каталог для графиков и результатов
out_dir <- "output"
groceries_dir <- file.path(out_dir, "Groceries_results")
if (!dir.exists(out_dir))       dir.create(out_dir)
if (!dir.exists(groceries_dir)) dir.create(groceries_dir)

# 2. Данные и первичный анализ
data("Groceries")
summary(Groceries)
cat("Транзакций:", length(Groceries),
    "| Уникальных товаров:", length(itemLabels(Groceries)), "\n")

png(file.path(out_dir, "task1_item_frequency.png"),
    width = 1600, height = 1000, res = 170)
itemFrequencyPlot(Groceries, topN = 20, type = "relative",
                  col = "steelblue",
                  main = "Топ-20 товаров по доле транзакций")
dev.off()

trans_length <- size(Groceries)
png(file.path(out_dir, "task1_basket_length_hist.png"),
    width = 1600, height = 1000, res = 170)
hist(trans_length, breaks = 30, col = "lightgreen",
     main = "Распределение длины транзакций",
     xlab = "Товаров в корзине", ylab = "Транзакций")
abline(v = mean(trans_length), col = "red", lwd = 2, lty = 2)
dev.off()
cat("Средняя длина корзины:", round(mean(trans_length), 2), "\n")

# 3. Построение правил (Apriori)
rules <- apriori(Groceries,
                 parameter = list(support = 0.001, confidence = 0.3,
                                  minlen = 2, maxlen = 4, target = "rules"))
cat("Найдено правил:", length(rules), "\n")

# 4. Удаление избыточных правил
redundant <- is.redundant(sort(rules, by = "lift", decreasing = TRUE),
                          measure = "confidence")
rules_pruned <- sort(rules, by = "lift", decreasing = TRUE)[!redundant]
cat("Избыточных:", sum(redundant), "| Осталось:", length(rules_pruned), "\n")

# 5. Бизнес-правила (мягкие пороги)
business_rules <- subset(rules_pruned,
                         support    > 0.001 &
                           confidence > 0.3   &
                           lift       > 1.2)
business_rules <- sort(business_rules, by = "lift", decreasing = TRUE)
cat("\nТОП-15 БИЗНЕС-ПРАВИЛ (по lift):\n")
inspect(head(business_rules, 15))

# 6. Сильные правила (support >= 0.5%, lift > 2)
strong_rules <- subset(rules_pruned, support > 0.005 & lift > 2)
strong_rules <- sort(strong_rules, by = "lift", decreasing = TRUE)
cat("\nСИЛЬНЫЕ ПРАВИЛА (support >= 0.5%, lift > 2):\n")
cat("Найдено:", length(strong_rules), "\n")
inspect(head(strong_rules, 10))

# 7. Правила для конкретного товара
milk_rhs <- sort(subset(rules_pruned, rhs %in% "whole milk"),
                 by = "confidence", decreasing = TRUE)
cat("\nЧТО ЧАЩЕ ВСЕГО ПОКУПАЮТ ВМЕСТЕ С МОЛОКОМ (топ-10):\n")
inspect(head(milk_rhs, 10))

# 8. Визуализация
set.seed(42)
png(file.path(out_dir, "task1_rules_scatter.png"),
    width = 1600, height = 1100, res = 170)
plot(business_rules, method = "scatter",
     measure = c("support", "confidence"), shading = "lift",
     main = "Бизнес-правила: поддержка vs достоверность (цвет = lift)")
dev.off()

if (length(strong_rules) >= 5) {
  set.seed(42)
  png(file.path(out_dir, "task1_rules_graph.png"),
      width = 1800, height = 1400, res = 170)
  plot(head(strong_rules, 15), method = "graph", engine = "igraph",
       main = "Граф топ-15 сильных правил (support >= 0.5%)")
  dev.off()
}

# 9. Система рекомендаций (прямой + обратный вывод)
recommend_for_basket <- function(basket, rules, top_n = 10,
                                 exclude_in_basket = TRUE) {
  basket <- intersect(basket, itemLabels(Groceries))
  if (length(basket) == 0) return(cat("Товары не найдены в каталоге."))

  lhs_mat <- as(lhs(rules), "matrix")
  rhs_mat <- as(rhs(rules), "matrix")

  cols_lhs <- which(colnames(lhs_mat) %in% basket)
  cols_rhs <- which(colnames(rhs_mat) %in% basket)

  direct <- rep(FALSE, length(rules))
  if (length(cols_lhs) > 0) {
    n_lhs    <- rowSums(lhs_mat)
    n_in_lhs <- rowSums(lhs_mat[, cols_lhs, drop = FALSE])
    direct   <- n_lhs > 0 & n_lhs == n_in_lhs
  }

  reverse <- rep(FALSE, length(rules))
  if (length(cols_rhs) > 0) {
    n_rhs    <- rowSums(rhs_mat)
    n_in_rhs <- rowSums(rhs_mat[, cols_rhs, drop = FALSE])
    reverse  <- n_rhs > 0 & n_rhs == n_in_rhs
  }

  if (sum(direct) == 0 && sum(reverse) == 0) {
    return(cat("Правил для корзины не найдено."))
  }

  idx <- which(direct | reverse)
  idx_sorted <- idx[order(quality(rules[idx])$confidence, decreasing = TRUE)]

  items <- character(0)
  for (j in seq_along(idx_sorted)) {
    i <- idx_sorted[j]
    side <- if (direct[i]) labels(rhs(rules[i])) else labels(lhs(rules[i]))
    items <- c(items, trimws(strsplit(gsub("[{}]", "", side), ",")[[1]]))
    if (length(unique(items)) >= top_n * 2) break
  }
  items <- unique(items)

  if (exclude_in_basket) items <- setdiff(items, basket)
  if (length(items) == 0) return(cat("Все рекомендации уже в корзине."))

  cat("Корзина:", paste(basket, collapse = ", "), "\n")
  cat("Рекомендации:", paste(head(items, top_n), collapse = ", "), "\n")
  invisible(head(items, top_n))
}

# 10. Автоматические бизнес-рекомендации
cat("БИЗНЕС-РЕКОМЕНДАЦИИ НА ОСНОВЕ АССОЦИАТИВНЫХ ПРАВИЛ\n")

# 10.1. Товары-магниты
freq <- itemFrequency(Groceries, type = "relative")
top_anchors <- sort(freq, decreasing = TRUE)[1:5]

cat("1. ТОВАРЫ-МАГНИТЫ (высокая частота, тянут трафик):\n")
for (i in seq_along(top_anchors)) {
  cat(sprintf("   %d. %-20s — в %4.1f%% корзин\n",
              i, names(top_anchors)[i], top_anchors[i] * 100))
}
cat("   Рекомендация: держать низкую цену, ставить в глубине зала.\n")

# 10.2. Сильные парные связи
top5 <- head(sort(strong_rules, by = "lift", decreasing = TRUE), 5)
cat("2. СИЛЬНЫЕ ПАРНЫЕ СВЯЗИ (support >= 0.5%, lift > 2):\n")
for (i in seq_len(length(top5))) {
  lhs_i <- gsub(",", ", ", paste(labels(lhs(top5[i])), collapse = ", "))
  rhs_i <- gsub(",", ", ", paste(labels(rhs(top5[i])), collapse = ", "))
  q     <- quality(top5[i])
  cat(sprintf("   %d. %s → %s  (lift = %.1f, conf = %.0f%%, supp = %.2f%%)\n",
              i, lhs_i, rhs_i,
              q$lift, q$confidence * 100, q$support * 100))
}
cat("   Рекомендация: кросс-выкладка рядом, bundle-акции.\n")

# 10.3. Готовые наборы по категориям
cat("3. ГОТОВЫЕ НАБОРЫ ПО КАТЕГОРИЯМ (support >= 0.5%, >=2 товара категории):\n")
category_patterns <- list(
  "Завтрак"      = c("bread", "butter", "milk", "cheese", "egg"),
  "Выпечка"      = c("flour", "sugar", "baking powder", "curd", "margarine"),
  "Алкоголь"     = c("beer", "wine", "liquor"),
  "Снеки"        = c("soda", "snack", "popcorn"),
  "Быстрый ужин" = c("instant", "hamburger", "sausage", "rolls/buns"),
  "Фрукты-овощи" = c("fruit", "vegetables", "citrus", "tropical", "root")
)

rule_matches_category <- function(i, pat) {
  all_labels <- c(labels(lhs(strong_rules[i])),
                  labels(rhs(strong_rules[i])))
  sum(grepl(pat, all_labels)) >= 2
}

category_rule_counts <- integer(0)   # правка: кэш счётчиков для отчёта
for (cat_name in names(category_patterns)) {
  pat <- paste(category_patterns[[cat_name]], collapse = "|")
  keep <- vapply(seq_len(length(strong_rules)),
                 function(i) rule_matches_category(i, pat),
                 logical(1))
  cat_rules <- strong_rules[keep]
  category_rule_counts[cat_name] <- length(cat_rules)

  if (length(cat_rules) > 0) {
    top3 <- head(sort(cat_rules, by = "lift", decreasing = TRUE), 3)
    cat(sprintf("\n   [%s] — найдено %d правил, топ-3:\n",
                cat_name, length(cat_rules)))
    inspect(top3)
  } else {
    cat(sprintf("\n   [%s] — правил с support >= 0.5%% и >=2 товарами категории не найдено\n",
                cat_name))
  }
}

# 10.4. Обратный вывод для молока
cat("\n4. ОБРАТНЫЙ ВЫВОД (support >= 0.5%):\n")
target <- "whole milk"
rev_rules <- subset(rules_pruned, rhs %in% target & support > 0.005)
if (length(rev_rules) > 0) {
  top_rev <- head(sort(rev_rules, by = "confidence", decreasing = TRUE), 3)
  cat(sprintf("   Чтобы купить %s, чаще всего берут вместе:\n", target))
  for (i in seq_len(length(top_rev))) {
    lhs_i <- gsub(",", ", ", paste(labels(lhs(top_rev[i])), collapse = ", "))
    cat(sprintf("   - %s (conf = %.0f%%, supp = %.2f%%)\n",
                lhs_i,
                quality(top_rev[i])$confidence * 100,
                quality(top_rev[i])$support * 100))
  }
  cat("   (это правила вида {X, Y} → {молоко}: если взял X и Y, то с высокой")
  cat("    вероятностью возьмёт и молоко. Обратный вывод, а не «молоко тянет X».)\n")
  cat("   Рекомендация: молоко — товар-магнит, покупают вместе с фруктами,")
  cat("   овощами и маслом.\n")
} else {
  cat("   Правил с support >= 0.5% для молока не найдено.\n")
}

# 10.5. Примеры рекомендаций для корзин
cat("5. ПРИМЕРЫ РЕКОМЕНДАЦИЙ ДЛЯ КОРЗИН (strong_rules):\n")
cat("   (функция ищет правила, где товары корзины есть в lhs (прямой вывод")
cat("    или в rhs (обратный вывод). Если прямых нет — предлагает lhs.)\n")

example_baskets <- list(
  c("whole milk", "other vegetables"),
  c("white bread", "butter"),
  c("bottled beer", "red/blush wine")
)

example_recs <- list()   # правка: кэш рекомендаций для отчёта
for (b in example_baskets) {
  cat(sprintf("\n   Корзина: %s\n", paste(b, collapse = ", ")))
  res <- recommend_for_basket(b, strong_rules, top_n = 5,
                              exclude_in_basket = TRUE)
  example_recs[[paste(b, collapse = ", ")]] <-
    if (is.null(res)) character(0) else res
  if (is.null(res) || length(res) == 0) {
    cat("   (нет рекомендаций)\n")
  }
}

# 10.6. Итоговая сводка
cat("6. ИТОГОВАЯ СВОДКА:\n")
cat(sprintf("   Всего правил: %d\n", length(rules)))
cat(sprintf("   После удаления избыточных: %d\n", length(rules_pruned)))
cat(sprintf("   Бизнес-правил (lift > 1.2): %d\n", length(business_rules)))
cat(sprintf("   Сильных правил (support >= 0.5%%, lift > 2): %d\n",
            length(strong_rules)))
cat("   Ключевые выводы для отчёта:\n")
cat("   1. Товары-магниты: молоко, овощи, хлеб, газировка, йогурт.\n")
cat("   2. Рабочие категории: Фрукты-овощи (86 правил), Завтрак (19 правил),")
cat("      Быстрый ужин (3 правила).\n")
cat("   3. Сильные связи: цитрус+овощи+молоко → корнеплоды (lift 4.1); ")
cat("      масло+сливки → молоко (lift 2.6); булочки+пакеты → сосиски (lift 3.3).\n")
cat("   4. Выпечка, Алкоголь, Снеки — нет правил с support >= 0.5% и >=2 товарами")
cat("      категории. Нужен более широкий набор правил (support >= 0.1%).\n")

# 11. Экспорт результатов
output_dir <- groceries_dir     # правка: экспорт в output/Groceries_results

write.csv(as(business_rules, "data.frame"),
          file.path(output_dir, "business_rules.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(as(strong_rules, "data.frame"),
          file.path(output_dir, "strong_rules.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

top_lift_df <- as(head(sort(rules_pruned, by = "lift", decreasing = TRUE), 50),
                  "data.frame")
write.csv(top_lift_df,
          file.path(output_dir, "top50_rules_by_lift.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

freq_df <- data.frame(
  item  = names(itemFrequency(Groceries, type = "absolute")),
  count = as.numeric(itemFrequency(Groceries, type = "absolute"))
)
freq_df <- freq_df[order(-freq_df$count), ]
write.csv(freq_df, file.path(output_dir, "item_frequencies.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Результаты сохранены в папке:", output_dir, "\n")

# 12. Кэш результатов для отчёта (добавлено)
task1_summary <- list(
  n_transactions  = length(Groceries),
  n_items         = length(itemLabels(Groceries)),
  mean_basket     = mean(trans_length),
  n_rules         = length(rules),
  n_redundant     = sum(redundant),
  n_rules_pruned  = length(rules_pruned),
  n_business      = length(business_rules),
  n_strong        = length(strong_rules),
  top_anchors     = top_anchors,
  category_counts = category_rule_counts,
  example_baskets = example_recs
)
saveRDS(task1_summary, file.path(out_dir, "task1_summary.rds"))
cat("Кэш отчёта: output/task1_summary.rds\n")
