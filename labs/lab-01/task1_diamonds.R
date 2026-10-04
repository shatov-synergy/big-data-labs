# ============================================================
# Лабораторный практикум 1 — Задание 1
# Разведочный анализ. Базовая визуализация данных (diamonds)
# ============================================================

# Пакеты (в Docker-образе уже установлены; проверка на всякий случай)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")

library(ggplot2)
library(dplyr)

# загружаем набор данных diamonds
data(diamonds)

?diamonds

str(diamonds)
summary(diamonds)

# проверяем наличие пропусков в данных
colSums(is.na(diamonds))

# папка для графиков (скрипт запускается из labs/lab-01)
out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# а) Гистограмма распределения бриллиантов по цене -------------------
p1 <- ggplot(diamonds, aes(x = price)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Распределение цены бриллиантов",
       x = "Цена ($)", y = "Количество")
print(p1)
ggsave(file.path(out_dir, "task1_a_price_hist.png"), p1, width = 8, height = 5, dpi = 150)

# а) Гистограмма распределения бриллиантов по глубине ----------------
p2 <- ggplot(diamonds, aes(x = depth)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(title = "Распределение глубины бриллиантов",
       x = "Глубина (%)", y = "Количество")
print(p2)
ggsave(file.path(out_dir, "task1_a_depth_hist.png"), p2, width = 8, height = 5, dpi = 150)

# б) Столбчатая диаграмма количества бриллиантов по качеству огранки -
p3 <- ggplot(diamonds, aes(x = cut, fill = cut)) +
  geom_bar() +
  labs(title = "Количество бриллиантов по качеству огранки",
       x = "Качество огранки", y = "Количество") +
  theme(legend.position = "none")
print(p3)
ggsave(file.path(out_dir, "task1_b_cut_bar.png"), p3, width = 8, height = 5, dpi = 150)

# в) Столбчатая диаграмма с подкатегориями (огранка x чистота) -------
p4 <- ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge") +
  labs(title = "Количество бриллиантов по огранке и чистоте",
       x = "Качество огранки", y = "Количество", fill = "Чистота")
print(p4)
ggsave(file.path(out_dir, "task1_b_cut_clarity_bar.png"), p4, width = 8, height = 5, dpi = 150)

# г) Диаграмма рассеяния: качество огранки и глубина (с джиттером) ---
p5 <- ggplot(diamonds, aes(x = cut, y = depth)) +
  geom_jitter(alpha = 0.2, color = "steelblue") +
  geom_boxplot(alpha = 0.5, fill = NA, color = "black", outlier.shape = NA) +
  labs(title = "Качество огранки и глубина",
       x = "Качество огранки", y = "Глубина (%)")
print(p5)
ggsave(file.path(out_dir, "task1_g_cut_depth_jitter.png"), p5, width = 8, height = 5, dpi = 150)

# г) Классическая диаграмма рассеяния: вес и цена ---------------------
p6 <- ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.2) +
  labs(title = "Зависимость цены от веса бриллианта",
       x = "Вес (карат)", y = "Цена ($)", color = "Огранка")
print(p6)
ggsave(file.path(out_dir, "task1_g_carat_price_scatter.png"), p6, width = 8, height = 5, dpi = 150)

# д) Ящики с усами: цена по качеству огранки -------------------------
p7 <- ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
  geom_boxplot() +
  labs(title = "Цена по качеству огранки",
       x = "Качество огранки", y = "Цена ($)") +
  theme(legend.position = "none")
print(p7)
ggsave(file.path(out_dir, "task1_d_price_cut_boxplot.png"), p7, width = 8, height = 5, dpi = 150)

# Краткие выводы -----------------------------------------------------
cat("\n=== Краткие выводы по заданию 1 ===\n")
cat("1. Распределение цены правоасимметрично: большинство бриллиантов дешевле 5000 $,\n")
cat("   медиана (2401 $) заметно меньше среднего (3933 $); экстремальные значения до 18823 $.\n")
cat("2. Распределение глубины близко к нормальному с узкой концентрацией около 61-63%,\n")
cat("   что соответствует стандартной «идеальной» глубине огранки.\n")
cat("3. Больше всего бриллиантов с огранкой Ideal (21551 из 53940 = 40%);\n")
cat("   качество огранки растёт с ростом числа камней — производство ориентировано на премиум.\n")
cat("4. Разные уровни чистоты встречаются внутри каждой огранки; наибольшая чистота SI1, VS2.\n")
cat("5. Цена экспоненциально растёт с весом (каратом); при равном весе цена сильно варьируется —\n")
cat("   вес объясняет большую часть, но не всю вариацию цены.\n")
cat("6. По ящикам с усами: медианная цена практически не растёт с качеством огранки,\n")
cat("   выбросы в дорогом сегменте присутствуют во всех категориях — огранка не главный фактор цены.\n")
