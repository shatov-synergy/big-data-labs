# ============================================================
# Лабораторный практикум 1 — Задание 2
# Разведочный анализ данных на основе датасета mtcars
# ============================================================

# 1. Загрузка пакетов и данных
# (в Docker-образе пакеты уже установлены; ставятся только при отсутствии)

pkgs <- c("tidyverse", "plotly", "tidyr", "broom")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new) > 0) install.packages(new)

library(dplyr)
library(tidyr)
library(broom)
library(plotly)
library(htmlwidgets)

data(mtcars)

out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2. Основные статистики. Расчет моды вручную ------------------------

# Функция для вычисления выборочной моды
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Описательные статистики
desc_stats <- summary(mtcars)
print(desc_stats)

# Мода для каждой переменной
modes <- sapply(mtcars, get_mode)
cat("\n=== Мода для каждой переменной ===\n")
print(modes)

# Расширенная таблица статистик
extended_stats <- data.frame(
  Variable = names(mtcars),
  Mean     = sapply(mtcars, mean),
  Median   = sapply(mtcars, median),
  Mode     = modes,
  SD       = sapply(mtcars, sd),
  Min      = sapply(mtcars, min),
  Max      = sapply(mtcars, max)
)
print(extended_stats)

# 3. Плотность распределения mpg --------------------------------------

dens <- density(mtcars$mpg)

p_density <- plot_ly() %>%
  add_histogram(
    x = ~mtcars$mpg, name = "Гистограмма",
    histnorm = "probability density",
    marker = list(color = "rgba(100,150,220,0.55)",
                  line = list(color = "white", width = 1))
  ) %>%
  add_lines(
    x = dens$x, y = dens$y,
    name = "Ядерная оценка плотности",
    line = list(color = "#d62728", width = 3)
  ) %>%
  add_lines(
    x = seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 200),
    y = dnorm(seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 200),
              mean = mean(mtcars$mpg), sd = sd(mtcars$mpg)),
    name = "Нормальное распределение",
    line = list(color = "green", width = 2, dash = "dash")
  ) %>%
  layout(
    title = "Плотность распределения mpg",
    xaxis = list(title = "mpg (миль на галлон)"),
    yaxis = list(title = "Плотность вероятности"),
    hovermode = "x unified"
  )
p_density
saveWidget(as_widget(p_density), file.path(out_dir, "task2_density_mpg.html"),
           selfcontained = TRUE)

# 4. QQ-PLOT И тест ШАПИРО-УИЛКА --------------------------------------

qq_data <- as.data.frame(qqnorm(mtcars$mpg, plot.it = FALSE))

p_qq <- plot_ly(qq_data, x = ~x, y = ~y,
                type = "scatter", mode = "markers",
                marker = list(color = "#1f77b4", size = 9,
                              line = list(color = "white", width = 1)),
                name = "Наблюдения") %>%
  add_lines(x = ~x,
            y = coef(lm(y ~ x, data = qq_data))[1] +
                coef(lm(y ~ x, data = qq_data))[2] * qq_data$x,
            name = "Опорная линия",
            line = list(color = "red", width = 2)) %>%
  layout(
    title = "QQ-plot для mpg",
    xaxis = list(title = "Теоретические квантили (нормальное)"),
    yaxis = list(title = "Выборочные квантили (mpg)")
  )
p_qq
saveWidget(as_widget(p_qq), file.path(out_dir, "task2_qq_mpg.html"),
           selfcontained = TRUE)

# Тест Шапиро-Уилка
shapiro_res <- shapiro.test(mtcars$mpg)
print(shapiro_res)

# 5. BOXPLOT И выбросы (метод IQR) ------------------------------------

p_box <- plot_ly(mtcars, y = ~mpg, type = "box",
                 boxpoints = "all", jitter = 0.4,
                 pointpos = 0,
                 marker = list(color = "#2ca02c", size = 8),
                 line = list(color = "#d62728"),
                 fillcolor = "rgba(255,180,180,0.4)",
                 name = "mpg") %>%
  layout(
    title = "Boxplot для mpg",
    yaxis = list(title = "mpg")
  )
p_box
saveWidget(as_widget(p_box), file.path(out_dir, "task2_boxplot_mpg.html"),
           selfcontained = TRUE)

# Определение выбросов через IQR
Q1 <- quantile(mtcars$mpg, 0.25)
Q3 <- quantile(mtcars$mpg, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

cat(sprintf("\nQ1 = %.2f, Q3 = %.2f, IQR = %.2f\n", Q1, Q3, IQR_val))
cat(sprintf("Границы нормы: [%.2f ; %.2f]\n", lower_bound, upper_bound))

outliers <- mtcars$mpg[mtcars$mpg < lower_bound | mtcars$mpg > upper_bound]
cat("Выбросы (mpg): ", paste(outliers, collapse = ", "), "\n")
cat("Автомобили-выбросы:\n")
print(mtcars[mtcars$mpg < lower_bound | mtcars$mpg > upper_bound, c("mpg","cyl","hp")])

# 6. Группировка ПО cyl с помощью dplyr ------------------------------

group_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    n          = n(),
    mean_mpg   = mean(mpg),
    median_mpg = median(mpg),
    sd_mpg     = sd(mpg),
    .groups    = "drop"
  )
print(group_stats)

# Визуализация групповых статистик
p_group <- plot_ly(group_stats, x = ~factor(cyl),
                   y = ~mean_mpg, type = "bar",
                   name = "Среднее",
                   error_y = list(type = "data", array = ~sd_mpg,
                                  color = "#333"),
                   marker = list(color = c("#1f77b4","#ff7f0e","#2ca02c"))) %>%
  layout(
    title = "Средний mpg по группам цилиндров (± SD)",
    xaxis = list(title = "Число цилиндров"),
    yaxis = list(title = "Средний mpg")
  )
p_group
saveWidget(as_widget(p_group), file.path(out_dir, "task2_group_mpg.html"),
           selfcontained = TRUE)

# 7. t-ТЕСТ (4 vs 6 цилиндров) ----------------------------------------

t_res <- t.test(mpg ~ cyl,
                data = mtcars %>% filter(cyl %in% c(4, 6)))
print(t_res)

# 8. ANOVA + POST-HOC (Тьюки, Tukey HSD) ------------------------------

mtcars$cyl_f <- factor(mtcars$cyl)
anova_res <- aov(mpg ~ cyl_f, data = mtcars)
print(summary(anova_res))

tukey_res <- TukeyHSD(anova_res)
print(tukey_res)

# Визуализация Tukey HSD
tukey_df <- as.data.frame(tukey_res$cyl_f)
tukey_df$comparison <- rownames(tukey_df)

p_tukey <- plot_ly(tukey_df, x = ~comparison, y = ~diff,
                   type = "bar",
                   error_y = list(type = "data",
                                  array = ~(`upr`-`lwr`)/2),
                   marker = list(color = ifelse(
                     tukey_df$`p adj` < 0.05, "#d62728", "#7f7f7f"))) %>%
  layout(
    title = "Tukey HSD: попарные различия средних mpg",
    xaxis = list(title = "Сравнение групп"),
    yaxis = list(title = "Разница средних")
  )
p_tukey
saveWidget(as_widget(p_tukey), file.path(out_dir, "task2_tukey.html"),
           selfcontained = TRUE)
