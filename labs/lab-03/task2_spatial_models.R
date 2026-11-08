# ============================================================================
# Лабораторный практикум 3. Тема 2. Методы очистки и предобработки данных
# Задача 1 (часть 2). Пространственные эконометрические модели цен на жильё:
# OLS -> тест Морана на остатки -> LM-тесты -> SAR (lagsarlm) и SEM (errorsarlm)
# -> сравнение AIC/BIC -> прямые и косвенные эффекты SAR.
#
# Опирается на результаты задачи 1 (output/task1_results.rds).
# Технические правки относительно методического скрипта:
#  - добавлен вызов impacts() для SAR-модели (в исходнике объект
#    sar_impacts_full использовался без создания);
#  - графики сохраняются в output/ через ggsave();
#  - все таблицы результатов сохраняются в output/ в формате csv.
# ============================================================================

library(spdep)
library(spatialreg)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

set.seed(42)

res1 <- readRDS("output/task1_results.rds")
rayon_reg_wgs <- res1$rayon_reg_wgs

# Проверяем данные регрессии
cat("Проверка данных для моделей\n")
cat("Количество районов:", nrow(rayon_reg_wgs), "\n")

# ---- ЭТАП 4: РЕГРЕССИЯ OLS С РАСШИРЕННЫМ НАБОРОМ ФАКТОРОВ -------------------
# Финальная спецификация (базовая)
ols_formula_full <- mean_log_price ~
  mean_dist_metro +      # Транспортная доступность
  mean_area +            # Средняя площадь
  mean_floors +          # Этажность дома
  share_first_floor +    # Доля первых этажей
  share_last_floor +     # Доля последних этажей
  share_studio +         # Доля студий
  share_secondary +      # Доля вторичного рынка
  share_brick +          # Кирпичные дома
  share_panel +          # Панельные дома
  share_designer +       # Дизайнерский ремонт
  share_euro +           # Евроремонт
  share_no_repair +      # Требует ремонта
  share_combined_bath +  # Совмещённый санузел
  share_high_rise +      # Повышенная этажность
  share_skyscraper +     # Высотная этажность
  n                      # Ликвидность рынка

# Проверяем, что все переменные есть
available_vars <- names(rayon_reg_wgs)
formula_vars <- all.vars(ols_formula_full)
missing_vars <- setdiff(formula_vars, available_vars)
if (length(missing_vars) > 0) {
  cat("Отсутствующие переменные:", missing_vars, "\n")
}

# Техническая правка: убираем строки с пропусками ДО оценивания,
# иначе step() падает с "number of rows in use has changed"
all_vars_full <- all.vars(ols_formula_full)
rayon_reg_wgs <- rayon_reg_wgs %>%
  filter(if_all(all_of(all_vars_full), ~ !is.na(.))) %>%
  filter(if_all(all_of(all_vars_full), ~ is.finite(.)))

# Оцениваем OLS
ols_full <- lm(ols_formula_full, data = rayon_reg_wgs)

cat("РАСШИРЕННАЯ OLS-РЕГРЕССИЯ\n")
print(summary(ols_full))

# ---- ЭТАП 5: ПОШАГОВЫЙ ОТБОР ПЕРЕМЕННЫХ -------------------------------------
# Пошаговый отбор на основе AIC
ols_step <- step(ols_full, direction = "both", trace = FALSE)

cat("РЕЗУЛЬТАТ ПОШАГОВОГО ОТБОРА\n")
print(summary(ols_step))
cat("AIC финальной модели:", AIC(ols_step), "\n")

# Финальная формула
final_formula <- formula(ols_step)
cat("Финальная формула:\n")
print(final_formula)

# ---- ЭТАП 6. ДИАГНОСТИКА OLS ------------------------------------------------
# Проверяем данные регрессии
cat("Наблюдений в OLS:", length(residuals(ols_step)), "\n")

# Оставляем только районы с полными данными модели
model_vars <- all.vars(formula(ols_step))

rayon_reg_wgs <- rayon_reg_wgs %>%
  filter(if_all(all_of(model_vars), ~ !is.na(.)))

# Удаляем возможные бесконечные значения
rayon_reg_wgs <- rayon_reg_wgs %>%
  filter(if_all(all_of(model_vars), ~ is.finite(.)))

# Пересчитываем OLS на актуальной выборке
ols_step <- lm(
  formula(ols_step),
  data = rayon_reg_wgs
)

cat("Районов после очистки:", nrow(rayon_reg_wgs), "\n")
cat("Наблюдений в OLS:", length(residuals(ols_step)), "\n")

# Строим матрицу весов заново на тех же районах
coords_reg <- st_coordinates(
  st_point_on_surface(rayon_reg_wgs)
)

W_queen_reg <- poly2nb(
  rayon_reg_wgs,
  queen = TRUE
)

W_queen_reg_listw <- nb2listw(
  W_queen_reg,
  style = "W",
  zero.policy = TRUE
)

# Проверка соответствия размерностей
stopifnot(
  length(residuals(ols_step)) ==
    length(W_queen_reg_listw$neighbours)
)

cat("Матрица весов и OLS согласованы по размерности\n")

# Тест Морана для остатков OLS
moran_ols_full <- lm.morantest(
  ols_step,
  W_queen_reg_listw,
  zero.policy = TRUE
)

cat("Тест Морана для остатков OLS\n")
print(moran_ols_full)

# ---- ЭТАП 7. LM-ТЕСТЫ -------------------------------------------------------
lm_tests_full <- lm.RStests(
  ols_step,
  listw = W_queen_reg_listw,
  test = "all",
  zero.policy = TRUE
)

cat("\nLM-тесты пространственной зависимости\n")
print(lm_tests_full)

# ---- ЭТАП 8. SAR-МОДЕЛЬ -----------------------------------------------------
sar_full <- lagsarlm(
  formula = final_formula,
  data = rayon_reg_wgs,
  listw = W_queen_reg_listw,
  method = "eigen",
  zero.policy = TRUE
)

cat("\nSAR-модель\n")
print(summary(sar_full))

# ---- ЭТАП 9. SEM-МОДЕЛЬ -----------------------------------------------------
sem_full <- errorsarlm(
  formula = final_formula,
  data = rayon_reg_wgs,
  listw = W_queen_reg_listw,
  method = "eigen",
  zero.policy = TRUE
)

cat("\nSEM-модель\n")
print(summary(sem_full))

# ---- ЭТАП 10. СРАВНЕНИЕ МОДЕЛЕЙ ---------------------------------------------
model_comparison_full <- data.frame(
  Model = c("OLS", "SAR", "SEM"),
  LogLik = c(
    as.numeric(logLik(ols_step)),
    as.numeric(logLik(sar_full)),
    as.numeric(logLik(sem_full))
  ),
  AIC = c(
    AIC(ols_step),
    AIC(sar_full),
    AIC(sem_full)
  ),
  BIC = c(
    BIC(ols_step),
    BIC(sar_full),
    BIC(sem_full)
  )
)

cat("Сравнение моделей\n")
print(model_comparison_full)

best_model_name <- model_comparison_full$Model[
  which.min(model_comparison_full$AIC)
]

cat("Лучшая модель по AIC:", best_model_name, "\n")

# ---- ЭТАП 11. ИЗВЛЕЧЕНИЕ ЭФФЕКТОВ SAR --------------------------------------
# (в методическом скрипте объект использовался без создания — добавлен вызов)
sar_impacts_full <- impacts(
  sar_full,
  listw = W_queen_reg_listw,
  R = 1000,
  zero.policy = TRUE
)

impacts_summary <- summary(
  sar_impacts_full,
  zstats = TRUE,
  short = TRUE
)

# Проверяем структуру объектов
str(impacts_summary$direct)
str(impacts_summary$indirect)
str(impacts_summary$total)

# Функция извлечения оценок из разных структур
extract_effects <- function(x) {
  if (is.list(x)) {
    # Если это список, извлекаем первый числовой элемент
    x <- x[[1]]
  }

  if (is.matrix(x) || is.data.frame(x)) {
    # Если это таблица, извлекаем первый столбец
    x <- x[, 1]
  }

  as.numeric(x)
}

direct_values <- extract_effects(impacts_summary$direct)
indirect_values <- extract_effects(impacts_summary$indirect)
total_values <- extract_effects(impacts_summary$total)

# Проверяем количество значений
cat("Прямые эффекты:", length(direct_values), "\n")
cat("Косвенные эффекты:", length(indirect_values), "\n")
cat("Общие эффекты:", length(total_values), "\n")

# Формируем таблицу
# Техническая правка: impacts() рассчитывает эффекты только для
# регрессоров (без свободного члена), поэтому метки берём из имён
# коэффициентов, исключая "(Intercept)" и "rho" — иначе метки
# сдвигаются на одну переменную.
regressor_names <- names(coef(sar_full))[
  !names(coef(sar_full)) %in% c("(Intercept)", "rho", "lambda")
]

if (length(direct_values) == length(indirect_values) &&
    length(direct_values) == length(total_values) &&
    length(regressor_names) == length(direct_values)) {

  impacts_table <- data.frame(
    Variable = regressor_names[seq_along(direct_values)],
    Direct = direct_values,
    Indirect = indirect_values,
    Total = total_values
  )

  print(impacts_table, row.names = FALSE)

} else {
  stop(
    "Количество эффектов не совпадает. ",
    "Выполните str(impacts_summary) и пришлите результат."
  )
}

# Значимость эффектов (z-статистики)
impacts_z <- impacts_summary
direct_z <- tryCatch(extract_effects(impacts_z$zdirect),
                     error = function(e) NA)
indirect_z <- tryCatch(extract_effects(impacts_z$zindirect),
                       error = function(e) NA)
total_z <- tryCatch(extract_effects(impacts_z$ztotal),
                    error = function(e) NA)
if (!all(is.na(direct_z))) {
  impacts_table$p_direct <- 2 * pnorm(-abs(direct_z))
  impacts_table$p_indirect <- 2 * pnorm(-abs(indirect_z))
  impacts_table$p_total <- 2 * pnorm(-abs(total_z))
}

write.csv(impacts_table, "output/task2_impacts.csv", row.names = FALSE)

# ---- ЭТАП 12. ВИЗУАЛИЗАЦИЯ ОСТАТКОВ И СРАВНЕНИЕ МОДЕЛЕЙ --------------------
rayon_reg_wgs$resid_ols <- as.numeric(residuals(ols_step))
rayon_reg_wgs$resid_sar <- as.numeric(residuals(sar_full))
rayon_reg_wgs$resid_sem <- as.numeric(residuals(sem_full))

# Преобразуем геоданные для карты
rayon_reg_utm <- st_transform(rayon_reg_wgs, 32637)

# Карта остатков OLS
p_resid_ols <- ggplot(rayon_reg_utm) +
  geom_sf(
    aes(fill = resid_ols),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 0,
    name = "Остатки"
  ) +
  labs(
    title = "Остатки OLS-модели",
    subtitle = "Положительные и отрицательные ошибки прогноза"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/task2_resid_ols.png", p_resid_ols,
       width = 8, height = 7, dpi = 200)

# Карта остатков SAR
p_resid_sar <- ggplot(rayon_reg_utm) +
  geom_sf(
    aes(fill = resid_sar),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 0,
    name = "Остатки"
  ) +
  labs(
    title = "Остатки SAR-модели",
    subtitle = "Ошибки модели с пространственным лагом"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/task2_resid_sar.png", p_resid_sar,
       width = 8, height = 7, dpi = 200)

# Карта остатков SEM
p_resid_sem <- ggplot(rayon_reg_utm) +
  geom_sf(
    aes(fill = resid_sem),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 0,
    name = "Остатки"
  ) +
  labs(
    title = "Остатки SEM-модели",
    subtitle = "Ошибки модели с пространственной ошибкой"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/task2_resid_sem.png", p_resid_sem,
       width = 8, height = 7, dpi = 200)

# График сравнения AIC
p_aic_full <- ggplot(
  model_comparison_full,
  aes(x = Model, y = AIC, fill = Model)
) +
  geom_col() +
  geom_text(
    aes(label = round(AIC, 1)),
    vjust = -0.5
  ) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Сравнение моделей по AIC",
    subtitle = "Меньшее значение AIC предпочтительнее",
    x = "",
    y = "AIC"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/task2_aic.png", p_aic_full,
       width = 7, height = 5, dpi = 200)

# График эффектов SAR
p_impacts <- impacts_table %>%
  pivot_longer(
    cols = c(Direct, Indirect, Total),
    names_to = "Effect",
    values_to = "Value"
  ) %>%
  ggplot(
    aes(
      x = reorder(Variable, Value),
      y = Value,
      fill = Effect
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Прямые, косвенные и общие эффекты SAR",
    x = "",
    y = "Эффект"
  ) +
  theme_minimal()

ggsave("output/task2_impacts.png", p_impacts,
       width = 9, height = 7, dpi = 200)

# ---- ЭТАП 13. ИТОГОВАЯ ТАБЛИЦА КОЭФФИЦИЕНТОВ -------------------------------
# OLS
ols_summary <- summary(ols_step)$coefficients

coef_ols_df <- data.frame(
  Variable = rownames(ols_summary),
  Estimate = ols_summary[, "Estimate"],
  Std_Error = ols_summary[, "Std. Error"],
  Statistic = ols_summary[, "t value"],
  p_value = ols_summary[, "Pr(>|t|)"],
  Model = "OLS",
  row.names = NULL
)

# SAR
sar_summary <- summary(sar_full)$Coef

coef_sar_df <- data.frame(
  Variable = rownames(sar_summary),
  Estimate = sar_summary[, "Estimate"],
  Std_Error = sar_summary[, "Std. Error"],
  Statistic = sar_summary[, "z value"],
  p_value = sar_summary[, "Pr(>|z|)"],
  Model = "SAR",
  row.names = NULL
)

# SEM
sem_summary <- summary(sem_full)$Coef

coef_sem_df <- data.frame(
  Variable = rownames(sem_summary),
  Estimate = sem_summary[, "Estimate"],
  Std_Error = sem_summary[, "Std. Error"],
  Statistic = sem_summary[, "z value"],
  p_value = sem_summary[, "Pr(>|z|)"],
  Model = "SEM",
  row.names = NULL
)

# Объединяем результаты
all_coefs_full <- bind_rows(
  coef_ols_df,
  coef_sar_df,
  coef_sem_df
) %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.1 ~ ".",
      TRUE ~ ""
    )
  )

cat("Сводная таблица коэффициентов\n")

print(
  all_coefs_full %>%
    select(
      Model,
      Variable,
      Estimate,
      Std_Error,
      p_value,
      sig
    ) %>%
    arrange(Model, Variable),
  row.names = FALSE
)

write.csv(all_coefs_full, "output/task2_coefficients.csv", row.names = FALSE)
write.csv(model_comparison_full, "output/task2_model_comparison.csv",
          row.names = FALSE)

# ---- Сохранение результатов для отчёта --------------------------------------
# LM-тесты: аккуратно извлекаем статистики и p-значения
lm_tests_df <- data.frame(
  Test = c("LM-лаг (LMlag)", "LM-ошибка (LMerr)",
           "RLM-лаг (устойчивый)", "RLM-ошибка (устойчивый)"),
  Statistic = NA_real_,
  p_value = NA_real_
)
lm_obj <- lm_tests_full
for (i in seq_len(nrow(lm_tests_df))) {
  tst <- tryCatch(lm_obj[[i]], error = function(e) NULL)
  if (!is.null(tst)) {
    lm_tests_df$Statistic[i] <- as.numeric(tst$statistic)
    lm_tests_df$p_value[i] <- as.numeric(tst$p.value)
  }
}

saveRDS(
  list(
    ols_step           = ols_step,
    sar_full           = sar_full,
    sem_full           = sem_full,
    model_comparison   = model_comparison_full,
    best_model_name    = best_model_name,
    moran_ols          = moran_ols_full,
    lm_tests_df        = lm_tests_df,
    impacts_table      = impacts_table,
    all_coefs          = all_coefs_full,
    n_rayons_final     = nrow(rayon_reg_wgs),
    queen_mean_nb      = mean(card(W_queen_reg_listw$neighbours)),
    queen_isolated     = sum(card(W_queen_reg_listw$neighbours) == 0),
    ols_r2             = summary(ols_step)$r.squared,
    ols_adj_r2         = summary(ols_step)$adj.r.squared,
    sar_rho            = sar_full$rho,
    sar_rho_p          = tryCatch(summary(sar_full)$LR1$p.value,
                                  error = function(e) NA_real_),
    sem_lambda         = sem_full$lambda,
    sem_lambda_p       = tryCatch(summary(sem_full)$LR1$p.value,
                                  error = function(e) NA_real_)
  ),
  "output/task2_results.rds"
)

cat("\nЗадача 2 завершена. Результаты: output/task2_results.rds\n")
cat("Таблицы: output/task2_coefficients.csv, output/task2_impacts.csv,",
    "output/task2_model_comparison.csv\n")
