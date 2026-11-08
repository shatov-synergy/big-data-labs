# ============================================================================
# Лабораторный практикум 3. Тема 2. Методы очистки и предобработки данных
# Задача 1 (часть 1). Предварительная обработка данных о квартирах и метро,
# привязка к районам Москвы, агрегирование показателей по районам,
# матрицы пространственных весов, индексы Морана (глобальный и локальный).
#
# Код следует методическому скрипту преподавателя с техническими правками:
#  - данные читаются из labs/lab-03/data/ (DATA.xlsx, METRO.xlsx, export.geojson);
#  - графики сохраняются в output/ через ggsave();
#  - результаты сохраняются в output/task1_results.rds для отчёта и задачи 2.
# ============================================================================

# ---- Установка и загрузка пакетов -------------------------------------------
pkgs <- c("tidyverse", "readr", "knitr", "sf", "geosphere", "spdep",
          "ggplot2", "viridis", "scales", "ggrepel", "patchwork",
          "stringr", "dplyr", "readxl")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new) > 0) install.packages(new)

library(tidyverse)
library(readr)
library(knitr)
library(sf)            # простые пространственные объекты
library(geosphere)     # географические расстояния
library(spdep)         # матрицы весов, Moran I, LISA
library(ggplot2)
library(viridis)       # цветовые палитры
library(scales)
library(ggrepel)
library(patchwork)     # компоновка нескольких графиков рядом
library(stringr)
library(dplyr)
library(readxl)

set.seed(42)

# ---- ЗАГРУЗКА ДАННЫХ --------------------------------------------------------
df_realty <- read_excel("data/DATA.xlsx")
df_metro  <- read_excel("data/METRO.xlsx")
cat("Строк в данных о квартирах:", nrow(df_realty), "\n")
cat("Строк в данных о метро:", nrow(df_metro), "\n")
str(df_realty)

# ---- ПРЕДОБРАБОТКА ДАННЫХ О НЕДВИЖИМОСТИ (базовая) --------------------------
df_realty$Цена <- as.numeric(gsub("[^0-9]", "", df_realty$Цена))
df_realty$Общая.площадь <- as.numeric(gsub(" м²", "", df_realty$Общая.площадь))
df_realty$Этаж_номер <- as.numeric(str_extract(df_realty$Этаж, "^\\d+"))
df_realty$Этажей.в.доме <- as.numeric(str_extract(df_realty$Этажей.в.доме, "^\\d+"))
df_realty$Широта  <- as.numeric(df_realty$Широта)
df_realty$Долгота <- as.numeric(df_realty$Долгота)
df_realty$Тип.дома     <- as.factor(df_realty$Тип.дома)
df_realty$Санузел      <- as.factor(df_realty$Санузел)
df_realty$Ремонт       <- as.factor(df_realty$Ремонт)
df_realty$Тип.рынка    <- as.factor(df_realty$Тип.рынка)
df_realty$Подкатегория <- as.factor(df_realty$Подкатегория)

df_realty$доля_этажа <- df_realty$Этаж_номер / df_realty$Этажей.в.доме

df_realty <- df_realty %>%
  filter(!is.na(Цена), !is.na(Общая.площадь),
         !is.na(Широта), !is.na(Долгота))

cat("Квартир после очистки:", nrow(df_realty), "\n")

# ---- ПРЕДОБРАБОТКА ДАННЫХ О МЕТРО -------------------------------------------
df_metro_clean <- df_metro %>%
  filter(!is.na(Долгота), !is.na(Широта)) %>%
  mutate(
    Долгота = as.numeric(Долгота),
    Широта  = as.numeric(Широта)
  )

metro_coords <- cbind(df_metro_clean$Долгота, df_metro_clean$Широта)

cat("Выходов метро:", nrow(df_metro_clean), "\n")

# ---- ЗАГРУЗКА ПОЛИГОНОВ РАЙОНОВ МОСКВЫ ИЗ GEOJSON ---------------------------
path_geojson <- "data/export.geojson"
if (!file.exists(path_geojson)) {
  stop("Файл полигонов районов не найден: ", path_geojson)
}

rayons_raw <- st_read(path_geojson, quiet = FALSE)

# ---- ОЧИСТКА ПОЛИГОНОВ ------------------------------------------------------
cat("До фильтрации:", nrow(rayons_raw), "объектов\n")

rayons_clean <- rayons_raw %>%
  # Только полигоны и мультиполигоны
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  # Только admin_level=8
  filter(admin_level == "8") %>%
  # Отбираем нужные столбцы
  select(
    any_of(c("id", "name", "alt_name", "wikidata", "wikipedia")),
    geometry
  ) %>%
  st_make_valid() %>%
  distinct(name, .keep_all = TRUE)

cat("После фильтрации:", nrow(rayons_clean), "районов\n")
cat("Типы геометрий:\n")
print(table(st_geometry_type(rayons_clean)))
cat("Первые названия:\n")
print(head(rayons_clean$name, 10))

# ---- НОРМАЛИЗАЦИЯ НАЗВАНИЙ РАЙОНОВ -----------------------------------------
normalize_rayon <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("ё", "е") %>%
    str_replace_all("(?i)^район\\s+", "") %>%
    str_replace_all("(?i)^муниципальный округ\\s+", "") %>%
    str_replace_all("(?i)\\s+район$", "") %>%
    str_replace_all("(?i)^поселение\\s+", "") %>%
    str_replace_all("(?i)^городской округ\\s+", "") %>%
    str_replace_all("[^а-яa-z0-9]", "") %>%
    str_squish()
}

rayons_clean <- rayons_clean %>%
  mutate(rayon_key = normalize_rayon(name))

cat("Примеры нормализации:\n")
rayons_clean %>%
  st_drop_geometry() %>%
  select(name, rayon_key) %>%
  head(10) %>%
  print()

# ---- ЦЕНТРОИДЫ РАЙОНОВ (для матрицы весов и подписей) ----------------------
rayons_centroids <- rayons_clean %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(rayon_key, rayon_name = name, lon, lat)

cat("Центроиды рассчитаны:", nrow(rayons_centroids), "\n")

# ---- ПРИВЯЗКА КВАРТИР К РАЙОНАМ ЧЕРЕЗ ПОЛИГОНЫ -----------------------------
df_sf <- df_realty %>%
  st_as_sf(coords = c("Долгота", "Широта"), crs = 4326)

# Пространственное соединение: квартира -> полигон района
df_joined <- st_join(df_sf, rayons_clean, join = st_within)

cat("Квартир с определённым районом:", sum(!is.na(df_joined$rayon_key)), "\n")
cat("Квартир без района:", sum(is.na(df_joined$rayon_key)), "\n")

# Если есть квартиры вне полигонов (граничные случаи) —
# привяжем их к ближайшему центроиду
df_joined_fixed <- df_joined %>%
  mutate(
    rayon_key = ifelse(
      is.na(rayon_key),
      rayons_centroids$rayon_key[
        st_nearest_feature(., st_as_sf(rayons_centroids,
                                       coords = c("lon", "lat"),
                                       crs = 4326))
      ],
      rayon_key
    )
  )

# ---- РАСЧЁТ РАССТОЯНИЯ ДО МЕТРО (для каждого дома) --------------------------
find_nearest_metro_idx <- function(lon, lat, metro_coords) {
  if (is.na(lon) || is.na(lat)) return(NA_integer_)
  d <- distHaversine(c(lon, lat), metro_coords)
  return(which.min(d))
}

# Координаты обратно в столбцы
coords_joined <- st_coordinates(df_joined_fixed)
df_joined_fixed$lon <- coords_joined[, 1]
df_joined_fixed$lat <- coords_joined[, 2]

df_joined_fixed$dist_to_metro <- NA_real_
df_joined_fixed$nearest_metro <- NA_character_

for (i in 1:nrow(df_joined_fixed)) {
  if (i %% 500 == 0) {
    cat("Обработано", i, "из", nrow(df_joined_fixed), "\n")
  }
  idx <- find_nearest_metro_idx(df_joined_fixed$lon[i],
                                df_joined_fixed$lat[i],
                                metro_coords)
  if (is.na(idx)) next
  df_joined_fixed$dist_to_metro[i] <- distHaversine(
    c(df_joined_fixed$lon[i], df_joined_fixed$lat[i]),
    metro_coords[idx, ]
  )
  df_joined_fixed$nearest_metro[i] <- df_metro_clean$Метро[idx]
}

df_joined_fixed$dist_to_metro_km <- df_joined_fixed$dist_to_metro / 1000

# ---- АГРЕГАЦИЯ ЦЕН ПО РАЙОНАМ ----------------------------------------------
rayon_prices <- df_joined_fixed %>%
  st_drop_geometry() %>%
  filter(!is.na(rayon_key)) %>%
  group_by(rayon_key) %>%
  summarise(
    mean_price = mean(Цена, na.rm = TRUE),
    n          = n(),
    .groups    = "drop"
  ) %>%
  filter(n >= 5)

cat("Районов с ценой:", nrow(rayon_prices), "\n")

# Соединяем с полигонами
rayon_sf <- rayons_clean %>%
  inner_join(rayon_prices, by = "rayon_key")

rayon_sf$log_price <- log(rayon_sf$mean_price)

cat("Районов с полигоном и ценой:", nrow(rayon_sf), "\n")

# ---- ПЕРЕПРОЕЦИРОВАНИЕ ДЛЯ КАРТ ---------------------------------------------
# WGS84 — для расстояний и Moran
rayon_sf_wgs <- rayon_sf

# UTM 37N — для карт
rayon_sf_utm <- st_transform(rayon_sf, 32637)

# ---- МАТРИЦА ПРОСТРАНСТВЕННЫХ ВЕСОВ (k-NN на центроидах) -------------------
coords <- st_coordinates(st_centroid(rayon_sf_wgs))

# k-NN
k <- 8
W_knn       <- knn2nb(knearneigh(coords, k = k))
W_knn_listw <- nb2listw(W_knn, style = "W", zero.policy = TRUE)

# Queen (по полигонам) — реальная фишка GeoJSON
W_queen <- poly2nb(rayon_sf_wgs, queen = TRUE)
W_queen_listw <- nb2listw(W_queen, style = "W", zero.policy = TRUE)

cat("Матрица k-NN\n")
cat("Среднее число соседей:", mean(card(W_knn_listw$neighbours)), "\n")
cat("Изолированные районы:", sum(card(W_knn_listw$neighbours) == 0), "\n")

cat("Матрица Queen\n")
cat("Среднее число соседей:", mean(card(W_queen_listw$neighbours)), "\n")
cat("Изолированные районы:", sum(card(W_queen_listw$neighbours) == 0), "\n")

# ---- ГЛОБАЛЬНЫЙ ИНДЕКС МОРАНА ----------------------------------------------
moran_knn   <- moran.test(rayon_sf_wgs$log_price, W_knn_listw,
                          zero.policy = TRUE)
moran_queen <- moran.test(rayon_sf_wgs$log_price, W_queen_listw,
                          zero.policy = TRUE)

cat("Moran's I (k-NN)\n")
print(moran_knn)

cat("Moran's I (Queen)\n")
print(moran_queen)

# ---- ЛОКАЛЬНЫЙ ИНДЕКС МОРАНА (LISA) — используем Queen ---------------------
lisa <- localmoran(rayon_sf_wgs$log_price, W_queen_listw,
                   zero.policy = TRUE)

rayon_sf_wgs <- rayon_sf_wgs %>%
  mutate(
    lag_log_price = lag.listw(W_queen_listw, log_price, zero.policy = TRUE),
    mean_log      = mean(log_price, na.rm = TRUE),
    mean_lag      = mean(lag_log_price, na.rm = TRUE),
    quadrant = case_when(
      log_price >= mean_log & lag_log_price >= mean_lag ~ "High-High",
      log_price <  mean_log & lag_log_price <  mean_lag ~ "Low-Low",
      log_price >= mean_log & lag_log_price <  mean_lag ~ "High-Low",
      TRUE                                              ~ "Low-High"
    ),
    p_value     = lisa[, "Pr(z != E(Ii))"],
    significant = !is.na(p_value) & p_value < 0.1,
    cluster_type = factor(
      case_when(
        is.na(p_value) | !significant ~ "Not significant",
        TRUE                          ~ quadrant
      ),
      levels = c("High-High", "Low-Low", "High-Low", "Low-High",
                 "Not significant")
    )
  )

cat("Распределение LISA-кластеров\n")
print(table(rayon_sf_wgs$cluster_type))
cat("Доля значимых:", round(mean(rayon_sf_wgs$significant) * 100, 1), "%\n")

# Синхронизация UTM-объекта для карты
rayon_sf_utm <- rayon_sf_utm %>%
  left_join(
    rayon_sf_wgs %>% st_drop_geometry() %>%
      select(rayon_key, cluster_type, p_value, significant),
    by = "rayon_key"
  )

# ---- КАРТА РАСПРЕДЕЛЕНИЯ ЦЕН (ХОРОПЛЕТ) ------------------------------------
p_map <- ggplot(rayon_sf_utm) +
  geom_sf(aes(fill = mean_price), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = label_number(scale = 1e-6, suffix = " млн ₽"),
    name   = "Средняя цена"
  ) +
  labs(
    title    = "Распределение средних цен на жильё по районам Москвы",
    subtitle = "Полигоны районов из OpenStreetMap",
    caption  = "Источник: собственные расчёты"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/task1_price_map.png", p_map, width = 9, height = 7.5, dpi = 200)

p_lisa <- ggplot(rayon_sf_utm) +
  geom_sf(
    aes(fill = cluster_type),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "High-High"       = "#d73027",
      "Low-Low"         = "#4575b4",
      "High-Low"        = "#fdae61",
      "Low-High"        = "#abd9e9",
      "Not significant" = "grey90"
    ),
    name = "Тип кластера"
  ) +
  geom_sf_text(
    data = rayon_sf_utm %>% filter(significant),
    aes(label = rayon_key),
    size = 2.2,
    color = "black",
    check_overlap = TRUE
  ) +
  labs(
    title = "LISA-кластеры цен на жильё в Москве",
    subtitle = paste0(
      "Матрица Queen, Moran's I = ",
      round(moran_queen$estimate[1], 3),
      ", значимых: ",
      round(mean(rayon_sf_wgs$significant) * 100),
      "%"
    ),
    caption = "Источник: собственные расчёты"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/task1_lisa_map.png", p_lisa, width = 9, height = 7.5, dpi = 200)

# ---- РАСШИРЕННАЯ ПРЕДОБРАБОТКА ДАННЫХ ---------------------------------------
df_realty_clean <- df_realty %>%
  mutate(
    # Цена
    Цена = as.numeric(gsub("[^0-9]", "", Цена)),

    # Площадь
    Общая.площадь = as.numeric(gsub("[^0-9.]", "", Общая.площадь)),

    # Этаж: извлекаем текущий и общий
    Этаж_номер = as.numeric(str_extract(Этаж, "^\\d+")),
    Этажей.в.доме = as.numeric(str_extract(Этажей.в.доме, "^\\d+")),

    # Широта/Долгота
    Широта = as.numeric(Широта),
    Долгота = as.numeric(Долгота)
  ) %>%
  # Факторы
  mutate(
    Подкатегория = factor(Подкатегория),
    Тип.рынка = factor(Тип.рынка),
    Тип.дома = factor(Тип.дома),
    Санузел = factor(Санузел),
    Ремонт = factor(Ремонт)
  ) %>%
  # Производные переменные
  mutate(
    # Доля этажа (0 = первый, 1 = последний)
    доля_этажа = Этаж_номер / Этажей.в.доме,

    # Первый этаж (бинарный)
    первый_этаж = as.integer(Этаж_номер == 1),

    # Последний этаж (бинарный)
    последний_этаж = as.integer(Этаж_номер == Этажей.в.доме),

    # Логарифм цены
    log_price = log(Цена),

    # Логарифм площади
    log_area = log(Общая.площадь),

    # Цена за квадратный метр
    price_per_sqm = Цена / Общая.площадь,

    # Логарифм цены за кв.м
    log_price_sqm = log(price_per_sqm),

    # Этажность дома (категория)
    этажность_кат = case_when(
      Этажей.в.доме <= 5 ~ "низкая (1-5)",
      Этажей.в.доме <= 9 ~ "средняя (6-9)",
      Этажей.в.доме <= 17 ~ "повышенная (10-17)",
      TRUE ~ "высотная (18+)"
    ),
    этажность_кат = factor(этажность_кат,
                           levels = c("низкая (1-5)", "средняя (6-9)",
                                      "повышенная (10-17)", "высотная (18+)")),

    # Категория ремонта (упорядоченная)
    Ремонт = factor(Ремонт,
                    levels = c("требует ремонта", "косметический",
                               "евро", "дизайнерский"),
                    ordered = FALSE)
  ) %>%
  # Удаляем пропуски
  filter(
    !is.na(Цена), !is.na(Общая.площадь),
    !is.na(Широта), !is.na(Долгота),
    !is.na(Этаж_номер), !is.na(Этажей.в.доме),
    Общая.площадь > 0, Цена > 0,
    Этажей.в.доме > 0, Этаж_номер > 0
  )

cat("Квартир после расширенной очистки:", nrow(df_realty_clean), "\n")
cat("\nРаспределение по подкатегориям:\n")
print(table(df_realty_clean$Подкатегория))
cat("Распределение по типу рынка:\n")
print(table(df_realty_clean$Тип.рынка))
cat("Распределение по типу дома:\n")
print(table(df_realty_clean$Тип.дома))
cat("Распределение по ремонту:\n")
print(table(df_realty_clean$Ремонт))
cat("Распределение по санузлу:\n")
print(table(df_realty_clean$Санузел))
cat("Распределение по этажности:\n")
print(table(df_realty_clean$этажность_кат))

# Проверка на выбросы
cat("Описательные статистики цены:\n")
print(summary(df_realty_clean$Цена))
cat("Описательные статистики площади:\n")
print(summary(df_realty_clean$Общая.площадь))
cat("Описательные статистики цены за кв.м:\n")
print(summary(df_realty_clean$price_per_sqm))

# Удаляем экстремальные выбросы (1% и 99% перцентили)
q_price <- quantile(df_realty_clean$Цена, c(0.01, 0.99), na.rm = TRUE)
q_area <- quantile(df_realty_clean$Общая.площадь, c(0.01, 0.99), na.rm = TRUE)
q_ppsqm <- quantile(df_realty_clean$price_per_sqm, c(0.01, 0.99), na.rm = TRUE)

df_realty_clean <- df_realty_clean %>%
  filter(
    Цена >= q_price[1], Цена <= q_price[2],
    Общая.площадь >= q_area[1], Общая.площадь <= q_area[2],
    price_per_sqm >= q_ppsqm[1], price_per_sqm <= q_ppsqm[2]
  )

cat("\nКвартир после удаления выбросов:", nrow(df_realty_clean), "\n")

# ---- ЭТАП 1 (расширенный): ПРИВЯЗКА К РАЙОНАМ И РАССТОЯНИЕ ДО МЕТРО ---------
df_sf <- df_realty_clean %>%
  st_as_sf(coords = c("Долгота", "Широта"), crs = 4326)

df_joined <- st_join(df_sf, rayons_clean, join = st_within)

cat("Квартир с определённым районом:", sum(!is.na(df_joined$rayon_key)), "\n")
cat("Квартир без района:", sum(is.na(df_joined$rayon_key)), "\n")

# Привязка к ближайшему центроиду для граничных случаев
df_joined_fixed <- df_joined %>%
  mutate(
    rayon_key = ifelse(
      is.na(rayon_key),
      rayons_centroids$rayon_key[
        st_nearest_feature(., st_as_sf(rayons_centroids,
                                       coords = c("lon", "lat"),
                                       crs = 4326))
      ],
      rayon_key
    )
  )

# Расстояние до метро
coords_joined <- st_coordinates(df_joined_fixed)
df_joined_fixed$lon <- coords_joined[, 1]
df_joined_fixed$lat <- coords_joined[, 2]

df_joined_fixed$dist_to_metro <- NA_real_
df_joined_fixed$nearest_metro <- NA_character_

for (i in 1:nrow(df_joined_fixed)) {
  if (i %% 1000 == 0) cat("Обработано", i, "из", nrow(df_joined_fixed), "\n")
  idx <- find_nearest_metro_idx(df_joined_fixed$lon[i],
                                df_joined_fixed$lat[i],
                                metro_coords)
  if (is.na(idx)) next
  df_joined_fixed$dist_to_metro[i] <- distHaversine(
    c(df_joined_fixed$lon[i], df_joined_fixed$lat[i]),
    metro_coords[idx, ]
  )
  df_joined_fixed$nearest_metro[i] <- df_metro_clean$Метро[idx]
}

df_joined_fixed$dist_to_metro_km <- df_joined_fixed$dist_to_metro / 1000

# ---- ЭТАП 2 (расширенный): АГРЕГАЦИЯ ВСЕХ ФАКТОРОВ ПО РАЙОНАМ --------------
rayon_features <- df_joined_fixed %>%
  st_drop_geometry() %>%
  filter(!is.na(rayon_key)) %>%
  group_by(rayon_key) %>%
  summarise(
    # Зависимая переменная
    mean_price = mean(Цена, na.rm = TRUE),
    median_price = median(Цена, na.rm = TRUE),
    mean_log_price = mean(log_price, na.rm = TRUE),
    mean_price_sqm = mean(price_per_sqm, na.rm = TRUE),
    mean_log_price_sqm = mean(log_price_sqm, na.rm = TRUE),

    # Объём выборки
    n = n(),

    # Площадь
    mean_area = mean(Общая.площадь, na.rm = TRUE),
    median_area = median(Общая.площадь, na.rm = TRUE),
    sd_area = sd(Общая.площадь, na.rm = TRUE),

    # Этажность
    mean_floors = mean(Этажей.в.доме, na.rm = TRUE),
    mean_floor_ratio = mean(доля_этажа, na.rm = TRUE),
    share_first_floor = mean(первый_этаж, na.rm = TRUE),
    share_last_floor = mean(последний_этаж, na.rm = TRUE),

    # Транспортная доступность
    mean_dist_metro = mean(dist_to_metro_km, na.rm = TRUE),
    median_dist_metro = median(dist_to_metro_km, na.rm = TRUE),
    min_dist_metro = min(dist_to_metro_km, na.rm = TRUE),
    share_near_metro = mean(dist_to_metro_km < 1, na.rm = TRUE),

    # Структура по подкатегориям
    share_studio = mean(Подкатегория == "Студии", na.rm = TRUE),
    share_apartment = mean(Подкатегория == "Квартиры", na.rm = TRUE),

    # Тип рынка
    share_secondary = mean(Тип.рынка == "Вторичный", na.rm = TRUE),
    share_new = mean(Тип.рынка == "Новостройки", na.rm = TRUE),

    # Тип дома
    share_brick = mean(Тип.дома == "кирпичный", na.rm = TRUE),
    share_panel = mean(Тип.дома == "панельный", na.rm = TRUE),
    share_monolith = mean(Тип.дома == "монолитный", na.rm = TRUE),
    share_monolith_brick = mean(Тип.дома == "монолитно-кирпичный", na.rm = TRUE),

    # Ремонт
    share_designer = mean(Ремонт == "дизайнерский", na.rm = TRUE),
    share_euro = mean(Ремонт == "евро", na.rm = TRUE),
    share_cosmetic = mean(Ремонт == "косметический", na.rm = TRUE),
    share_no_repair = mean(Ремонт == "требует ремонта", na.rm = TRUE),

    # Санузел
    share_combined_bath = mean(Санузел == "совмещенный", na.rm = TRUE),
    share_separate_bath = mean(Санузел == "раздельный", na.rm = TRUE),

    # Этажность
    share_low_rise = mean(этажность_кат == "низкая (1-5)", na.rm = TRUE),
    share_mid_rise = mean(этажность_кат == "средняя (6-9)", na.rm = TRUE),
    share_high_rise = mean(этажность_кат == "повышенная (10-17)", na.rm = TRUE),
    share_skyscraper = mean(этажность_кат == "высотная (18+)", na.rm = TRUE),

    .groups = "drop"
  ) %>%
  filter(n >= 10)

cat("Районов с полными данными:", nrow(rayon_features), "\n")

# ---- ЭТАП 3: ФОРМИРОВАНИЕ РЕГРЕССИОННОГО ДАТАФРЕЙМА ------------------------
# Соединяем с пространственными данными
rayon_reg <- rayons_clean %>%
  inner_join(rayon_features, by = "rayon_key")

rayon_reg_wgs <- rayon_reg
rayon_reg_utm <- st_transform(rayon_reg, 32637)

# ---- Проверка мультиколлинеарности (VIF) ------------------------------------
cat("\nПроверка мультиколлинеарности (VIF):\n")

# Создаём матрицу предикторов
X_vif <- rayon_reg_wgs %>%
  st_drop_geometry() %>%
  select(
    mean_dist_metro, mean_area, n,
    share_studio, share_secondary,
    share_brick, share_panel, share_monolith,
    share_designer, share_euro, share_cosmetic,
    share_combined_bath,
    mean_floors, share_first_floor, share_last_floor,
    share_high_rise, share_skyscraper
  ) %>%
  as.data.frame()

# Функция для расчёта VIF
calc_vif <- function(data) {
  vif_values <- sapply(names(data), function(var) {
    formula <- as.formula(paste(var, "~ ."))
    r2 <- summary(lm(formula, data = data))$r.squared
    if (r2 >= 1) return(Inf)
    1 / (1 - r2)
  })
  return(vif_values)
}

vif_values <- calc_vif(X_vif)
cat("VIF значения:\n")
print(round(sort(vif_values, decreasing = TRUE), 2))

# Удаляем переменные с VIF > 10
vars_to_keep <- names(vif_values)[vif_values < 10]
cat("Переменные с VIF < 10:", length(vars_to_keep), "\n")
print(vars_to_keep)

# ---- Сохранение результатов для задачи 2 и отчёта ---------------------------
saveRDS(
  list(
    rayon_reg_wgs   = rayon_reg_wgs,
    rayon_centroids = rayons_centroids,
    moran_knn       = moran_knn,
    moran_queen     = moran_queen,
    lisa_table      = table(rayon_sf_wgs$cluster_type),
    lisa_share_sig  = mean(rayon_sf_wgs$significant),
    n_realty_raw    = nrow(read_excel("data/DATA.xlsx")),
    n_realty_base   = nrow(df_realty),
    n_realty_clean  = nrow(df_realty_clean),
    n_metro         = nrow(df_metro_clean),
    n_rayons_raw    = nrow(rayons_raw),
    n_rayons_clean  = nrow(rayons_clean),
    n_rayons_priced = nrow(rayon_sf),
    n_rayons_model  = nrow(rayon_reg_wgs),
    knn_neighbors   = mean(card(W_knn_listw$neighbours)),
    queen_neighbors = mean(card(W_queen_listw$neighbours)),
    vif_values      = vif_values,
    summary_price   = summary(df_realty_clean$Цена),
    summary_area    = summary(df_realty_clean$Общая.площадь),
    summary_ppsqm   = summary(df_realty_clean$price_per_sqm),
    tbl_subcat      = table(df_realty_clean$Подкатегория),
    tbl_market      = table(df_realty_clean$Тип.рынка),
    tbl_house       = table(df_realty_clean$Тип.дома),
    tbl_repair      = table(df_realty_clean$Ремонт),
    dist_metro_summary = summary(df_joined_fixed$dist_to_metro_km)
  ),
  "output/task1_results.rds"
)

cat("\nЗадача 1 завершена. Результаты: output/task1_results.rds\n")
cat("Графики: output/task1_price_map.png, output/task1_lisa_map.png\n")
