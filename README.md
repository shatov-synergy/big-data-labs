# big-data-labs

Лабораторные работы по дисциплине
**«Продвинутые технологии работы с большими данными»**
(Университет «Синергия», направление 09.03.02 «Информационные системы и технологии»).

Работы выполняются в **RStudio Server, развёрнутом в Docker** — на любой машине
с Docker получается идентичное окружение (R 4.6.1 + все пакеты).

## Быстрый старт

```bash
cp .env.example .env        # при желании смените пароль
docker compose up -d --build
```

Откройте **http://localhost:8787** — логин `rstudio`, пароль из `.env`
(по умолчанию `bigdata`). Репозиторий примонтирован в `/home/rstudio/project`:
всё, что вы создаёте в RStudio, сразу лежит в git-репозитории.

Остановка: `docker compose down`.

## Структура репозитория

```
assignments/          исходные задания (docx) по каждой работе
labs/
  lab-01/             лабораторная работа 1
    task1_diamonds.R  задание 1: визуализация diamonds
    task2_mtcars.R    задание 2: статистический анализ mtcars
    task3_tesla.R     задание 3: временной ряд Tesla
    lab01_report.Rmd  отчёт (R Markdown -> html/docx в стандартах Синергии)
    reference.docx    стили отчёта (Times New Roman 14, поля 2/1,5/2/3)
    output/           результаты: графики, html, итоговые csv
data/                 кэш данных (котировки TSLA и пр.)
templates/            ЕДИНАЯ СИСТЕМА ОФОРМЛЕНИЯ ОТЧЁТОВ (см. ниже)
docker/               служебные скрипты образа
```

## Шаблоны отчётов (главное)

Все правила оформления зафиксированы в
[`templates/REPORT_STANDARDS.md`](templates/REPORT_STANDARDS.md):
поля страницы, шрифты, титульный лист «Синергии», схема каждого пункта
(«Условие → Код → Результат → Гипотезы → Интерпретация → Вывод»),
подписи рисунков/таблиц.

Два способа собрать отчёт:

1. **Из Python** (любой предмет, нейросеть собирает docx сама):
   ```bash
   python templates/generate_report.py   # пример-демонстрация
   ```
   API — класс `SynergyReport` в `templates/synergy_docx.py`.

2. **Из R Markdown** (для R-лабораторных: код, результаты и графики
   подставляются автоматически при Knit). Шаблон стилей уже подключён
   через `reference_docx: reference.docx`.

Пересборка docx-шаблонов после изменения стандартов:
`python templates/build_templates.py`.

## Выполнение лабораторной работы

В RStudio откройте `labs/lab-01/` и запускайте скрипты по одному
(`Ctrl+Shift+Enter` — строка, `Ctrl+Shift+S` — весь файл).
Скрипты пишут графики в `labs/lab-01/output/`.

Отчёт: откройте `lab01_report.Rmd` → `Knit` (получите
`output/lab01_report.html` и `output/lab01_report.docx`).

Headless-прогон (без интерфейса):

```bash
docker compose run --rm rstudio bash -lc \
  "cd labs/lab-01 && Rscript task1_diamonds.R"
```

## Особенности сети

Образ ставит пакеты с зеркала CRAN, доступного из Docker-контейнеров
(P3M/p3m.dev из ряда сетей из контейнеров не отвечает — скрипт
`docker/install_pkgs.R` перебирает зеркала автоматически).
Если понадобится доустановить пакет в сессии RStudio:
`install.packages("имя")` — зеркало уже прописано в `Rprofile.site`.
