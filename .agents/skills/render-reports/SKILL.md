---
name: render-reports
description: Пересобрать отчёты лабораторной (Rmd -> docx) и сконвертировать в pdf внутри Docker-контейнера big-data-labs. Использовать после правок zadachaM_report.Rmd или шаблонов оформления.
---

# Скилл: сборка отчётов (docx + pdf)

Работает из корня репозитория (где `docker-compose.yml`).

## 1. Если менялись шаблоны оформления

После правок `templates/synergy_docx.py` или `templates/build_templates.py`:

```bash
cd templates && python build_templates.py && cd ..
```

Скрипт пересоздаст `templates/report_template.docx`, `templates/reference.docx`
и скопирует reference.docx в `labs/lab-01/`. Для других lab-NN скопировать
reference.docx туда вручную.

## 2. Рендер docx

Rmd НЕ содержит титульного листа (начинается сразу с заголовка «Задача N»).

```bash
docker compose run --rm rstudio bash -lc \
  "cd labs/lab-01 && for i in 1 2 3; do Rscript -e \"rmarkdown::render('zadacha\${i}_report.Rmd', output_dir = 'output')\"; done"
```

## 3. Титульный лист (клон ВКР) + центрирование картинок

Сразу после рендера, на хосте (нужен python-docx: `pip install python-docx`):

```bash
python templates/apply_title_page.py "labs/lab-01/output/zadacha1_report.docx" \
  --lab 1 --topic "Задача 1. Разведочный анализ. Базовая визуализация данных"
# для остальных задач — свои --topic
```

Скрипт вставляет титульный лист (структура 1-в-1 с ВКР: форма-таблицы,
подчёркнутые ячейки, подсказки, «МОСКВА 2026 г.») и центрирует все
абзацы с изображениями.

## 4. Итоговые имена + PDF (LibreOffice в контейнере)

```bash
cd labs/lab-01/output
cp -f zadacha1_report.docx "Laboratornyiy praktikum 1_Задача 1_отчет.docx"
# ... для каждой задачи ...

docker compose run --rm rstudio bash -lc \
  "apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq --no-install-recommends libreoffice-writer poppler-utils fonts-liberation >/dev/null 2>&1 && cd /home/rstudio/project/labs/lab-01/output && for i in 1 2 3; do soffice --headless --convert-to pdf --outdir . \"Laboratornyiy praktikum 1_Задача \${i}_отчет.docx\" >/dev/null 2>&1; done && ls -la *.pdf"
```

LibreOffice ставится во временный контейнер (~2 мин), образ не меняется.

## 5. Контроль качества

- `python <путь к docx-скиллу>/scripts/postcheck.py <файл>.docx` — 0 ошибок.
- Выборочная визуальная проверка: pdftoppm первой страницы (титульник) и
  страницы с таблицей/рисунком.
- Убрать временные файлы проверок (`_chk*`, `_verify`).

## Частые ошибки

- `there is no package called 'X'` — добавить пакет в `docker/install_pkgs.R`
  и пересобрать образ `docker compose build`.
- Картинка не по центру — титульник/центрирование применяются ТОЛЬКО
  скриптом `apply_title_page.py` после рендера; повторный прогон скрипта
  по уже обработанному файлу вставит второй титульник — сначала
  перерендерите docx.
- Подпись не в формате ГОСТ — рисунки: «Рисунок N — Название» (тире),
  таблицы: «Таблица N — Название» одной строкой слева над таблицей.
- Кракозябры в pdf — конвертацию делать только с fonts-liberation (TNR
  подменяется Liberation Serif, метрически совместим).
