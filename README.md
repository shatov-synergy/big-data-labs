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
    task1_diamonds.R  задача 1: визуализация diamonds
    task2_mtcars.R    задача 2: статистический анализ mtcars
    task3_tesla.R     задача 3: временной ряд Tesla
    zadacha{1,2,3}_report.Rmd  отчёты по каждой задаче (R Markdown -> docx)
    reference.docx    стили отчёта (Times New Roman 14, поля 2/1,5/2/3,
                      красная строка 1,25)
    output/           результаты: графики, html, отчёты docx+pdf
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

## Правила для ИИ-агентов и нейросетей

Любая нейросеть (ZCode, Claude Code, Codex, Copilot и др.), работающая
с этим репозиторием, обязана следовать:

- [`AGENTS.md`](AGENTS.md) — главный файл правил: структура, команды,
  сетевые особенности, запреты, конвенции;
- [`templates/REPORT_STANDARDS.md`](templates/REPORT_STANDARDS.md) —
  стандарты оформления отчётов;
- [`ROADMAP.md`](ROADMAP.md) — дорожная карта валидации: этапы
  (проходы 1–6) и чеклист контрольных требований аудита практикума 4
  (задача 1); дословный протокол —
  [`labs/lab-04/audit/protocol-zadacha1-apriori.md`](labs/lab-04/audit/protocol-zadacha1-apriori.md);
- скиллы в `.agents/skills/`: `new-lab` (новая лабораторная),
  `render-reports` (сборка docx+pdf), `synergy-report` (сверка стиля
  отчёта);
- `CLAUDE.md` и `.github/copilot-instructions.md` — то же для
  Claude Code и GitHub Copilot.

Чтобы ZCode подхватил правила и скиллы автоматически, открывайте сессию
в корне этого репозитория.

## Выполнение лабораторной работы

В RStudio откройте `labs/lab-01/` и запускайте скрипты по одному
(`Ctrl+Shift+Enter` — строка, `Ctrl+Shift+S` — весь файл).
Скрипты пишут графики в `labs/lab-01/output/`.

Отчёты: по **каждой задаче** — отдельный файл `zadachaN_report.Rmd`.
Кнопка `Knit` в RStudio собирает docx в стилях Синергии.
По требованиям дисциплины результат сдаётся в **docx и pdf**:

```bash
# docx (внутри контейнера или в RStudio)
docker compose run --rm rstudio bash -lc \
  "cd labs/lab-01 && Rscript -e 'rmarkdown::render(\"zadacha1_report.Rmd\", output_dir = \"output\")'"

# pdf — конвертация docx (LibreOffice)
docker compose run --rm rstudio bash -lc \
  "apt-get update -qq && apt-get install -y -qq libreoffice-writer && \
   cd labs/lab-01/output && soffice --headless --convert-to pdf \
   'Laboratornyiy praktikum 1_Задача 1_отчет.docx'"
```

Готовые файлы для сдачи лежат в `labs/lab-01/output/`:
`Laboratornyiy praktikum 1_Задача {1,2,3}_отчет.docx` и `.pdf`.

## Особенности сети

Образ ставит пакеты с зеркала CRAN, доступного из Docker-контейнеров
(P3M/p3m.dev из ряда сетей из контейнеров не отвечает — скрипт
`docker/install_pkgs.R` перебирает зеркала автоматически).
Если понадобится доустановить пакет в сессии RStudio:
`install.packages("имя")` — зеркало уже прописано в `Rprofile.site`.
