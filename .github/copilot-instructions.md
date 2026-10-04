# Copilot instructions — big-data-labs

Учебный репозиторий: лабораторные работы по «Продвинутые технологии работы
с большими данными» (Синергия) в RStudio Server под Docker.

Главные правила (полная версия — в AGENTS.md в корне):

1. **Отвечай и пиши тексты на русском.** Код — на английском.
2. **Отчёты** — по стандартам `templates/REPORT_STANDARDS.md`: по каждой
   задаче отдельный файл `Laboratornyiy praktikum N_Задача M_отчет.docx`
   (+ pdf), TNR 14 / 1,5 интервала / по ширине / отступ 1,25 см, титульник
   Синергии, полужирные подписи рисунков и таблиц, ссылки на них в тексте.
3. **R выполняется в контейнере**: `docker compose run --rm rstudio bash -lc "…"`
   (RStudio: http://localhost:8787). На хосте R нет.
4. **Сеть в контейнерах**: p3m.dev и cloud.r-project.org часто недоступны —
   использовать зеркала `cran.ma.imperial.ac.uk` и др. (см. docker/install_pkgs.R).
5. **Не коммитить**: `.env`, `docker/build.log`, `__pycache__/`, `Rplots.pdf`.
6. Шаблоны docx (`reference.docx`, `report_template.docx`) генерируются
   `python templates/build_templates.py` — руками не редактировать.
