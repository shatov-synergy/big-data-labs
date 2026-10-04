@AGENTS.md

Дополнительно для Claude Code:

- Проектные скиллы лежат в `.agents/skills/` (`new-lab`, `render-reports`,
  `synergy-report`) — используй их сценарии при соответствующих задачах.
- Единый источник стандартов оформления отчётов —
  `templates/REPORT_STANDARDS.md`; python-API стилей —
  `templates/synergy_docx.py` (класс `SynergyReport`).
- Отчёты рендерятся ТОЛЬКО внутри Docker-контейнера проекта
  (`docker compose run --rm rstudio …`), не на хосте.
