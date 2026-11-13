#!/usr/bin/env python3
"""Разрывы страниц только для структурных элементов (требование преподавателя).

По умолчанию стиль Heading 1 в reference.docx начинает каждый раздел
с новой страницы. Для практикума 2 преподаватель просил сплошную вёрстку
без пустого места: нумерованные разделы 1, 2, 3… идут потоком, а
структурные элементы (ВВЕДЕНИЕ, ВЫВОДЫ, СПИСОК ИСПОЛЬЗОВАННЫХ
ИСТОЧНИКОВ) остаются каждый с новой страницы.

Скрипт запускается НАД УЖЕ ОТРЕНДЕРЕННЫМ docx (после pandoc, до
templates/apply_title_page.py) и ставит явный page_break_before только
на заголовки структурных элементов:

    python3 labs/lab-02/structural_breaks.py output/zadacha1_report.docx

Разрыв у нумерованных разделов отключён на уровне стиля: в
labs/lab-02/reference.docx у Heading 1 стоит pageBreakBefore=0
(в отличие от общего templates/reference.docx). Если шаблоны
перегенерированы (`python3 templates/build_templates.py` копирует
общий reference.docx в labs/lab-02/), восстановить вариант практикума 2:

    python3 -c "from docx import Document; \
        d = Document('labs/lab-02/reference.docx'); \
        d.styles['Heading 1'].paragraph_format.page_break_before = False; \
        d.save('labs/lab-02/reference.docx')"
"""
import argparse

from docx import Document

# Заголовки, которые остаются на новой странице (заглавными, как в Rmd)
STRUCTURAL = {
    "ВВЕДЕНИЕ",
    "ВЫВОДЫ",
    "ЗАКЛЮЧЕНИЕ",
    "СПИСОК ИСПОЛЬЗОВАННЫХ ИСТОЧНИКОВ",
}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("docx")
    args = ap.parse_args()

    doc = Document(args.docx)
    done = []
    for p in doc.paragraphs:
        style = p.style.name if p.style is not None else ""
        if style != "Heading 1":
            continue
        text = p.text.strip().upper()
        if text in STRUCTURAL:
            p.paragraph_format.page_break_before = True
            done.append(p.text.strip())
    doc.save(args.docx)
    print("OK:", args.docx, "| разрывов поставлено:", len(done), "|", "; ".join(done))


if __name__ == "__main__":
    main()
