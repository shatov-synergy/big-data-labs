# -*- coding: utf-8 -*-
"""
apply_title_page.py — вставить титульный лист (клон ВКР) в начало
docx-отчёта, отрендеренного из R Markdown (без титульного блока).

Использование:
  python templates/apply_title_page.py <файл.docx> \
      --lab 1 --task 1 \
      --topic "Задача 1. Разведочный анализ. Базовая визуализация данных" \
      --discipline "Продвинутые технологии работы с большими данными" \
      [--group ВБИо-403-рсоб] [--teacher "Иванов И. И."] [--year 2026]

Скрипт открывает docx, собирает титульный лист структуру как в ВКР
(шаблон из synergy_docx.add_title_page) и переносит его элементы
в начало тела документа. Первый заголовок отчёта имеет свойство
«с новой страницы», поэтому отдельный разрыв не добавляется.
"""

import argparse
import copy
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from docx import Document  # noqa: E402

import synergy_docx as sd  # noqa: E402


def build_title_elements(args):
    """Титульный лист во временном документе -> список body-элементов."""
    tmp = sd.new_document()
    sd.add_title_page(
        tmp,
        discipline=args.discipline,
        topic=args.topic,
        lab_number=args.lab,
        student=args.student,
        group=args.group,
        teacher=args.teacher,
        year=args.year,
    )
    return [copy.deepcopy(el) for el in tmp.element.body
            if el.tag.endswith('}p') or el.tag.endswith('}tbl')]


def center_images(doc):
    """Центрирует каждый абзац с картинкой и убирает красную строку —
    гарантированно, независимо от того, какой стиль применил pandoc."""
    from docx.oxml.ns import qn as _qn
    n = 0
    for p in doc.paragraphs:
        if p._p.findall(".//" + _qn("w:drawing")):
            p.alignment = None  # сброс; ниже явный jc=center
            ppr = p._p.get_or_add_pPr()
            for tag in ("w:jc", "w:ind"):
                for el in ppr.findall(_qn(tag)):
                    ppr.remove(el)
            jc = ppr.makeelement(_qn("w:jc"), {})
            jc.set(_qn("w:val"), "center")
            ppr.append(jc)
            n += 1
    return n


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("docx")
    ap.add_argument("--lab", required=True)
    ap.add_argument("--topic", required=True)
    ap.add_argument("--discipline",
                    default="Продвинутые технологии работы с большими "
                            "данными")
    ap.add_argument("--student", default="Шатов Илья Андреевич")
    ap.add_argument("--group", default="ВБИо-403-рсоб")
    ap.add_argument("--teacher", default="")
    ap.add_argument("--year", type=int, default=2026)
    args = ap.parse_args()

    doc = Document(args.docx)
    elements = build_title_elements(args)
    body = doc.element.body
    for el in reversed(elements):
        body.insert(0, el)
    n_img = center_images(doc)
    doc.save(args.docx)
    print("OK:", args.docx, "| картинок отцентрировано:", n_img)


if __name__ == "__main__":
    main()
