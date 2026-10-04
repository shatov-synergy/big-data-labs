# -*- coding: utf-8 -*-
"""
build_templates.py — генерация docx-шаблонов в стандартах «Синергии».

Создаёт:
  1. templates/report_template.docx — заготовка отчёта с титульным листом
     (для ручного заполнения в Word или программной генерации).
  2. templates/reference.docx — pandoc-reference-документ со стилями
     «Синергии» (для вывода R Markdown -> docx, параметр reference_docx).
  3. labs/lab-01/reference.docx — копия reference.docx рядом с отчётом лаб. 1.

Запуск:  python templates/build_templates.py
"""

import os
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from synergy_docx import new_document, add_title_page  # noqa: E402

BASE = os.path.dirname(os.path.abspath(__file__))


def build_report_template():
    doc = new_document()
    add_title_page(
        doc,
        discipline="{ДИСЦИПЛИНА}",
        topic="{ТЕМА}",
        lab_number="{N}",
        student="{ФИО ОБУЧАЮЩЕГОСЯ}",
        group="{ГРУППА}",
        teacher="{ФИО ПРЕПОДАВАТЕЛЯ}",
        year="{ГОД}",
    )
    doc.add_heading("1 {Название первого раздела}", level=1)
    doc.add_paragraph("{Текст отчёта. Times New Roman 14, полуторный "
                      "интервал, по ширине, красная строка 1,25 см.}")
    path = os.path.join(BASE, "report_template.docx")
    doc.save(path)
    print("OK:", path)


def build_reference():
    doc = new_document()
    doc.add_paragraph("")  # pandoc игнорирует содержимое, важны только стили
    path = os.path.join(BASE, "reference.docx")
    doc.save(path)
    print("OK:", path)

    # копия рядом с Rmd лабораторной работы 1
    lab_ref = os.path.join(BASE, "..", "labs", "lab-01", "reference.docx")
    shutil.copyfile(path, os.path.normpath(lab_ref))
    print("OK:", os.path.normpath(lab_ref))


if __name__ == "__main__":
    build_report_template()
    build_reference()
