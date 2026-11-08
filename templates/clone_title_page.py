#!/usr/bin/env python3
"""Клонировать титульный лист из эталонного отчёта в новый docx.

Титульный лист (элементы тела до абзаца «СОДЕРЖАНИЕ») берётся из
сданного образца — там форма ВКР с реальными данными обучающегося —
и вставляется в начало указанного файла. Номер работы и тема
заменяются на новые.

Пример:
  python templates/clone_title_page.py output/otchet.docx \
      --from "labs/lab-01/itog/Laboratornyiy praktikum 1_Задача 1_отчет.docx" \
      --old-lab 1 --new-lab 3 \
      --old-topic "Задача 1. Разведочный анализ. Базовая визуализация данных" \
      --new-topic "Методы очистки и предобработки данных"
"""
import argparse
import copy

from docx import Document
from docx.oxml.ns import qn


def body_elements_until_toc(doc):
    """Элементы тела (w:p, w:tbl) до абзаца СОДЕРЖАНИЕ (не включая)."""
    els = []
    for el in doc.element.body:
        if el.tag == qn("w:sectPr"):
            continue
        if el.tag == qn("w:p"):
            texts = el.findall(".//" + qn("w:t"))
            joined = "".join(t.text or "" for t in texts)
            if "СОДЕРЖАНИЕ" in joined:
                break
        els.append(el)
    return els


def patch_texts(elements, replacements):
    """Заменить текст в w:t-узлах (узлы с полным совпадением текста)."""
    n = 0
    for el in elements:
        for t in el.findall(".//" + qn("w:t")):
            txt = t.text or ""
            for old, new in replacements:
                if old in txt:
                    t.text = txt.replace(old, new)
                    n += 1
    return n


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("docx", help="целевой файл (титул вставляется в начало)")
    ap.add_argument("--from", dest="src", required=True,
                    help="эталонный docx с титульным листом")
    ap.add_argument("--old-lab", required=True)
    ap.add_argument("--new-lab", required=True)
    ap.add_argument("--old-topic", required=True)
    ap.add_argument("--new-topic", required=True)
    args = ap.parse_args()

    src = Document(args.src)
    title_els = body_elements_until_toc(src)
    if not title_els:
        raise SystemExit("в эталоне не найден титульный лист (нет СОДЕРЖАНИЯ)")

    copies = [copy.deepcopy(el) for el in title_els]
    n = patch_texts(copies, [
        (f"№ {args.old_lab}", f"№ {args.new_lab}"),
        (args.old_topic, args.new_topic),
    ])

    dst = Document(args.docx)
    body = dst.element.body
    first = None
    for el in body:
        if el.tag.endswith("}p") or el.tag.endswith("}tbl"):
            first = el
            break
    if first is None:
        raise SystemExit("целевой документ пуст")
    anchor = first.getprevious()
    for el in copies:
        if anchor is None:
            body.insert(0, el)
        else:
            anchor.addnext(el)
            anchor = el

    dst.save(args.docx)
    print("OK:", args.docx, "| скопировано элементов титула:",
          len(copies), "| текстовых замен:", n)


if __name__ == "__main__":
    main()
