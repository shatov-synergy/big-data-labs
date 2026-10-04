# -*- coding: utf-8 -*-
"""
apply_title_page.py — финализация docx-отчёта по ГОСТ 7.32-2017:

  1) вставка титульного листа (структурный клон ВКР) в начало документа;
  2) центрирование всех абзацев с изображениями;
  3) (--toc) вставка страницы СОДЕРЖАНИЕ после титульного листа
     (заголовки берутся из стилей Heading 1/2; номера страниц —
     заполнители, обновляются шагом 4);
  4) (--update-toc <pdf>) проставление фактических номеров страниц
     в СОДЕРЖАНИИ по сконвертированному PDF;
  5) нумерация страниц: PAGE-поле по центру футера, титульный лист
     без номера (titlePg), счёт с 1.

Типовой цикл (см. скилл render-reports):

  python templates/apply_title_page.py out.docx --lab 1 --toc --topic "..."
  # docx -> pdf, затем:
  python templates/apply_title_page.py out.docx --lab 1 --update-toc out.pdf --topic "..."
  # docx -> pdf ещё раз
"""

import argparse
import copy
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from docx import Document  # noqa: E402
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_TAB_ALIGNMENT, \
    WD_TAB_LEADER  # noqa: E402
from docx.oxml import OxmlElement  # noqa: E402
from docx.oxml.ns import qn  # noqa: E402
from docx.shared import Cm, Pt  # noqa: E402

import synergy_docx as sd  # noqa: E402

TOC_TITLE = "СОДЕРЖАНИЕ"


# ------------------------------------------------------------------
# 1-2. Титульный лист и центрирование картинок
# ------------------------------------------------------------------

def build_title_elements(args):
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
            if el.tag.endswith("}p") or el.tag.endswith("}tbl")]


def center_images(doc):
    n = 0
    for p in doc.paragraphs:
        if p._p.findall(".//" + qn("w:drawing")):
            ppr = p._p.get_or_add_pPr()
            for tag in ("w:jc", "w:ind"):
                for el in ppr.findall(qn(tag)):
                    ppr.remove(el)
            jc = ppr.makeelement(qn("w:jc"), {})
            jc.set(qn("w:val"), "center")
            ppr.append(jc)
            n += 1
    return n


# ------------------------------------------------------------------
# 3. Страница СОДЕРЖАНИЕ
# ------------------------------------------------------------------

def _headings(doc):
    """[(level, text)] по стилям Heading 1/2."""
    out = []
    for p in doc.paragraphs:
        name = (p.style.name or "")
        if name in ("Heading 1", "Heading 2") and p.text.strip():
            out.append((1 if name == "Heading 1" else 2,
                        re.sub(r"\s+", " ", p.text.strip())))
    return out


def _toc_paragraph(doc, level, text, page="0"):
    p = doc.add_paragraph()
    pf = p.paragraph_format
    pf.first_line_indent = Cm(0)
    pf.line_spacing = 1.5
    pf.space_after = Pt(0)
    if level == 2:
        pf.left_indent = Cm(0.75)
    pf.tab_stops.add_tab_stop(Cm(16.4), WD_TAB_ALIGNMENT.RIGHT,
                              WD_TAB_LEADER.DOTS)
    r = p.add_run(text)
    r.font.name = sd.TNR
    r.font.size = Pt(14)
    if level == 1:
        r.bold = True
    r2 = p.add_run("\t" + page)
    r2.font.name = sd.TNR
    r2.font.size = Pt(14)
    if level == 1:
        r2.bold = True
    return p


def insert_toc(doc, first_element):
    """Вставляет СОДЕРЖАНИЕ (титул + пункты) перед first_element."""
    body = doc.element.body

    def _new_para(insert_before, text="", bold=False, size=14,
                  align=WD_ALIGN_PARAGRAPH.CENTER, page_break=False,
                  after=Pt(12)):
        p = doc.add_paragraph()
        ppr = p._p.get_or_add_pPr()
        if page_break:
            br = OxmlElement("w:pageBreakBefore")
            ppr.append(br)
        el = copy.deepcopy(p._p)
        body.remove(p._p)
        first_element.addprevious(el)
        from docx.text.paragraph import Paragraph
        para = Paragraph(el, doc._body)
        para.alignment = align
        para.paragraph_format.first_line_indent = Cm(0)
        para.paragraph_format.line_spacing = 1.0
        para.paragraph_format.space_after = after
        if text:
            run = para.add_run(text)
            run.bold = bold
            run.font.name = sd.TNR
            run.font.size = Pt(size)
        return para

    _new_para(first_element, TOC_TITLE, bold=True, page_break=True)
    for level, text in _headings(doc):
        entry = _toc_paragraph(doc, level, text)
        el = copy.deepcopy(entry._p)
        entry._p.getparent().remove(entry._p)
        first_element.addprevious(el)


# ------------------------------------------------------------------
# 4. Обновление номеров страниц в СОДЕРЖАНИИ по PDF
# ------------------------------------------------------------------

def _norm(s):
    return re.sub(r"\s+", " ", s).strip().lower()


def update_toc_from_pdf(doc, pdf_path):
    from pypdf import PdfReader
    pages = [_norm(p.extract_text() or "") for p in PdfReader(pdf_path).pages]

    # страница СОДЕРЖАНИЕ: её саму и всё до неё исключаем из поиска
    toc_page = 0
    for i, ptext in enumerate(pages, start=1):
        if "содержание" in ptext:
            toc_page = i
            break

    def find_page(title):
        t = _norm(title)
        # 1) заголовок отдельной строкой (точное совпадение)
        for i, ptext in enumerate(pages, start=1):
            if i <= toc_page:
                continue
            lines = [_norm(x) for x in re.split(r"[\n\r]+", ptext)]
            if t in lines:
                return i
        # 2) подстрока (на случай переноса длинного заголовка)
        for i, ptext in enumerate(pages, start=1):
            if i <= toc_page:
                continue
            if t in ptext:
                return i
        return None

    state = {"seen_title": False, "updated": 0}
    for p in doc.paragraphs:
        if not state["seen_title"]:
            if p.text.strip() == TOC_TITLE:
                state["seen_title"] = True
            continue
        name = p.style.name or ""
        if name.startswith("Heading"):
            break
        entry = re.sub(r"\s+", " ", p.text.strip())
        m = re.match(r"^(.*?)(\d+)$", entry)
        if not m or not m.group(1).strip():
            continue
        title = m.group(1).strip()
        page = find_page(title)
        if page is None:
            continue
        runs = p.runs
        if runs and runs[-1].text.lstrip("\t").isdigit():
            runs[-1].text = "\t" + str(page)
            state["updated"] += 1
    return state["updated"]


# ------------------------------------------------------------------
# 5. Нумерация страниц в футере
# ------------------------------------------------------------------

def add_page_numbers(doc):
    section = doc.sections[0]
    section.different_first_page_header_footer = True

    # pgNumType start=1
    sect_pr = section._sectPr
    for el in sect_pr.findall(qn("w:pgNumType")):
        sect_pr.remove(el)
    pg = OxmlElement("w:pgNumType")
    pg.set(qn("w:start"), "1")
    sect_pr.append(pg)

    # обычный футер: PAGE по центру
    footer = section.footer
    footer.is_linked_to_previous = False
    p = footer.paragraphs[0] if footer.paragraphs else footer.add_paragraph()
    for r in list(p.runs):
        r._r.getparent().remove(r._r)
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run()
    run.font.name = sd.TNR
    run.font.size = Pt(14)
    f1 = OxmlElement("w:fldChar")
    f1.set(qn("w:fldCharType"), "begin")
    instr = OxmlElement("w:instrText")
    instr.set(qn("xml:space"), "preserve")
    instr.text = " PAGE "
    f2 = OxmlElement("w:fldChar")
    f2.set(qn("w:fldCharType"), "end")
    run._r.append(f1)
    run._r.append(instr)
    run._r.append(f2)

    # первый (титульный) футер пустой
    first = section.first_page_footer
    first.is_linked_to_previous = False
    p = first.paragraphs[0] if first.paragraphs else first.add_paragraph()
    for r in list(p.runs):
        r._r.getparent().remove(r._r)


# ------------------------------------------------------------------

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
    ap.add_argument("--toc", action="store_true",
                    help="вставить страницу СОДЕРЖАНИЕ после титульного")
    ap.add_argument("--update-toc", metavar="PDF",
                    help="проставить номера страниц в СОДЕРЖАНИИ по PDF")
    args = ap.parse_args()

    doc = Document(args.docx)

    if args.update_toc:
        n = update_toc_from_pdf(doc, args.update_toc)
        doc.save(args.docx)
        print("OK:", args.docx, "| обновлено пунктов СОДЕРЖАНИЕ:", n)
        return

    elements = build_title_elements(args)
    body = doc.element.body
    for el in reversed(elements):
        body.insert(0, el)

    n_img = center_images(doc)
    add_page_numbers(doc)

    if args.toc:
        # СОДЕРЖАНИЕ вставляется ПЕРЕД первым элементом тела
        # (первый Heading — ВВЕДЕНИЕ)
        first_el = None
        for el in body:
            if el.tag.endswith("}p"):
                from docx.text.paragraph import Paragraph
                p = Paragraph(el, doc._body)
                if (p.style.name or "").startswith("Heading"):
                    first_el = el
                    break
        if first_el is not None:
            insert_toc(doc, first_el)

    doc.save(args.docx)
    print("OK:", args.docx, "| картинок отцентрировано:", n_img,
          "| СОДЕРЖАНИЕ:", "да" if args.toc else "нет")


if __name__ == "__main__":
    main()
