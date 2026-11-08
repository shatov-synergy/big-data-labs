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


def _first_heading_element(doc):
    for el in doc.element.body:
        if not el.tag.endswith("}p"):
            continue
        p_pr = el.find(qn("w:pPr"))
        if p_pr is None:
            continue
        ps = p_pr.find(qn("w:pStyle"))
        if ps is None:
            continue
        if (ps.get(qn("w:val")) or "").startswith("Heading"):
            return el
    return None


def _remove_toc_block(doc):
    """Удаляет статический или полевой блок СОДЕРЖАНИЕ до первого заголовка."""
    body = doc.element.body
    children = list(body)
    start = None
    for i, el in enumerate(children):
        if not el.tag.endswith("}p"):
            continue
        text = "".join(t.text or "" for t in el.findall(".//" + qn("w:t"))).strip()
        if text == TOC_TITLE:
            start = i
            break
    if start is None:
        return 0
    removed = 0
    for el in children[start:]:
        if el.tag.endswith("}p"):
            p_pr = el.find(qn("w:pPr"))
            ps = p_pr.find(qn("w:pStyle")) if p_pr is not None else None
            sid = ps.get(qn("w:val")) if ps is not None else ""
            text = "".join(t.text or "" for t in el.findall(".//" + qn("w:t"))).strip()
            if sid.startswith("Heading") and text != TOC_TITLE:
                break
        elif el.tag.endswith("}tbl"):
            break
        body.remove(el)
        removed += 1
    return removed


def _detached(doc):
    p = doc.add_paragraph()
    el = p._p
    el.getparent().remove(el)
    return p


def _fld(paragraph, kind, instr=None, dirty=False):
    run = paragraph.add_run()
    if kind == "instr":
        el = OxmlElement("w:instrText")
        el.set(qn("xml:space"), "preserve")
        el.text = instr
        run._r.append(el)
        return
    el = OxmlElement("w:fldChar")
    el.set(qn("w:fldCharType"), kind)
    if dirty and kind == "begin":
        el.set(qn("w:dirty"), "true")
    run._r.append(el)


def _bookmark_heading(paragraph, name, bid):
    """Закладка на заголовок — цель PAGEREF поля оглавления."""
    p = paragraph._element
    for old in list(p.findall(qn("w:bookmarkStart"))):
        if (old.get(qn("w:name")) or "").startswith("_Toc"):
            bid_old = old.get(qn("w:id"))
            p.remove(old)
            for end in list(p.findall(qn("w:bookmarkEnd"))):
                if end.get(qn("w:id")) == bid_old:
                    p.remove(end)
    start = OxmlElement("w:bookmarkStart")
    start.set(qn("w:id"), str(bid))
    start.set(qn("w:name"), name)
    end = OxmlElement("w:bookmarkEnd")
    end.set(qn("w:id"), str(bid))
    p_pr = p.find(qn("w:pPr"))
    if p_pr is not None:
        p_pr.addnext(start)
    else:
        p.insert(0, start)
    p.append(end)


def _mark_headings(doc):
    n = 0
    for p in doc.paragraphs:
        if (p.style.name or "") not in ("Heading 1", "Heading 2"):
            continue
        if not p.text.strip():
            continue
        n += 1
        _bookmark_heading(p, "_Toc%d" % n, 3000 + n)
    return n


def insert_toc(doc, first_element=None):
    """Поле TOC: при открытии в Word пересобирается по заголовкам 1–2.

    Кэш страниц заполняется отдельно (update_toc_from_pdf), чтобы PDF
    сразу показывал верные номера. Новые разделы и сдвиг страниц из‑за
    таблиц подхватывает обновление полей.
    """
    sd.ensure_toc_styles(doc)
    sd.enable_update_fields(doc)
    _remove_toc_block(doc)
    _mark_headings(doc)
    if first_element is None or first_element.getparent() is None:
        first_element = _first_heading_element(doc)
    if first_element is None:
        return 0

    page_break = first_element.getprevious() is not None
    title = _detached(doc)
    if page_break:
        br = OxmlElement("w:pageBreakBefore")
        title._p.get_or_add_pPr().append(br)
    title.alignment = WD_ALIGN_PARAGRAPH.CENTER
    title.paragraph_format.first_line_indent = Cm(0)
    title.paragraph_format.line_spacing = 1.0
    title.paragraph_format.space_after = Pt(12)
    run = title.add_run(TOC_TITLE)
    run.bold = True
    run.font.name = sd.TNR
    run.font.size = Pt(14)

    begin = _detached(doc)
    _fld(begin, "begin", dirty=True)
    _fld(begin, "instr", ' TOC \\o "1-2" \\h \\z \\u ')
    _fld(begin, "separate")

    entries = []
    for level, text in _headings(doc):
        p = _detached(doc)
        try:
            p.style = doc.styles["toc %d" % level]
        except KeyError:
            p.style = doc.styles["TOC %d" % level]
        p.paragraph_format.first_line_indent = Cm(0)
        r = p.add_run(text)
        r.bold = level == 1
        r.font.name = sd.TNR
        r.font.size = Pt(14)
        tab = p.add_run("\t")
        tab.font.name = sd.TNR
        tab.font.size = Pt(14)
        num = p.add_run("0")
        num.font.name = sd.TNR
        num.font.size = Pt(14)
        if level == 1:
            num.bold = True
        entries.append(p)

    end = _detached(doc)
    _fld(end, "end")

    for p in [title, begin] + entries + [end]:
        first_element.addprevious(p._p)
    return len(entries)


def refresh_auto_toc(doc):
    """Пересобрать блок СОДЕРЖАНИЕ как поле TOC."""
    return insert_toc(doc)


# ------------------------------------------------------------------
# 4. Обновление номеров страниц в СОДЕРЖАНИИ по PDF
# ------------------------------------------------------------------

def _norm(s):
    return re.sub(r"\s+", " ", s).strip().lower()


def _toc_pages(pages, titles):
    """Страницы самого оглавления не считать местом заголовка."""
    skip = set()
    norms = [_norm(t) for t in titles]
    for i, ptext in enumerate(pages, start=1):
        if "содержание" in ptext and i <= 4:
            skip.add(i)
            continue
        lines = [_norm(x) for x in re.split(r"[\n\r]+", ptext) if _norm(x)]
        if len(lines) < 3:
            continue
        hits = 0
        for line in lines:
            core = re.sub(r"[\s\.·…]*\d+\s*$", "", line).strip()
            for t in norms:
                if core == t or (len(core) > 8 and (t.startswith(core) or core.startswith(t))):
                    hits += 1
                    break
        if hits >= 3 and hits >= len(lines) // 2:
            skip.add(i)
    return skip


def _line_is_heading(line, title):
    if line == title:
        return True
    if line.startswith(title + " ") or line.startswith(title + "\t"):
        return True
    if line.startswith(title) and len(line) <= len(title) + 60:
        return True
    return False


def update_toc_from_pdf(doc, pdf_path):
    """Проставить номера в кэше поля TOC.

    Совпадение только с начала строки заголовка: слово «выводы» внутри
    абзаца не считается заголовком ВЫВОДЫ.
    """
    from pypdf import PdfReader
    raw_pages = [p.extract_text() or "" for p in PdfReader(pdf_path).pages]
    page_lines = []
    for text in raw_pages:
        page_lines.append([_norm(x) for x in re.split(r"[\n\r]+", text) if _norm(x)])
    flat = [" ".join(lines) for lines in page_lines]

    entries = []
    seen = False
    for p in doc.paragraphs:
        if not seen:
            if p.text.strip() == TOC_TITLE:
                seen = True
            continue
        sid = p.style.style_id if p.style is not None else ""
        name = p.style.name or ""
        if name.startswith("Heading") or sid.startswith("Heading"):
            break
        if sid not in ("TOC1", "TOC2") and name not in ("toc 1", "toc 2"):
            continue
        title = re.sub(r"\s+\d+\s*$", "", re.sub(r"\s+", " ", p.text)).strip()
        if title:
            entries.append((p, title))

    skip = _toc_pages(flat, [t for _, t in entries])

    def find_pages(title):
        """Все страницы-кандидаты заголовка (перенос строки в абзаце
        может дать ложное начало «выводы ...» — поэтому кандидаты
        фильтруются монотонностью ниже)."""
        t = _norm(title)
        out = []
        for i, lines in enumerate(page_lines, start=1):
            if i in skip:
                continue
            for j, line in enumerate(lines):
                if _line_is_heading(line, t):
                    out.append(i)
                    break
                if j + 1 < len(lines):
                    joined = (line + " " + lines[j + 1]).strip()
                    if joined == t or joined.startswith(t + " "):
                        out.append(i)
                        break
        return out

    # Заголовки в документе идут по порядку: каждому пункту оглавления
    # назначается наименьшая страница-кандидат не меньше страницы
    # предыдущего пункта (ложные совпадения слов из середины абзацев
    # отбрасываются автоматически).
    updated = 0
    last_page = 0
    for p, title in entries:
        candidates = find_pages(title)
        page = next((c for c in candidates if c >= last_page), None)
        if page is None and candidates:
            page = candidates[0]
        if page is None:
            continue
        last_page = page
        texts = p._element.findall(".//" + qn("w:t"))
        for node in reversed(texts):
            if (node.text or "").strip().isdigit():
                node.text = str(page)
                updated += 1
                break
    return updated


# ------------------------------------------------------------------
# 5. Нумерация страниц в футере
# ------------------------------------------------------------------

def add_page_numbers(doc, blank_first=True):
    section = doc.sections[0]
    section.different_first_page_header_footer = blank_first

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

    if not blank_first:
        return
    # первый (титульный) футер пустой
    first = section.first_page_footer
    first.is_linked_to_previous = False
    p = first.paragraphs[0] if first.paragraphs else first.add_paragraph()
    for r in list(p.runs):
        r._r.getparent().remove(r._r)


def enforce_alignment(doc):
    """Прямое форматирование абзацев основного текста (по ширине,
    красная строка 1,25, полуторный интервал) — независимо от разрешения
    стилей в разных офисных пакетах."""
    n = 0
    for p in doc.paragraphs:
        sid = p.style.style_id if p.style is not None else ""
        if sid in ("BodyText", "FirstParagraph") and p.text.strip():
            pf = p.paragraph_format
            pf.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
            pf.first_line_indent = Cm(1.25)
            pf.line_spacing = 1.5
            n += 1
    return n


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
    ap.add_argument("--no-title", action="store_true",
                    help="не вставлять титульный лист (уже есть в файле)")
    ap.add_argument("--update-toc", metavar="PDF",
                    help="проставить номера страниц в СОДЕРЖАНИИ по PDF")
    args = ap.parse_args()

    doc = Document(args.docx)

    if args.update_toc:
        n = update_toc_from_pdf(doc, args.update_toc)
        doc.save(args.docx)
        print("OK:", args.docx, "| обновлено пунктов СОДЕРЖАНИЕ:", n)
        return

    body = doc.element.body
    if not args.no_title:
        elements = build_title_elements(args)
        for el in reversed(elements):
            body.insert(0, el)

    n_img = center_images(doc)
    add_page_numbers(doc)
    n_san = sd.sanitize_fonts(doc)
    n_al = enforce_alignment(doc)
    gost = sd.enforce_gost_body(doc)

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
        else:
            refresh_auto_toc(doc)

    doc.save(args.docx)
    print("OK:", args.docx, "| картинок отцентрировано:", n_img,
          "| стилей санитизировано:", n_san,
          "| абзацев выровнено:", n_al,
          "| таблиц ГОСТ:", gost["tables"],
          "| код:", gost["code"], "| вывод R:", gost["output"],
          "| СОДЕРЖАНИЕ:", "да" if args.toc else "нет")


if __name__ == "__main__":
    main()
