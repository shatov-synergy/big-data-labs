# -*- coding: utf-8 -*-
"""
synergy_docx.py — единый модуль стандартов оформления отчётов
Университет «Синергия», лабораторные работы.

Стандарты воспроизводят «1 в 1» оформление утверждённых документов
обучающегося (отчёт по практике, ВКР) и требования методички дисциплины
(ГОСТ 7.32-2017):

  Страница     A4 портрет; поля: верх 2 см, низ 2 см, лево 3 см, право 1,5 см
               (как в ВКР; колонтитулы 1,25 см).
  Обычный текст Times New Roman 14 пт, полуторный интервал (1.5),
               выравнивание по ширине, красная строка 1,25 см (ГОСТ/методичка).
  Заголовок 1  Times New Roman 16 пт, полужирный, по центру, с новой страницы,
               одинарный интервал, без красной строки.
  Заголовок 2  Times New Roman 14 пт, полужирный, по центру, одинарный интервал.
  Заголовок 3  Times New Roman 14 пт, полужирный, слева.
  Рисунки      по центру; подпись ПОД рисунком: «Рисунок N. Название» —
               по центру, полужирно, TNR 14, одинарный интервал (как в ВКР).
  Таблицы      «Таблица N» — отдельной строкой СПРАВА, полужирно; следующей
               строкой название по центру полужирно с точкой в конце;
               границы всех ячеек одинарные; шапка полужирная по центру;
               в тексте обязательны ссылки «(рисунок N)», «(таблица N)».
  Код          Courier New 10 пт, одинарный, фон #F2F2F2.
  Вывод R      Courier New 10 пт, одинарный, без фона.
  Титульный лист — форма Синергии: метки полужирные, значения ПОДЧЁРКНУТЫЕ,
               подсказки «(наименование ...)» 9 пт по центру, внизу «Москва N г.»
"""

from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_BREAK
from docx.enum.style import WD_STYLE_TYPE
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.oxml.ns import qn
from docx.oxml import OxmlElement

TNR = "Times New Roman"
MONO = "Courier New"

# ------------------------------------------------------------------
# Базовая настройка документа
# ------------------------------------------------------------------

def _set_style_font(style, font_name, size_pt, bold=None, italic=None,
                    color=None, underline=None):
    """Прописывает шрифт во всех слотах rFonts и УБИРАЕТ ссылки на тему —
    иначе Word подменяет шрифт Calibri (majorHAnsi/minorHAnsi)."""
    style.font.name = font_name
    style.font.size = Pt(size_pt)
    if bold is not None:
        style.font.bold = bold
    if italic is not None:
        style.font.italic = italic
    if color is not None:
        style.font.color.rgb = RGBColor(*color)
    if underline is not None:
        style.font.underline = underline
    r_pr = style.element.get_or_add_rPr()
    r_fonts = r_pr.find(qn("w:rFonts"))
    if r_fonts is None:
        r_fonts = OxmlElement("w:rFonts")
        r_pr.append(r_fonts)
    for attr in ("w:asciiTheme", "w:hAnsiTheme", "w:cstheme", "w:eastAsiaTheme"):
        if r_fonts.get(qn(attr)) is not None:
            del r_fonts.attrib[qn(attr)]
    for attr in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
        r_fonts.set(qn(attr), font_name)


def _set_doc_defaults(doc, font_name=TNR, size_pt=14):
    """docDefaults документа: Times New Roman, 14 пт — шрифт по умолчанию
    для любых прогонов без явного форматирования."""
    styles_el = doc.styles.element
    dd = styles_el.find(qn("w:docDefaults"))
    if dd is None:
        dd = OxmlElement("w:docDefaults")
        styles_el.insert(0, dd)
    rprd = dd.find(qn("w:rPrDefault"))
    if rprd is None:
        rprd = OxmlElement("w:rPrDefault")
        dd.append(rprd)
    rpr = rprd.find(qn("w:rPr"))
    if rpr is None:
        rpr = OxmlElement("w:rPr")
        rprd.append(rpr)
    r_fonts = rpr.find(qn("w:rFonts"))
    if r_fonts is None:
        r_fonts = OxmlElement("w:rFonts")
        rpr.insert(0, r_fonts)
    for attr in ("w:asciiTheme", "w:hAnsiTheme", "w:cstheme", "w:eastAsiaTheme"):
        if r_fonts.get(qn(attr)) is not None:
            del r_fonts.attrib[qn(attr)]
    for attr in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
        r_fonts.set(qn(attr), font_name)
    for tag, val in (("w:sz", str(int(size_pt * 2))), ("w:szCs", str(int(size_pt * 2)))):
        el = rpr.find(qn(tag))
        if el is None:
            el = OxmlElement(tag)
            rpr.append(el)
        el.set(qn("w:val"), val)


# стили кода/подсветки синтаксиса (pandoc) — моноширинные
MONO_STYLES = {"Source Code", "R Output", "Verbatim Char", "SourceCode",
               "VerbatimChar"}
# любые символьные стили подсветки pandoc (*Tok) — тоже моноширинные


def _is_mono_style(name):
    return name in MONO_STYLES or name.endswith("Tok")


def sanitize_fonts(doc):
    """Гарантия «никакого Calibri» во всём документе:

    1) каждому абзацному/символьному стилю прописывает явный шрифт
       (Times New Roman; стили кода и подсветки — Courier New)
       и удаляет theme-атрибуты (asciiTheme и пр., ссылающиеся на тему
       со шрифтами Calibri/Cambria);
    2) docDefaults → Times New Roman 14;
    3) в theme1.xml.majorFont/minorFont задаёт Times New Roman
       (страховка для любых оставшихся ссылок на тему).
    """
    _set_doc_defaults(doc, TNR, 14)

    # прямой обход всех w:style в styles.xml (надёжнее итератора python-docx,
    # который пропускает стили без имени)
    n = 0
    for st_el in doc.styles.element.findall(qn("w:style")):
        stype = st_el.get(qn("w:type"))
        if stype not in ("paragraph", "character"):
            continue
        name_el = st_el.find(qn("w:name"))
        name = name_el.get(qn("w:val")) if name_el is not None else ""
        sid = st_el.get(qn("w:styleId")) or ""
        font = MONO if (name in MONO_STYLES or sid in MONO_STYLES
                        or name.endswith("Tok") or sid.endswith("Tok")) else TNR
        rpr = st_el.find(qn("w:rPr"))
        if rpr is None:
            rpr = OxmlElement("w:rPr")
            st_el.append(rpr)
        r_fonts = rpr.find(qn("w:rFonts"))
        if r_fonts is None:
            r_fonts = OxmlElement("w:rFonts")
            rpr.insert(0, r_fonts)
        for attr in ("w:asciiTheme", "w:hAnsiTheme", "w:cstheme",
                     "w:eastAsiaTheme"):
            if r_fonts.get(qn(attr)) is not None:
                del r_fonts.attrib[qn(attr)]
        for attr in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
            r_fonts.set(qn(attr), font)
        n += 1

    # тема документа: major/minor → Times New Roman
    try:
        import re as _re
        for part in doc.part.package.iter_parts():
            if str(part.partname) == "/word/theme/theme1.xml":
                xml = part.blob.decode("utf-8")
                xml = _re.sub(r'(<a:majorFont>\s*<a:latin[^>]*typeface=")[^"]*(")',
                              r"\g<1>Times New Roman\g<2>", xml)
                xml = _re.sub(r'(<a:minorFont>\s*<a:latin[^>]*typeface=")[^"]*(")',
                              r"\g<1>Times New Roman\g<2>", xml)
                part._blob = xml.encode("utf-8")
                break
    except Exception:
        pass

    return n


def _get_or_add_style(doc, name, style_type=WD_STYLE_TYPE.PARAGRAPH, base="Normal"):
    try:
        return doc.styles[name]
    except KeyError:
        st = doc.styles.add_style(name, style_type)
        if base:
            try:
                st.base_style = doc.styles[base]
            except KeyError:
                pass
        return st


def _shade_style(style, fill):
    p_pr = style.element.get_or_add_pPr()
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:fill"), fill)
    p_pr.append(shd)


def _style_borders(style_el, sz="4"):
    """Одинарные границы для стиля таблицы."""
    tbl_pr = style_el.find(qn("w:tblPr"))
    if tbl_pr is None:
        tbl_pr = OxmlElement("w:tblPr")
        style_el.append(tbl_pr)
    borders = OxmlElement("w:tblBorders")
    for edge in ("top", "left", "bottom", "right", "insideH", "insideV"):
        el = OxmlElement("w:" + edge)
        el.set(qn("w:val"), "single")
        el.set(qn("w:sz"), sz)
        el.set(qn("w:space"), "0")
        el.set(qn("w:color"), "auto")
        borders.append(el)
    tbl_pr.append(borders)


def _style_first_row(style_el):
    """Условное форматирование первой строки: полужирно, по центру, 12 пт."""
    cond = OxmlElement("w:tblStylePr")
    cond.set(qn("w:type"), "firstRow")
    r_pr = OxmlElement("w:rPr")
    b = OxmlElement("w:b")
    r_pr.append(b)
    sz = OxmlElement("w:sz")
    sz.set(qn("w:val"), "24")
    r_pr.append(sz)
    cond.append(r_pr)
    p_pr = OxmlElement("w:pPr")
    jc = OxmlElement("w:jc")
    jc.set(qn("w:val"), "center")
    p_pr.append(jc)
    cond.append(p_pr)
    style_el.append(cond)


def configure_page(doc):
    """A4, поля 2/1,5/2/3 см (верх/право/низ/лево), колонтитулы 1,25 см."""
    for section in doc.sections:
        section.page_width = Cm(21.0)
        section.page_height = Cm(29.7)
        section.top_margin = Cm(2.0)
        section.bottom_margin = Cm(2.0)
        section.left_margin = Cm(3.0)
        section.right_margin = Cm(1.5)
        section.header_distance = Cm(1.25)
        section.footer_distance = Cm(1.25)


def configure_styles(doc):
    """Все стили по стандартам Синергии (клон ВКР)."""
    # сначала шрифт документа по умолчанию — Times New Roman 14
    _set_doc_defaults(doc, TNR, 14)

    # --- Normal: TNR 14, 1.5, по ширине, красная строка 1,25 ---
    normal = doc.styles["Normal"]
    _set_style_font(normal, TNR, 14, color=(0, 0, 0))
    pf = normal.paragraph_format
    pf.line_spacing = 1.5
    pf.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
    pf.first_line_indent = Cm(1.25)
    pf.space_before = Pt(0)
    pf.space_after = Pt(0)

    # --- Стили pandoc (совместимость с R Markdown -> docx) ---
    for name in ("Body Text", "First Paragraph"):
        st = _get_or_add_style(doc, name, base="Normal")
        _set_style_font(st, TNR, 14, color=(0, 0, 0))
    # Compact — ячейки kable: 12 пт, одинарный, без красной строки
    compact = _get_or_add_style(doc, "Compact", base="Normal")
    _set_style_font(compact, TNR, 12, color=(0, 0, 0))
    compact.paragraph_format.space_after = Pt(0)
    compact.paragraph_format.space_before = Pt(0)
    compact.paragraph_format.line_spacing = 1.0
    compact.paragraph_format.first_line_indent = Cm(0)
    compact.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT

    # --- Заголовки (как в ВКР: полужирные, по центру, одинарный интервал) ---
    h1 = _get_or_add_style(doc, "Heading 1", base="Normal")
    _set_style_font(h1, TNR, 16, bold=True, color=(0, 0, 0))
    h1.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    h1.paragraph_format.first_line_indent = Cm(0)
    h1.paragraph_format.page_break_before = True
    h1.paragraph_format.keep_with_next = True
    h1.paragraph_format.line_spacing = 1.0
    h1.paragraph_format.space_before = Pt(0)
    h1.paragraph_format.space_after = Pt(12)

    h2 = _get_or_add_style(doc, "Heading 2", base="Normal")
    _set_style_font(h2, TNR, 14, bold=True, color=(0, 0, 0))
    h2.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    h2.paragraph_format.first_line_indent = Cm(0)
    h2.paragraph_format.keep_with_next = True
    h2.paragraph_format.line_spacing = 1.0
    h2.paragraph_format.space_before = Pt(8)
    h2.paragraph_format.space_after = Pt(8)

    h3 = _get_or_add_style(doc, "Heading 3", base="Normal")
    _set_style_font(h3, TNR, 14, bold=True, color=(0, 0, 0))
    h3.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
    h3.paragraph_format.first_line_indent = Cm(0)
    h3.paragraph_format.keep_with_next = True
    h3.paragraph_format.line_spacing = 1.0
    h3.paragraph_format.space_before = Pt(8)
    h3.paragraph_format.space_after = Pt(6)

    # --- Код и вывод R ---
    code = _get_or_add_style(doc, "Source Code", base="Normal")
    _set_style_font(code, MONO, 10, color=(0, 0, 0))
    code.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
    code.paragraph_format.first_line_indent = Cm(0)
    code.paragraph_format.line_spacing = 1.0
    code.paragraph_format.space_after = Pt(0)
    _shade_style(code, "F2F2F2")

    rout = _get_or_add_style(doc, "R Output", base="Normal")
    _set_style_font(rout, MONO, 10, color=(0, 0, 0))
    rout.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
    rout.paragraph_format.first_line_indent = Cm(0)
    rout.paragraph_format.line_spacing = 1.0
    rout.paragraph_format.space_after = Pt(0)

    verbatim = _get_or_add_style(doc, "Verbatim Char", WD_STYLE_TYPE.CHARACTER,
                                 base=None)
    _set_style_font(verbatim, MONO, 10)

    # --- Абзац с картинкой: ПО ЦЕНТРУ, без красной строки (ГОСТ 7.32-2017) ---
    for fig_name in ("Figure", "Captioned Figure"):
        fig = _get_or_add_style(doc, fig_name, base="Normal")
        _set_style_font(fig, TNR, 14, color=(0, 0, 0))
        fig.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
        fig.paragraph_format.first_line_indent = Cm(0)
        fig.paragraph_format.line_spacing = 1.0
        fig.paragraph_format.space_before = Pt(6)
        fig.paragraph_format.space_after = Pt(0)
        fig.paragraph_format.keep_with_next = True

    # --- Подпись рисунка: «Рисунок N — Название», по центру, полужирно ---
    cap = _get_or_add_style(doc, "Image Caption", base="Normal")
    _set_style_font(cap, TNR, 14, bold=True, color=(0, 0, 0))
    cap.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    cap.paragraph_format.first_line_indent = Cm(0)
    cap.paragraph_format.line_spacing = 1.0
    cap.paragraph_format.space_before = Pt(6)
    cap.paragraph_format.space_after = Pt(12)
    cap.paragraph_format.keep_with_next = False

    # --- Подпись таблицы по ГОСТ 7.32-2017: «Таблица N — Название» ---
    # одна строка НАД таблицей, слева, без абзацного отступа, полужирно
    tcap = _get_or_add_style(doc, "Table Caption", base="Normal")
    _set_style_font(tcap, TNR, 14, bold=True, color=(0, 0, 0))
    tcap.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
    tcap.paragraph_format.first_line_indent = Cm(0)
    tcap.paragraph_format.line_spacing = 1.0
    tcap.paragraph_format.space_before = Pt(12)
    tcap.paragraph_format.space_after = Pt(6)
    tcap.paragraph_format.keep_with_next = True

    ensure_toc_styles(doc)

    # --- Стиль таблиц pandoc: одинарные границы, шапка полужирная по центру ---
    try:
        tbl_style = _get_or_add_style(doc, "Table", WD_STYLE_TYPE.TABLE, base=None)
        _style_borders(tbl_style.element)
        _style_first_row(tbl_style.element)
    except Exception:
        pass

    # --- Титульный лист: форма с подчёркнутыми значениями ---
    tp = _get_or_add_style(doc, "TitlePage", base="Normal")
    _set_style_font(tp, TNR, 14, color=(0, 0, 0))
    tp.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    tp.paragraph_format.first_line_indent = Cm(0)
    tp.paragraph_format.line_spacing = 1.0
    tp.paragraph_format.space_after = Pt(0)

    tps = _get_or_add_style(doc, "TitlePageSmall", base="Normal")
    _set_style_font(tps, TNR, 9, color=(0, 0, 0))
    tps.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    tps.paragraph_format.first_line_indent = Cm(0)
    tps.paragraph_format.line_spacing = 1.0
    tps.paragraph_format.space_after = Pt(0)

    # знаковый стиль «подчёркнутое значение формы»
    und = _get_or_add_style(doc, "UnderlineValue", WD_STYLE_TYPE.CHARACTER,
                            base=None)
    _set_style_font(und, TNR, 14, underline=True)


def ensure_toc_styles(doc):
    """Стили поля оглавления: toc 1 / toc 2, точки-лидеры справа."""
    specs = (
        ("toc 1", "TOC1", Cm(0), True),
        ("toc 2", "TOC2", Cm(0.75), False),
    )
    tab_pos = str(int(16.4 / 2.54 * 1440))
    for name, sid, indent, bold in specs:
        st = None
        for key in (sid, name):
            try:
                st = doc.styles[key]
                break
            except KeyError:
                continue
        if st is None:
            st = doc.styles.add_style(name, WD_STYLE_TYPE.PARAGRAPH)
        st.element.set(qn("w:styleId"), sid)
        name_el = st.element.find(qn("w:name"))
        if name_el is not None:
            name_el.set(qn("w:val"), name)
        _set_style_font(st, TNR, 14, bold=bold, color=(0, 0, 0))
        pf = st.paragraph_format
        pf.alignment = WD_ALIGN_PARAGRAPH.LEFT
        pf.first_line_indent = Cm(0)
        pf.left_indent = indent
        pf.line_spacing = 1.5
        pf.space_before = Pt(0)
        pf.space_after = Pt(0)
        p_pr = st.element.get_or_add_pPr()
        for old in list(p_pr.findall(qn("w:tabs"))):
            p_pr.remove(old)
        tabs = OxmlElement("w:tabs")
        tab = OxmlElement("w:tab")
        tab.set(qn("w:val"), "right")
        tab.set(qn("w:leader"), "dot")
        tab.set(qn("w:pos"), tab_pos)
        tabs.append(tab)
        p_pr.append(tabs)
    return True


def enable_update_fields(doc):
    """Word/OnlyOffice пересобирают оглавление при открытии."""
    settings_part = None
    for rel in doc.part.rels.values():
        if "settings" in rel.reltype:
            settings_part = rel.target_part
            break
    if settings_part is None:
        return False
    root = settings_part.element
    if root.find(qn("w:updateFields")) is None:
        uf = OxmlElement("w:updateFields")
        uf.set(qn("w:val"), "true")
        root.append(uf)
    return True


def new_document():
    """Пустой документ со всеми стандартами (страница + стили)."""
    doc = Document()
    configure_page(doc)
    configure_styles(doc)
    sanitize_fonts(doc)
    return doc


# ------------------------------------------------------------------
# Постобработка тела отчёта (после R Markdown -> docx)
# ------------------------------------------------------------------

# ширина набора: A4 − лево 3 см − право 1,5 см = 16,5 см
_BODY_TABLE_WIDTH_CM = 16.5


def _set_run_mono_black(run, size_pt=10):
    """Courier New, чёрный, без цветовой подсветки pandoc."""
    run.font.name = MONO
    run.font.size = Pt(size_pt)
    run.font.color.rgb = RGBColor(0, 0, 0)
    r_pr = run._element.get_or_add_rPr()
    r_fonts = r_pr.find(qn("w:rFonts"))
    if r_fonts is None:
        r_fonts = OxmlElement("w:rFonts")
        r_pr.insert(0, r_fonts)
    for attr in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
        r_fonts.set(qn(attr), MONO)
    color = r_pr.find(qn("w:color"))
    if color is None:
        color = OxmlElement("w:color")
        r_pr.append(color)
    color.set(qn("w:val"), "000000")
    # убрать theme-цвет, если есть
    if color.get(qn("w:themeColor")) is not None:
        del color.attrib[qn("w:themeColor")]


def _clear_para_shading(paragraph):
    p_pr = paragraph._element.get_or_add_pPr()
    for shd in list(p_pr.findall(qn("w:shd"))):
        p_pr.remove(shd)


def _set_para_shading(paragraph, fill="F2F2F2"):
    p_pr = paragraph._element.get_or_add_pPr()
    for shd in list(p_pr.findall(qn("w:shd"))):
        p_pr.remove(shd)
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:fill"), fill)
    p_pr.append(shd)


def _para_text(paragraph):
    return "".join(r.text or "" for r in paragraph.runs)


def _is_r_output_para(paragraph):
    """Вывод knitr: Verbatim Char или строки, начинающиеся с ##."""
    text = _para_text(paragraph).lstrip()
    if text.startswith("##"):
        return True
    for run in paragraph.runs:
        r_pr = run._element.find(qn("w:rPr"))
        if r_pr is None:
            continue
        r_style = r_pr.find(qn("w:rStyle"))
        if r_style is None:
            continue
        sid = r_style.get(qn("w:val")) or ""
        if sid in ("VerbatimChar", "Verbatim Char"):
            return True
    return False


def format_code_blocks(doc):
    """Код: Courier New 10, чёрный, фон #F2F2F2.
    Вывод R: стиль R Output, Courier New 10, без фона.
    """
    n_code = 0
    n_out = 0
    for p in doc.paragraphs:
        sid = p.style.style_id if p.style is not None else ""
        sname = p.style.name if p.style is not None else ""
        if sid not in ("SourceCode", "ROutput") and sname not in (
                "Source Code", "R Output"):
            continue
        if _is_r_output_para(p) or sid == "ROutput" or sname == "R Output":
            try:
                p.style = doc.styles["R Output"]
            except KeyError:
                pass
            _clear_para_shading(p)
            for run in p.runs:
                _set_run_mono_black(run, 10)
            n_out += 1
        else:
            try:
                p.style = doc.styles["Source Code"]
            except KeyError:
                pass
            _set_para_shading(p, "F2F2F2")
            for run in p.runs:
                _set_run_mono_black(run, 10)
            n_code += 1
    return n_code, n_out


def _tbl_style_id(table):
    tbl_pr = table._tbl.tblPr
    if tbl_pr is None:
        return None
    st = tbl_pr.find(qn("w:tblStyle"))
    if st is None:
        return None
    return st.get(qn("w:val"))


def _set_cell_borders(cell, sz="4"):
    tc_pr = cell._tc.get_or_add_tcPr()
    for old in list(tc_pr.findall(qn("w:tcBorders"))):
        tc_pr.remove(old)
    borders = OxmlElement("w:tcBorders")
    for edge in ("top", "left", "bottom", "right"):
        el = OxmlElement("w:" + edge)
        el.set(qn("w:val"), "single")
        el.set(qn("w:sz"), sz)
        el.set(qn("w:space"), "0")
        el.set(qn("w:color"), "auto")
        borders.append(el)
    tc_pr.append(borders)


def _set_tbl_borders(table, sz="4"):
    tbl_pr = table._tbl.tblPr
    if tbl_pr is None:
        tbl_pr = OxmlElement("w:tblPr")
        table._tbl.insert(0, tbl_pr)
    for old in list(tbl_pr.findall(qn("w:tblBorders"))):
        tbl_pr.remove(old)
    borders = OxmlElement("w:tblBorders")
    for edge in ("top", "left", "bottom", "right", "insideH", "insideV"):
        el = OxmlElement("w:" + edge)
        el.set(qn("w:val"), "single")
        el.set(qn("w:sz"), sz)
        el.set(qn("w:space"), "0")
        el.set(qn("w:color"), "auto")
        borders.append(el)
    tbl_pr.append(borders)


def _set_tbl_width_cm(table, width_cm):
    """Фиксированная ширина таблицы на всю ширину набора, по центру."""
    twips = int(width_cm / 2.54 * 1440)
    tbl_pr = table._tbl.tblPr
    if tbl_pr is None:
        tbl_pr = OxmlElement("w:tblPr")
        table._tbl.insert(0, tbl_pr)
    for old in list(tbl_pr.findall(qn("w:tblW"))):
        tbl_pr.remove(old)
    tbl_w = OxmlElement("w:tblW")
    tbl_w.set(qn("w:type"), "dxa")
    tbl_w.set(qn("w:w"), str(twips))
    tbl_pr.append(tbl_w)
    for old in list(tbl_pr.findall(qn("w:jc"))):
        tbl_pr.remove(old)
    jc = OxmlElement("w:jc")
    jc.set(qn("w:val"), "center")
    tbl_pr.append(jc)
    table.alignment = WD_TABLE_ALIGNMENT.CENTER


def format_body_tables(doc):
    """Таблицы pandoc (стиль Table): границы, 12 пт, шапка полужирная.
    Титульные безрамочные таблицы не трогаются.
    """
    n = 0
    for table in doc.tables:
        sid = _tbl_style_id(table)
        if sid not in ("Table", "TableGrid"):
            continue
        _set_tbl_borders(table, "4")
        _set_tbl_width_cm(table, _BODY_TABLE_WIDTH_CM)
        for i, row in enumerate(table.rows):
            for cell in row.cells:
                _set_cell_borders(cell, "4")
                for p in cell.paragraphs:
                    p.paragraph_format.first_line_indent = Cm(0)
                    p.paragraph_format.line_spacing = 1.0
                    p.paragraph_format.space_before = Pt(0)
                    p.paragraph_format.space_after = Pt(0)
                    if i == 0:
                        p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                    else:
                        p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
                    for run in p.runs:
                        run.font.name = TNR
                        run.font.size = Pt(12)
                        run.bold = (i == 0)
                        r_pr = run._element.get_or_add_rPr()
                        r_fonts = r_pr.find(qn("w:rFonts"))
                        if r_fonts is None:
                            r_fonts = OxmlElement("w:rFonts")
                            r_pr.insert(0, r_fonts)
                        for attr in ("w:ascii", "w:hAnsi", "w:cs",
                                     "w:eastAsia"):
                            r_fonts.set(qn(attr), TNR)
        n += 1
    return n


def enforce_gost_body(doc):
    """Постобработка кода, вывода R и таблиц тела по ГОСТ/Синергии."""
    n_tbl = format_body_tables(doc)
    n_code, n_out = format_code_blocks(doc)
    return {"tables": n_tbl, "code": n_code, "output": n_out}


# ------------------------------------------------------------------
# Титульный лист — клон структуры ВКР (Диплом_3_версия.docx):
# форма из таблиц без внешних границ, значения подчёркнуты нижней
# границей ячейки, подсказки курсивом сверху 10 пт.
# ------------------------------------------------------------------

def _cell_bottom_border(cell):
    """Нижняя граница ячейки — «линия для заполнения» как в образце."""
    tc_pr = cell._tc.get_or_add_tcPr()
    borders = OxmlElement("w:tcBorders")
    b = OxmlElement("w:bottom")
    b.set(qn("w:val"), "single")
    b.set(qn("w:sz"), "4")
    b.set(qn("w:space"), "0")
    b.set(qn("w:color"), "auto")
    borders.append(b)
    tc_pr.append(borders)


def _no_borders(table):
    tbl_pr = table._tbl.tblPr
    borders = OxmlElement("w:tblBorders")
    for edge in ("top", "left", "bottom", "right", "insideH", "insideV"):
        el = OxmlElement("w:" + edge)
        el.set(qn("w:val"), "none")
        el.set(qn("w:sz"), "0")
        borders.append(el)
    tbl_pr.append(borders)


def _para(container, text="", bold=False, italic=False, underline=False,
          size=14, align=WD_ALIGN_PARAGRAPH.CENTER, superscript=False,
          before=0, after=0):
    p = container.add_paragraph()
    p.alignment = align
    pf = p.paragraph_format
    pf.first_line_indent = Cm(0)
    pf.line_spacing = 1.0
    pf.space_before = Pt(before)
    pf.space_after = Pt(after)
    if text:
        r = p.add_run(text)
        r.bold = bold
        r.italic = italic
        r.underline = underline
        r.font.name = TNR
        r.font.size = Pt(size)
        if superscript:
            r.font.superscript = True
    return p


def _cell_para(cell, text="", bold=False, italic=False, size=12,
               align=WD_ALIGN_PARAGRAPH.CENTER, superscript=False):
    cell.text = ""
    return _para(cell, text, bold=bold, italic=italic, size=size,
                 align=align, superscript=superscript)


def add_title_page(doc, discipline="", topic="", lab_number="",
                   student="Шатов Илья Андреевич", group="",
                   teacher="", year=2026):
    """Титульный лист — точный клон структуры титульного листа ВКР."""
    # --- шапка вуза ---
    _para(doc, before=6)
    _para(doc, "НЕГОСУДАРСТВЕННОЕ ОБРАЗОВАТЕЛЬНОЕ ЧАСТНОЕ УЧРЕЖДЕНИЕ "
               "ВЫСШЕГО ОБРАЗОВАНИЯ")
    _para(doc, "«МОСКОВСКИЙ ФИНАНСОВО-ПРОМЫШЛЕННЫЙ УНИВЕРСИТЕТ «СИНЕРГИЯ»»",
          bold=True)
    _para(doc)

    # --- факультет ---
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.first_line_indent = Cm(0)
    p.paragraph_format.line_spacing = 1.0
    r1 = p.add_run("Факультет ")
    r1.font.name = TNR
    r1.font.size = Pt(15)
    r2 = p.add_run("Информационных технологий")
    r2.font.name = TNR
    r2.font.size = Pt(15)
    r2.underline = True

    # --- таблица «Направление / Кафедра / Профиль» (как в ВКР) ---
    t = doc.add_table(rows=3, cols=4)
    _no_borders(t)
    t.autofit = False
    widths = (Cm(3.2), Cm(8.0), Cm(2.1), Cm(3.2))
    for row in t.rows:
        for cell, w in zip(row.cells, widths):
            cell.width = w

    _cell_para(t.rows[0].cells[0], "Направление подготовки:", bold=True,
               align=WD_ALIGN_PARAGRAPH.LEFT)
    c = t.rows[0].cells[1]
    _cell_para(c, "09.03.02 Информационные системы и технологии")
    _cell_bottom_border(c)
    _cell_para(t.rows[0].cells[2], "Кафедра", bold=True,
               align=WD_ALIGN_PARAGRAPH.LEFT)
    c = t.rows[0].cells[3]
    _cell_para(c, "ИИиАД")
    _cell_bottom_border(c)

    _cell_para(t.rows[1].cells[0], "(код, наименование)", italic=True,
               size=10, superscript=True)
    _cell_para(t.rows[1].cells[3], "(аббревиатура)", italic=True,
               size=10, superscript=True)

    _cell_para(t.rows[2].cells[0], "Профиль:", bold=True,
               align=WD_ALIGN_PARAGRAPH.LEFT)
    merged = t.rows[2].cells[1].merge(t.rows[2].cells[3])
    _cell_para(merged, "Разработка, сопровождение и обеспечение "
                       "безопасности информационных систем")
    _cell_bottom_border(merged)

    _para(doc)

    # --- тип работы ---
    _para(doc, "ОТЧЕТ ПО ЛАБОРАТОРНОЙ РАБОТЕ № " + str(lab_number),
          bold=True, size=16, after=6)
    _para(doc, "по дисциплине «" + discipline + "»", bold=True, after=6)

    # --- таблица «на тему» (как в ВКР) ---
    t2 = doc.add_table(rows=1, cols=2)
    _no_borders(t2)
    t2.autofit = False
    t2.rows[0].cells[0].width = Cm(3.2)
    t2.rows[0].cells[1].width = Cm(13.3)
    _cell_para(t2.rows[0].cells[0], "на тему", bold=True, size=14,
               align=WD_ALIGN_PARAGRAPH.LEFT)
    c = t2.rows[0].cells[1]
    _cell_para(c, topic, bold=True, size=12)
    _cell_bottom_border(c)

    _para(doc)

    # --- таблица участников (как в ВКР) ---
    t3 = doc.add_table(rows=5, cols=2)
    _no_borders(t3)
    t3.autofit = False
    for row in t3.rows:
        row.cells[0].width = Cm(5.5)
        row.cells[1].width = Cm(11.0)

    _cell_para(t3.rows[0].cells[0], "Обучающийся", bold=True,
               align=WD_ALIGN_PARAGRAPH.LEFT)
    _cell_para(t3.rows[0].cells[1], student, bold=True, italic=True)
    _cell_para(t3.rows[1].cells[0], "(Фамилия Имя Отчество)", italic=True,
               size=8)
    _cell_para(t3.rows[1].cells[1], "(подпись)", italic=True, size=8)

    _cell_para(t3.rows[2].cells[0], "Группа", bold=True,
               align=WD_ALIGN_PARAGRAPH.LEFT)
    _cell_para(t3.rows[2].cells[1], group, bold=True, italic=True)

    _cell_para(t3.rows[3].cells[0], "Преподаватель", bold=True,
               align=WD_ALIGN_PARAGRAPH.LEFT)
    _cell_para(t3.rows[3].cells[1], teacher if teacher else "", bold=True,
               italic=True)
    _cell_para(t3.rows[4].cells[0], "(Фамилия Имя Отчество)", italic=True,
               size=8)
    _cell_para(t3.rows[4].cells[1], "(подпись)", italic=True, size=8)

    for _ in range(2):
        _para(doc)

    # --- город и год (как в ВКР: МОСКВА 20XX г., год подчёркнут) ---
    # «МОСКВА» прижата к низу страницы отступом сверху абзаца.
    # 40 пт (было 220): по фактической разметке LibreOffice нижняя строка
    # таблицы участников заканчивается на ~656 пт, до нижнего поля остаётся
    # ~130 пт; 220/180 пт выталкивали строку на вторую страницу.
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.first_line_indent = Cm(0)
    p.paragraph_format.line_spacing = 1.0
    p.paragraph_format.space_before = Pt(40)
    r = p.add_run("МОСКВА ")
    r.bold = True
    r.font.name = TNR
    r.font.size = Pt(14)
    r = p.add_run(str(year))
    r.bold = True
    r.underline = True
    r.font.name = TNR
    r.font.size = Pt(14)
    r = p.add_run(" г.")
    r.bold = True
    r.font.name = TNR
    r.font.size = Pt(14)


# ------------------------------------------------------------------
# Высокоуровневый API для генерации отчётов
# ------------------------------------------------------------------

class SynergyReport:
    """
    Отчёт по лабораторной работе в стандартах «Синергии» (клон ВКР).

    Пример:
        r = SynergyReport(discipline="Продвинутые технологии работы "
                          "с большими данными", topic="...",
                          lab_number=1, group="ВБИо-403-рсоб")
        r.heading("1 Анализ данных", level=1)
        r.paragraph("Условие задачи ...")
        r.code("ggplot(diamonds, aes(price)) + geom_histogram()")
        r.output("`stat_bin()` using `bins = 30` ...")
        r.figure("output/plot.png", "Распределение цены")
        r.table([["Переменная", "Среднее"], ["mpg", "20.09"]],
                caption="Описательные статистики", header=True)
        r.conclusion("Распределение цены правоасимметрично.")
        r.save("labs/lab-01/output/report.docx")
    """

    def __init__(self, discipline="", topic="", lab_number="",
                 student="Шатов Илья Андреевич", group="",
                 teacher="", year=2026):
        self.doc = new_document()
        add_title_page(self.doc, discipline=discipline, topic=topic,
                       lab_number=lab_number, student=student, group=group,
                       teacher=teacher, year=year)
        self.fig_n = 0
        self.tab_n = 0
        # разрыв страницы после титульного листа
        p = self.doc.add_paragraph(style="TitlePage")
        p.add_run().add_break(WD_BREAK.PAGE)

    # --- структура ---
    def heading(self, text, level=1):
        h = self.doc.add_heading(text, level=level)
        h.style = self.doc.styles["Heading %d" % level]
        h.paragraph_format.first_line_indent = Cm(0)
        return h

    def paragraph(self, text, bold=False, italic=False):
        p = self.doc.add_paragraph(text, style="Body Text")
        if bold:
            for run in p.runs:
                run.bold = True
        if italic:
            for run in p.runs:
                run.italic = True
        return p

    # --- код и вывод ---
    def code(self, text):
        for line in text.rstrip("\n").split("\n"):
            self.doc.add_paragraph(line, style="Source Code")
        self._spacer()

    def output(self, text):
        for line in text.rstrip("\n").split("\n"):
            self.doc.add_paragraph(line, style="R Output")
        self._spacer()

    def _spacer(self):
        p = self.doc.add_paragraph(style="Body Text")
        p.paragraph_format.space_after = Pt(6)

    # --- иллюстрации и таблицы ---
    def figure(self, image_path, caption, width_cm=15.5):
        self.fig_n += 1
        self.doc.add_picture(image_path, width=Cm(width_cm))
        self.doc.paragraphs[-1].alignment = WD_ALIGN_PARAGRAPH.CENTER
        cap = self.doc.add_paragraph(style="Image Caption")
        cap.add_run("Рисунок %d — %s" % (self.fig_n, caption))
        return self.fig_n

    def table(self, rows, caption=None, header=True, font_size=12):
        if caption:
            self.tab_n += 1
            tc = self.doc.add_paragraph(style="Table Caption")
            tc.add_run("Таблица %d — %s" % (self.tab_n, caption.rstrip(".")))
        n_cols = len(rows[0])
        tbl = self.doc.add_table(rows=len(rows), cols=n_cols)
        try:
            tbl.style = self.doc.styles["Table Grid"]
        except KeyError:
            pass
        tbl.alignment = WD_TABLE_ALIGNMENT.CENTER
        for i, row in enumerate(rows):
            for j, value in enumerate(row):
                cell = tbl.cell(i, j)
                cell.text = str(value)
                for p in cell.paragraphs:
                    p.style = self.doc.styles["Normal"]
                    p.paragraph_format.first_line_indent = Cm(0)
                    p.paragraph_format.line_spacing = 1.0
                    p.paragraph_format.alignment = (
                        WD_ALIGN_PARAGRAPH.CENTER if header and i == 0
                        else WD_ALIGN_PARAGRAPH.LEFT)
                    for run in p.runs:
                        run.font.name = TNR
                        run.font.size = Pt(font_size)
                        if header and i == 0:
                            run.bold = True
        self._spacer()
        return tbl

    # --- выводы ---
    def conclusion(self, text, label="Вывод"):
        p = self.doc.add_paragraph(style="Body Text")
        p.add_run(label + ": ").bold = True
        p.add_run(text)
        return p

    def hypotheses(self, h0, h1):
        p = self.doc.add_paragraph(style="Body Text")
        p.add_run("Гипотезы: ").bold = True
        p.add_run("H\u2080 — " + h0 + "; ")
        p.add_run("H\u2081 — ").bold = True
        p.add_run(h1)
        return p

    def interpretation(self, text):
        p = self.doc.add_paragraph(style="Body Text")
        p.add_run("Интерпретация: ").bold = True
        p.add_run(text)
        return p

    # --- сохранение ---
    def save(self, path):
        self.doc.save(path)
        return path
