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
    """Явно прописывает шрифт во всех слотах rFonts (ascii/hAnsi/cs/eastAsia)."""
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
    for attr in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
        r_fonts.set(qn(attr), font_name)


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
    compact = _get_or_add_style(doc, "Compact", base="Normal")
    compact.paragraph_format.space_after = Pt(0)

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

    # --- Подпись рисунка: «Рисунок N. Название», по центру, ПОЛУЖИРНО ---
    cap = _get_or_add_style(doc, "Image Caption", base="Normal")
    _set_style_font(cap, TNR, 14, bold=True, color=(0, 0, 0))
    cap.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    cap.paragraph_format.first_line_indent = Cm(0)
    cap.paragraph_format.line_spacing = 1.0
    cap.paragraph_format.space_before = Pt(6)
    cap.paragraph_format.space_after = Pt(12)
    cap.paragraph_format.keep_with_next = False

    # --- Подписи таблиц (как в ВКР, две строки) ---
    tcap = _get_or_add_style(doc, "Table Caption", base="Normal")
    _set_style_font(tcap, TNR, 14, bold=True, color=(0, 0, 0))
    tcap.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.RIGHT
    tcap.paragraph_format.first_line_indent = Cm(0)
    tcap.paragraph_format.line_spacing = 1.0
    tcap.paragraph_format.space_before = Pt(12)
    tcap.paragraph_format.space_after = Pt(0)
    tcap.paragraph_format.keep_with_next = True

    ttitle = _get_or_add_style(doc, "Table Title", base="Normal")
    _set_style_font(ttitle, TNR, 14, bold=True, color=(0, 0, 0))
    ttitle.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    ttitle.paragraph_format.first_line_indent = Cm(0)
    ttitle.paragraph_format.line_spacing = 1.0
    ttitle.paragraph_format.space_before = Pt(0)
    ttitle.paragraph_format.space_after = Pt(6)
    ttitle.paragraph_format.keep_with_next = True

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


def new_document():
    """Пустой документ со всеми стандартами (страница + стили)."""
    doc = Document()
    configure_page(doc)
    configure_styles(doc)
    return doc


# ------------------------------------------------------------------
# Титульный лист «Синергии» (форма: метки полужирные, значения
# подчёркнутые, подсказки 9 пт)
# ------------------------------------------------------------------

def _tp(doc, text="", bold=False, small=False, underline_value=None):
    p = doc.add_paragraph(style="TitlePageSmall" if small else "TitlePage")
    if bold and text:
        p.add_run(text).bold = True
    elif underline_value is not None:
        r = p.add_run(underline_value)
        r.underline = True
    elif text:
        p.add_run(text)
    return p


def _tp_label_value(doc, label, value, hint=None):
    """Строка формы: «Метка: значение(подчёркнуто)» + подсказка 9 пт."""
    p = doc.add_paragraph(style="TitlePage")
    p.add_run(label).bold = True
    r = p.add_run(" " + value + " ")
    r.underline = True
    if hint:
        _tp(doc, hint, small=True)


def add_title_page(doc, discipline="", topic="", lab_number="",
                   student="Шатов Илья Андреевич", group="",
                   teacher="", year=2026,
                   institute="Информационных технологий",
                   direction="09.03.02 Информационные системы и технологии",
                   study_form="очная"):
    """Титульный лист по образцу формы Синергии."""
    # шапка вуза
    _tp(doc, "НЕГОСУДАРСТВЕННОЕ ОБРАЗОВАТЕЛЬНОЕ ЧАСТНОЕ", bold=True)
    _tp(doc, "УЧРЕЖДЕНИЕ ВЫСШЕГО ОБРАЗОВАНИЯ", bold=True)
    _tp(doc, "«МОСКОВСКИЙ ФИНАНСОВО-ПРОМЫШЛЕННЫЙ УНИВЕРСИТЕТ «СИНЕРГИЯ»»",
        bold=True)
    _tp(doc)
    # форма
    _tp_label_value(doc, "Факультет:", institute,
                    "(наименование факультета / института)")
    _tp_label_value(doc, "Направление подготовки:", direction,
                    "(код и наименование направления подготовки / специальности)")
    _tp_label_value(doc, "Форма обучения:", study_form,
                    "(очная, очно-заочная, заочная)")
    _tp(doc)
    _tp(doc)
    # тип работы
    _tp(doc, "ОТЧЕТ", bold=True)
    _tp(doc, "по лабораторной работе № " + str(lab_number), bold=True)
    p = doc.add_paragraph(style="TitlePage")
    p.add_run("по дисциплине ").bold = True
    p.add_run("«" + discipline + "»")
    p = doc.add_paragraph(style="TitlePage")
    p.add_run("на тему: ").bold = True
    r = p.add_run("«" + topic + "»")
    r.underline = True
    _tp(doc)
    _tp(doc)
    _tp(doc)
    # исполнители
    p = doc.add_paragraph(style="TitlePage")
    p.add_run("Обучающийся: ").bold = True
    p.add_run(" " + student + " ").underline = True
    _tp(doc, "(Фамилия Имя Отчество)                                    (подпись)",
        small=True)
    p = doc.add_paragraph(style="TitlePage")
    p.add_run("Группа: ").bold = True
    p.add_run(" " + group + " ").underline = True
    p = doc.add_paragraph(style="TitlePage")
    p.add_run("Преподаватель: ").bold = True
    p.add_run(" " + (teacher if teacher else " " * 30) + " ").underline = True
    _tp(doc, "(Фамилия Имя Отчество)                                    (подпись)",
        small=True)
    _tp(doc)
    _tp(doc)
    _tp(doc)
    _tp(doc, "Москва " + str(year) + " г.", bold=True)


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
        cap.add_run("Рисунок %d. %s" % (self.fig_n, caption))
        return self.fig_n

    def table(self, rows, caption=None, header=True, font_size=12):
        if caption:
            self.tab_n += 1
            tc = self.doc.add_paragraph(style="Table Caption")
            tc.add_run("Таблица %d" % self.tab_n)
            tt = self.doc.add_paragraph(style="Table Title")
            tt.add_run(caption.rstrip(".") + ".")
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
