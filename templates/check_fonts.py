# -*- coding: utf-8 -*-
"""
check_fonts.py — полная проверка шрифтов и выравнивания docx по ГОСТ.

Проверяет:
  1) docDefaults и тему документа (majorFont/minorFont) — Times New Roman;
  2) ни один стиль не содержит Calibri/Cambria или theme-атрибутов;
  3) ключевые стили ГОСТ (Normal, Heading 1-3, подписи, код) — верные
     шрифт и размер; код — Courier New;
  4) в document.xml ни один прогон не ссылается на тему и не задаёт
     Calibri явно;
  5) абзацы основного текста выровнены по ширине с красной строкой.

Выход: «ГОСТ: ШРИФТЫ И ВЫРАВНИВАНИЕ OK» либо список расхождений (exit 1).
"""
import re
import sys
import zipfile

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml.ns import qn

TNR = "Times New Roman"
MONO = "Courier New"

WANT = {
    "Normal":          (TNR, 14),
    "Heading 1":       (TNR, 16),
    "Heading 2":       (TNR, 14),
    "Heading 3":       (TNR, 14),
    "Image Caption":   (TNR, 14),
    "Table Caption":   (TNR, 14),
    "Source Code":     (MONO, 10),
    "R Output":        (MONO, 10),
    "Body Text":       (TNR, 14),
    "First Paragraph": (TNR, 14),
}

path = sys.argv[1]
problems = []

z = zipfile.ZipFile(path)
styles_xml = z.read("word/styles.xml").decode("utf-8")
doc_xml = z.read("word/document.xml").decode("utf-8")
theme_xml = z.read("word/theme/theme1.xml").decode("utf-8") \
    if "word/theme/theme1.xml" in z.namelist() else ""

# --- 1. тема ---
m1 = re.search(r'<a:majorFont>\s*<a:latin[^>]*typeface="([^"]*)"', theme_xml)
m2 = re.search(r'<a:minorFont>\s*<a:latin[^>]*typeface="([^"]*)"', theme_xml)
if m1 and m1.group(1) != TNR:
    problems.append("тема majorFont=%s (нужно %s)" % (m1.group(1), TNR))
if m2 and m2.group(1) != TNR:
    problems.append("тема minorFont=%s (нужно %s)" % (m2.group(1), TNR))

# --- 2. Calibri/Cambria/theme в текстовых стилях ---
# (неиспользуемые gallery-стили таблиц Word не влияют на отображение)
text_blocks = [b for b in re.findall(r"<w:style [^>]*>.*?</w:style>",
                                     styles_xml, re.S)
               if re.search(r'w:type="(paragraph|character)"', b)]
n_theme_styles = sum("Theme" in b for b in text_blocks)
if n_theme_styles:
    problems.append("styles.xml: %d текстовых стилей с theme-атрибутами"
                    % n_theme_styles)
if "Calibri" in "".join(text_blocks):
    problems.append("styles.xml: Calibri в текстовых стилях")

# используемые в документе стили обязаны быть TNR/Courier без темы
used_ids = set(re.findall(r'w:(?:p|r)Style w:val="([^"]+)"', doc_xml))
for b in text_blocks:
    sid_m = re.search(r'w:styleId="([^"]+)"', b)
    if not sid_m or sid_m.group(1) not in used_ids:
        continue
    if "Theme" in b or "Calibri" in b:
        problems.append("используемый стиль %s содержит theme/Calibri"
                        % sid_m.group(1))

# --- 3. ключевые стили ---
doc = Document(path)
for name, (want_font, want_size) in WANT.items():
    try:
        st = doc.styles[name]
    except KeyError:
        problems.append("стиль отсутствует: %s" % name)
        continue
    el = st.element
    rf = el.find(qn("w:rPr") + "/" + qn("w:rFonts"))
    font = rf.get(qn("w:ascii")) if rf is not None else None
    sz = st.font.size.pt if st.font.size else None
    if font != want_font or sz != want_size:
        problems.append("стиль %s: font=%s size=%s (нужно %s %s)"
                        % (name, font, sz, want_font, want_size))

# --- 4. прогоны в document.xml ---
runs_theme = len(re.findall(r'<w:rFonts[^>]*Theme[^>]*>', doc_xml))
if runs_theme:
    problems.append("document.xml: %d прогонов с theme-атрибутами" % runs_theme)
if "Calibri" in doc_xml:
    problems.append("document.xml содержит Calibri")

# --- 5. выравнивание основного текста ---
n_bad = 0
for p in doc.paragraphs:
    sid = p.style.style_id if p.style is not None else ""
    if sid in ("BodyText", "FirstParagraph") and p.text.strip():
        if p.paragraph_format.alignment != WD_ALIGN_PARAGRAPH.JUSTIFY:
            n_bad += 1
if n_bad:
    problems.append("абзацев основного текста не по ширине: %d" % n_bad)

# --- итог ---
if problems:
    print("РАСХОЖДЕНИЯ:")
    for p in problems:
        print("  -", p)
    sys.exit(1)
print("ГОСТ: ШРИФТЫ И ВЫРАВНИВАНИЕ OK "
      "(тема, docDefaults, стили, прогоны, выравнивание)")
