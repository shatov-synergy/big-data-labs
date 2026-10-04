# -*- coding: utf-8 -*-
"""Проверка стилей ГОСТ в docx: шрифты и параметры key-стилей."""
import sys

from docx import Document
from docx.oxml.ns import qn

WANT = {
    "Normal":         ("Times New Roman", 14),
    "Heading 1":      ("Times New Roman", 16),
    "Heading 2":      ("Times New Roman", 14),
    "Heading 3":      ("Times New Roman", 14),
    "Image Caption":  ("Times New Roman", 14),
    "Table Caption":  ("Times New Roman", 14),
    "Source Code":    ("Courier New", 10),
    "R Output":       ("Courier New", 10),
    "Body Text":      ("Times New Roman", 14),
    "First Paragraph": ("Times New Roman", 14),
}

ok = True

doc = Document(sys.argv[1])

# docDefaults
styles_el = doc.styles.element
rfonts = styles_el.find(qn("w:docDefaults") + "/" + qn("w:rPrDefault")
                        + "/" + qn("w:rPr") + "/" + qn("w:rFonts"))
if rfonts is None:
    print("!! docDefaults rFonts не найден")
    ok = False
else:
    dd_font = rfonts.get(qn("w:ascii"))
    theme = [a for a in rfonts.attrib if a.endswith("Theme") or a.endswith("theme")]
    print("docDefaults: ascii=%s theme-атрибуты=%s" % (dd_font, theme or "нет"))
    if dd_font != "Times New Roman" or theme:
        ok = False

for name, (want_font, want_size) in WANT.items():
    try:
        st = doc.styles[name]
    except KeyError:
        print("!! стиль отсутствует: %s" % name)
        ok = False
        continue
    el = st.element
    rf = el.find(qn("w:rPr") + "/" + qn("w:rFonts"))
    font = rf.get(qn("w:ascii")) if rf is not None else None
    theme = [a for a in (rf.attrib if rf is not None else {})
             if a.endswith("Theme") or a.endswith("theme")]
    sz = st.font.size.pt if st.font.size else None
    status = "OK " if (font == want_font and not theme and sz == want_size) else "!!!"
    if status == "!!!":
        ok = False
    print("%s %-15s font=%-14s size=%-4s theme=%s" %
          (status, name, font, sz, theme or "нет"))

print("\nИТОГ:", "ВСЕ СТИЛИ СООТВЕТСТВУЮТ ГОСТ" if ok else "ЕСТЬ РАСХОЖДЕНИЯ")
sys.exit(0 if ok else 1)
