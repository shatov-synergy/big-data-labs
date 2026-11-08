#!/bin/bash
# Рендер всех страниц финального PDF в PNG для визуальной проверки.
set -e
cd /project/labs/lab-03/output
rm -f pg-*.png page_check*.png
pdfinfo "Laboratornyiy praktikum 3_отчет.pdf" | grep -E "Pages|Page size"
pdftoppm -png -r 70 "Laboratornyiy praktikum 3_отчет.pdf" pg
ls pg-*.png
