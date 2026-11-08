#!/bin/bash
# Финальный PDF (проход 2) из docx с заполненным СОДЕРЖАНИЕМ
# и копирование под именами сдачи. Запускать в контейнере lab03.
set -e
cd /project/labs/lab-03/output
soffice --headless --convert-to pdf --outdir . otchet.docx >/dev/null 2>&1
cp -f otchet.docx "Laboratornyiy praktikum 3_отчет.docx"
cp -f otchet.pdf "Laboratornyiy praktikum 3_отчет.pdf"
rm -f pg-*.png
pdftoppm -png -r 70 "Laboratornyiy praktikum 3_отчет.pdf" pg
ls pg-*.png | wc -l
ls -la "Laboratornyiy praktikum 3_отчет.docx" "Laboratornyiy praktikum 3_отчет.pdf"
