#!/bin/bash
# Финальный PDF (проход 2) из docx с заполненным СОДЕРЖАНИЕМ
# и копирование под именами сдачи. Запускать в контейнере
# (docker exec lab04, репозиторий смонтирован в /project).
set -e
cd /project/labs/lab-04/output

for i in 1 2; do
  soffice --headless --convert-to pdf --outdir . "zadacha${i}_report.docx" >/dev/null 2>&1
done

cp -f zadacha1_report.docx "Laboratornyiy praktikum 4_Задача 1_отчет.docx"
cp -f zadacha1_report.pdf "Laboratornyiy praktikum 4_Задача 1_отчет.pdf"
cp -f zadacha2_report.docx "Laboratornyiy praktikum 4_Задача 2_отчет.docx"
cp -f zadacha2_report.pdf "Laboratornyiy praktikum 4_Задача 2_отчет.pdf"

mkdir -p ../itog
cp -f "Laboratornyiy praktikum 4_Задача 1_отчет.docx" \
      "../itog/ДМЭ-101ЦЭИИ.Шатов_Лабораторный практикум_4_Задача 1_отчет.docx"
cp -f "Laboratornyiy praktikum 4_Задача 1_отчет.pdf" \
      "../itog/ДМЭ-101ЦЭИИ.Шатов_Лабораторный практикум_4_Задача 1_отчет.pdf"
cp -f "Laboratornyiy praktikum 4_Задача 2_отчет.docx" \
      "../itog/ДМЭ-101ЦЭИИ.Шатов_Лабораторный практикум_4_Задача 2_отчет.docx"
cp -f "Laboratornyiy praktikum 4_Задача 2_отчет.pdf" \
      "../itog/ДМЭ-101ЦЭИИ.Шатов_Лабораторный практикум_4_Задача 2_отчет.pdf"

rm -f pg-*.png
pdftoppm -png -r 70 "Laboratornyiy praktikum 4_Задача 1_отчет.pdf" pg1
pdftoppm -png -r 70 "Laboratornyiy praktikum 4_Задача 2_отчет.pdf" pg2
ls pg1-*.png pg2-*.png | wc -l
ls -la "Laboratornyiy praktikum 4_Задача 1_отчет.docx" \
       "Laboratornyiy praktikum 4_Задача 2_отчет.docx"
