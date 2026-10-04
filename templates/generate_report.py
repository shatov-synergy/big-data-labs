# -*- coding: utf-8 -*-
"""
generate_report.py — пример генерации отчёта по лабораторной работе
в стандартах «Синергии» без Word и без R (только python-docx).

Нейросеть (или обучающийся) вызывает класс SynergyReport из synergy_docx.py:

    cd templates
    python generate_report.py          # создаёт demo_report.docx

Схема каждого пункта анализа в отчёте (по образцу утверждённых отчётов):
    условие задачи -> код (Courier, серый фон) -> результат R (Courier)
    -> «Гипотезы: H0 — ...; H1 — ...» -> «Интерпретация: ...» -> «Вывод: ...»
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from synergy_docx import SynergyReport  # noqa: E402


def main():
    r = SynergyReport(
        discipline="Продвинутые технологии работы с большими данными",
        topic="Разведочный анализ данных и предобработка временных рядов",
        lab_number=1,
        group="ВБИо-403-рсоб",
        year=2026,
    )

    r.heading("1 Задание 1. Разведочный анализ. Базовая визуализация данных", 1)
    r.paragraph("Загрузите встроенный набор данных diamonds и постройте "
                "следующие виды визуализации данных...")
    r.code('library(ggplot2)\ndata(diamonds)\n'
           'ggplot(diamonds, aes(x = price)) +\n'
           '  geom_histogram(bins = 30, fill = "steelblue")')
    r.output("'data.frame': 53940 obs. of  10 variables:")
    r.figure("../labs/lab-01/output/task1_a_price_hist.png",
             "Гистограмма распределения цены бриллиантов")
    r.table([["Переменная", "Среднее", "Медиана"],
             ["price", "3932.8", "2401"],
             ["depth", "61.75", "61.8"]],
            caption="Описательные статистики (фрагмент)")
    r.conclusion("Распределение цены правоасимметрично: медиана (2401 $) "
                 "заметно меньше среднего (3933 $).")

    out = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       "demo_report.docx")
    r.save(out)
    print("OK:", out)


if __name__ == "__main__":
    main()
