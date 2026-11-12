"""Аудит Задачи 2, Проход 5: победитель по трём метрикам, PDF<->CSV
по метрикам и подбору, статические факты о графиках."""
import csv
import math
import re
import subprocess

# --- 1. победитель по каждой метрике ---
rows = list(csv.DictReader(open("labs/lab-04/output/task2_predictions.csv",
                                encoding="utf-8")))
models = {}
for r in rows:
    models.setdefault(r["model"], []).append((float(r["y"]), float(r["predicted"])))

def rmse(v): return math.sqrt(sum((y - h) ** 2 for y, h in v) / len(v))
def mae(v):  return sum(abs(y - h) for y, h in v) / len(v)
def mape(v): return 100 * sum(abs((y - h) / y) for y, h in v) / len(v)

print("=== 1. Победитель по каждой метрике ===")
for name, fn in [("RMSE", rmse), ("MAE", mae), ("MAPE", mape)]:
    best = min(models, key=lambda m: fn(models[m]))
    print(f"{name}: {best} ({fn(models[best]):.4f})")

# --- 2. PDF-текст: извлечь и сверить ключевые числа ---
pdf = "labs/lab-04/output/Laboratornyiy praktikum 4_Задача 2_отчет.pdf"
txt = subprocess.run(["pdftotext", pdf, "-"], capture_output=True,
                     text=True).stdout
txt_n = re.sub(r"\s+", " ", txt)

print("\n=== 2. PDF <-> CSV по метрикам (таблица 7) ===")
metrics = {r["model"]: r for r in csv.DictReader(
    open("labs/lab-04/output/task2_metrics.csv", encoding="utf-8"))}
checks = [
    ("Ensemble RMSE 68,3", "68,3" in txt_n),
    ("Ensemble MAPE 8,79", "8,79" in txt_n),
    ("ETS(AAA) 87,4", "87,4" in txt_n),
    ("Prophet base 94,5", "94,5" in txt_n),
    ("Prophet+погода 96,4", "96,4" in txt_n),
    ("SARIMA 97,3", "97,3" in txt_n),
    ("Prophet+погода+цена 97,8", "97,8" in txt_n),
]
for name, ok in checks:
    print(f"{'OK ' if ok else 'НЕТ'} {name}")

print("\n=== 3. PDF: подбор и абляция ===")
checks3 = [
    ("weekly.seasonality = 3", re.search(r"weekly\.seasonality\s*=\s*3", txt_n)),
    ("n.changepoints = 40", re.search(r"n\.changepoints\s*=\s*40", txt_n)),
    ("seas 0,5 / chg 0,05 / hol 5", "0,5" in txt_n and "0,05" in txt_n and "5" in txt_n),
    ("NY: 94,5 vs 113,9", "113,9" in txt_n),
    ("Итоговый рейтинг (строка со всеми моделями)",
     "Ensemble" in txt_n and "SARIMA" in txt_n and "ETS" in txt_n),
]
for name, ok in checks3:
    print(f"{'OK ' if ok else 'НЕТ'} {name}")

print("\n=== 4. Формулировки победителя (не 'доказанно лучшая вообще') ===")
ctx = re.findall(r"[^.]{0,80}(?:лучш|победил|выигрыв)[^.]{0,80}", txt_n)
for c in ctx[:8]:
    print(" •", c.strip()[:150])

print("\n=== 5. Остатки старой категории (Алкоголь) в PDF? ===")
for w in ["Алкогол", "алкогол", "pre-NY", "ВДНХ"]:
    print(f"'{w}':", txt_n.count(w), "вхожд.")
alco_ctx = re.findall(r".{60}[Аа]лкогол.{60}", txt_n)
for c in alco_ctx[:4]:
    print(" •", c.strip()[:140])

print("\n=== 6. Страницы PDF ===")
pages = txt.count("\f") + 1
print("страниц (по pdftotext):", pages)
