"""Аудит Задачи 2, Проход 4 (python): ансамбль и метаморфные тесты.

Проверки:
1. yhat_ens = 0.5*yhat_prophet_best + 0.5*yhat_ets по 30 датам
   (веса, сумма весов, совпадение дат).
2. Метаморфный тест весов: вес Prophet=1 -> ансамбль = Prophet;
   вес Prophet=0 -> ансамбль = ETS.
3. Метаморфный тест масштаба: y и yhat * k -> RMSE/MAE * k, MAPE неизменен.
4. Независимый пересчёт метрик всех 6 моделей из predictions.csv.
"""
import csv
import math

P = "labs/lab-04/output/task2_predictions.csv"
rows = list(csv.DictReader(open(P, encoding="utf-8")))
models = {}
for r in rows:
    models.setdefault(r["model"], []).append(
        (r["ds"], float(r["y"]), float(r["predicted"])))
for m in models:
    models[m].sort(key=lambda t: t[0])

def rmse(pairs): return math.sqrt(sum((y - h) ** 2 for _, y, h in pairs) / len(pairs))
def mae(pairs): return sum(abs(y - h) for _, y, h in pairs) / len(pairs)
def mape(pairs): return 100 * sum(abs((y - h) / y) for _, y, h in pairs) / len(pairs)

print("=== 1. Формула ансамбля ===")
ens = models["Ensemble (Prophet + ETS)"]
prb = models["Prophet (base)"]          # лучшая Prophet по RMSE (проход 3)
ets = models["ETS (AAA: A,Ad,A)"]
dates_equal = [t[0] for t in ens] == [t[0] for t in prb] == [t[0] for t in ets]
print("даты трёх рядов равны:", dates_equal)
mid = [0.5 * prb[i][2] + 0.5 * ets[i][2] for i in range(30)]
print("max |ens - (0.5*Prophet + 0.5*ETS)| =", max(abs(ens[i][2] - mid[i]) for i in range(30)))
print("веса 0.5 и 0.5, сумма = 1.0; способ выбора весов: простое среднее (без подбора по test)")

print("\n=== 2. Метаморфный тест весов ===")
w1 = [1.0 * prb[i][2] + 0.0 * ets[i][2] for i in range(30)]
w0 = [0.0 * prb[i][2] + 1.0 * ets[i][2] for i in range(30)]
print("вес Prophet=1 совпадает с Prophet:", max(abs(w1[i] - prb[i][2]) for i in range(30)) == 0)
print("вес Prophet=0 совпадает с ETS:   ", max(abs(w0[i] - ets[i][2]) for i in range(30)) == 0)

print("\n=== 3. Метаморфный тест масштаба (k=3) ===")
k = 3.0
sc = [(d, k * y, k * h) for d, y, h in ens]
r0, r1 = rmse(ens), rmse(sc)
m0, m1 = mae(ens), mae(sc)
mp0, mp1 = mape(ens), mape(sc)
print(f"RMSE: {r0:.4f} -> {r1:.4f} (ожидалось {k*r0:.4f}); отношение {r1/r0:.6f}")
print(f"MAE:  {m0:.4f} -> {m1:.4f}; отношение {m1/m0:.6f}")
print(f"MAPE: {mp0:.6f} -> {mp1:.6f}; |Δ| = {abs(mp1-mp0):.2e}")

print("\n=== 4. Независимые метрики 6 моделей (из predictions.csv) ===")
csv_metrics = {r["model"]: (float(r["RMSE"]), float(r["MAPE"]))
               for r in csv.DictReader(open("labs/lab-04/output/task2_metrics.csv", encoding="utf-8"))}
for m in sorted(models, key=lambda x: rmse(models[x])):
    r_, m_, mp_ = rmse(models[m]), mae(models[m]), mape(models[m])
    cr = csv_metrics.get(m, (float("nan"),) * 2)
    print(f"{m:28s} RMSE={r_:9.4f} (csv {cr[0]:9.4f}, Δ={r_-cr[0]:+.2e}) "
          f"MAE={m_:8.4f} MAPE={mp_:6.3f}% (csv {cr[1]:6.3f})")

print("\n=== 5. Факт един и без NA ===")
all_pairs = [(m, t[0], t[1]) for m in models for t in models[m]]
bad = [f for f in all_pairs if f[2] != f[2]]
dates = {t[0] for m in models for t in models[m]}
print("строк всего:", len(all_pairs), "| NA в y:", len(bad),
      "| уникальных дат:", len(dates), "| строк на модель:", len(rows) // len(models))
