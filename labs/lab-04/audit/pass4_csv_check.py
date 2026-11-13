# Проход 4: содержательная сверка CSV (независимо от R)
import csv, io
from collections import defaultdict

BASE = "labs/lab-04/output/Groceries_results/"
N = 9835

def load(fn):
    raw = open(BASE + fn, "rb").read()
    raw.decode("utf-8")  # кодировка UTF-8 (иначе exception)
    rdr = csv.reader(io.StringIO(raw.decode("utf-8")))
    return list(rdr)

rep = []
def check(name, cond, detail=""):
    rep.append((name, "OK" if cond else "FAIL", detail))

for fn, nexp in [("business_rules.csv", 10736), ("strong_rules.csv", 255),
                 ("top50_rules_by_lift.csv", 50), ("item_frequencies.csv", 169)]:
    rows = load(fn)
    hdr, data = rows[0], rows[1:]
    check(f"{fn}: строк данных", len(data) == nexp, f"{len(data)} vs {nexp}")
    check(f"{fn}: без дубликатов ключа", len({r[0] for r in data}) == len(data))

# правила-файлы: поля и инварианты
for fn, filt in [("business_rules.csv", lambda s, c, l: s > 0.001 and c > 0.3 and l > 1.2),
                 ("strong_rules.csv",    lambda s, c, l: s > 0.005 and l > 2)]:
    rows = load(fn)[1:]
    check(f"{fn}: заголовок",
          load(fn)[0] == ["rules", "support", "confidence", "coverage", "lift", "count"])
    bad_f = [r[0] for r in rows if not filt(float(r[1]), float(r[2]), float(r[4]))]
    check(f"{fn}: все строки проходят фильтр", not bad_f, f"нарушений {len(bad_f)}")
    bad_cnt = [r[0] for r in rows if abs(int(r[5]) - round(float(r[1]) * N)) > 0]
    check(f"{fn}: count == round(support*N)", not bad_cnt, f"нарушений {len(bad_cnt)}")
    bad_conf = [r[0] for r in rows
                if abs(float(r[2]) - int(r[5]) / round(float(r[3]) * N)) > 1e-9]
    check(f"{fn}: confidence == count/(coverage*N)", not bad_conf, f"нарушений {len(bad_conf)}")
    # lift: confidence/support(rhs); support(rhs) константен по группе rhs
    g = defaultdict(set)
    for r in rows:
        rhs = r[0].split("=>")[1].strip()
        g[rhs].add(round(float(r[2]) / float(r[4]), 12))
    bad_g = {k: v for k, v in g.items() if len(v) > 1}
    check(f"{fn}: conf/lift одинаков внутри rhs-группы", not bad_g,
          f"групп с расхождением {len(bad_g)}")

# top50: lift нестрого убывает; максимум совпадает с бизнес-топом
t50 = load("top50_rules_by_lift.csv")[1:]
lifts = [float(r[4]) for r in t50]
check("top50: lift нестрого убывает", all(a >= b for a, b in zip(lifts, lifts[1:])))
biz = load("business_rules.csv")[1:]
check("top50[0] == business[0] (макс lift pruned)", t50[0][0] == biz[0][0], t50[0][0])

# item_frequencies: убывание, максимум, согласованность с N
fr = load("item_frequencies.csv")[1:]
cnts = [int(r[1]) for r in fr]
check("item_frequencies: count убывается", all(a >= b for a, b in zip(cnts, cnts[1:])))
check("item_frequencies: максимум whole milk=2513", fr[0] == ["whole milk", "2513"])
check("item_frequencies: все count в (0, N]", all(0 < c <= N for c in cnts))

# strong ⊆ business?
bset = {r[0] for r in biz}
srows = load("strong_rules.csv")[1:]
missing = [r[0] for r in srows if r[0] not in bset]
check("strong ⊆ business (по строке правила)", not missing,
      f"вне business: {len(missing)}; conf=0.3 ровно у них: "
      f"{sum(1 for r in srows if r[0] in missing and float(r[2]) == 0.3)}")

for name, st, d in rep:
    print(f"{st:4} | {name}" + (f" | {d}" if d else ""))
fails = [r for r in rep if r[1] == "FAIL"]
print()
print("ИТОГ:", "ВСЕ ПРОВЕРКИ OK" if not fails else f"FAIL: {len(fails)}")
