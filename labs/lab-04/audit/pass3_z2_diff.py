"""Аудит Задачи 2, Проход 3: сопоставление базового кода преподавателя
("Skript koda ... Magazin Alko ispr.docx") с выполненным скриптом
task2_magazin_ovoshchi_frukty.R. Только чтение; вывод — консоль.

Метод: извлечь строки базового кода из docx, нормализовать (HTML-entity,
пробелы), разметить блоки по заголовкам «# N. ...», затем для каждого
блока посчитать долю строк, присутствующих в выполненном скрипте
(точное вхождение нормализованной строки)."""
import re
import zipfile

BASE_DOCX = ("assignments/lab-04/Skript koda k Laboratornomu praktikumu "
             "4_Magazin Alko ispr.docx")
WORK_R = "labs/lab-04/task2_magazin_ovoshchi_frukty.R"

# --- извлечение базового кода ---
xml = zipfile.ZipFile(BASE_DOCX).read("word/document.xml").decode("utf-8")
paras = re.split(r"</w:p>", xml)
lines = []
for p in paras:
    t = "".join(re.findall(r"<w:t[^>]*>([^<]*)</w:t>", p))
    t = (t.replace("&lt;", "<").replace("&gt;", ">").replace("&amp;", "&")
          .replace("&quot;", '"'))
    lines.append(t)

# --- нормализация строки для сопоставления ---
def norm(s: str) -> str:
    s = re.sub(r"#.*", "", s)                # комментарии прочь
    s = re.sub(r"\s+", " ", s).strip()
    return s

base_lines = [l for l in lines if norm(l)]
work_src = open(WORK_R, encoding="utf-8").read()
work_norms = set(norm(l) for l in work_src.splitlines() if norm(l))

# --- блоки базового кода ---
blocks = []
cur_name, cur = "(шапка)", []
for l in base_lines:
    m = re.match(r"#\s*(\d+[\wа-я.]*\.?\s*.*)", l)
    if m and len(norm(l)) < 60:
        blocks.append((cur_name, cur)); cur_name, cur = l.strip(), []
    else:
        cur.append(l)
blocks.append((cur_name, cur))

print(f"базовый код: {len(base_lines)} значимых строк; блоков: {len(blocks)}\n")
print(f"{'блок':52s} {'строк':>6s} {'найдено':>8s} {'доля':>6s}")
total = found = 0
detail_missing = []
for name, blk in blocks:
    hits = 0
    missing = []
    for l in blk:
        n = norm(l)
        if not n:
            continue
        total += 1
        # точное вхождение или с заменой Алкоголь/Гвоздика-переменных
        variants = [n]
        if "Алкоголь" in n:
            variants.append(n.replace("Алкоголь", "Овощи - Фрукты"))
        if any(v in work_norms for v in variants):
            hits += 1
        else:
            missing.append(n)
    found += hits
    if blk:
        print(f"{name[:52]:52s} {len(blk):6d} {hits:8d} {hits/len(blk):6.0%}")
        if missing:
            detail_missing.append((name, missing))

print(f"\nИТОГО: {found}/{total} строк базового кода присутствуют "
      f"в выполненном ({found/total:.0%})")

print("\n--- ненайденные строки (по блокам, до 8 на блок) ---")
for name, missing in detail_missing:
    print(f"\n[{name}]")
    for m in missing:
        print("  -", m[:110])
