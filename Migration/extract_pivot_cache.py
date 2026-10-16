"""Write the data behind an Excel pivot table out as a plain CSV.

Home Affairs publishes its visa statistics on data.gov.au as Excel pivot
tables: the visible sheet only shows totals, and the record-level data sits in
the workbook's pivot cache. This reads that cache with the Python standard
library (no packages needed). Called by '1g. Migration.R' when a new Home
Affairs file is downloaded; the CSV is cached.

Usage: python extract_pivot_cache.py <input.xlsx> <output.csv> [field filter]
  field filter (optional): FIELD=VALUE, e.g. "Citizenship Country=United States of America",
  keeps only matching records so the CSV stays small.
"""
import csv
import sys
import zipfile
import xml.etree.ElementTree as ET

NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"


def item_value(el):
    tag = el.tag.replace(NS, "")
    if tag == "m":
        return ""
    return el.get("v", "")


def main(src, dest, filt=None):
    with zipfile.ZipFile(src) as z:
        defs = sorted(n for n in z.namelist() if n.startswith("xl/pivotCache/pivotCacheDefinition"))
        if not defs:
            raise SystemExit(f"{src}: no pivot cache found")
        # Use the largest cache (the main pivot table)
        best = max(defs, key=lambda n: z.getinfo(n.replace("Definition", "Records")).file_size
                   if n.replace("Definition", "Records") in z.namelist() else 0)
        d = ET.fromstring(z.read(best))
        fields, shared = [], []
        for cf in d.iter(f"{NS}cacheField"):
            fields.append(cf.get("name"))
            si = cf.find(f"{NS}sharedItems")
            shared.append([item_value(x) for x in list(si)] if si is not None else [])
        f_idx = f_val = None
        if filt:
            name, val = filt.split("=", 1)
            f_idx = fields.index(name)
            f_val = val
        rec_name = best.replace("Definition", "Records")
        kept = total = 0
        with z.open(rec_name) as f, open(dest, "w", newline="", encoding="utf-8") as out:
            w = csv.writer(out)
            w.writerow(fields)
            for _, el in ET.iterparse(f):
                if el.tag != f"{NS}r":
                    continue
                row = []
                for i, c in enumerate(list(el)):
                    tag = c.tag.replace(NS, "")
                    if tag == "x":
                        items = shared[i]
                        k = int(c.get("v"))
                        row.append(items[k] if k < len(items) else "")
                    else:
                        row.append(item_value(c))
                el.clear()
                total += 1
                if f_idx is not None and (f_idx >= len(row) or row[f_idx] != f_val):
                    continue
                w.writerow(row)
                kept += 1
        print(f"{src}: {kept} of {total} records; fields: {fields}", file=sys.stderr)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else None)
