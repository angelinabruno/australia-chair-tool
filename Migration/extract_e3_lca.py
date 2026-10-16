"""Pull E-3 rows out of a US Department of Labor LCA disclosure workbook.

Called by '1g. Migration.R' once per new DOL file; the result is cached as a
small CSV, so this only runs when a new yearly file is added.

The DOL workbooks are 50-300 MB and too slow for R's Excel readers, so this
streams the sheet XML with the Python standard library (no packages needed)
and keeps only the columns the dashboard uses.

Usage: python extract_e3_lca.py <input.xlsx> <output.csv>
"""
import csv
import re
import sys
import zipfile
import xml.etree.ElementTree as ET

NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
REL = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}"

# Output column -> header names used across DOL's file layouts since FY2008
WANTED = {
    "case_number":    ["CASE_NUMBER", "LCA_CASE_NUMBER", "CASE_NO"],
    "received_date":  ["RECEIVED_DATE", "CASE_SUBMITTED", "LCA_CASE_SUBMIT", "SUBMITTED_DATE"],
    "decision_date":  ["DECISION_DATE", "LCA_CASE_DECISION_DATE", "DOL_DECISION_DATE", "CERTIFIED_DATE"],
    "visa_class":     ["VISA_CLASS", "PROGRAM", "PROGRAM_DESIGNATION"],
    "case_status":    ["CASE_STATUS", "STATUS", "APPROVAL_STATUS"],
    "soc_code":       ["SOC_CODE", "LCA_CASE_SOC_CODE"],
    "soc_title":      ["SOC_TITLE", "SOC_NAME", "LCA_CASE_SOC_NAME", "OCCUPATIONAL_TITLE"],
    "job_title":      ["JOB_TITLE", "LCA_CASE_JOB_TITLE"],
    "employer":       ["EMPLOYER_NAME", "LCA_CASE_EMPLOYER_NAME", "NAME"],
    "worksite_city":  ["WORKSITE_CITY", "WORKSITE_CITY_1", "LCA_CASE_WORKLOC1_CITY", "CITY_1", "WORK_LOCATION_CITY1"],
    "worksite_state": ["WORKSITE_STATE", "WORKSITE_STATE_1", "LCA_CASE_WORKLOC1_STATE", "STATE_1", "WORK_LOCATION_STATE1"],
    "positions":      ["TOTAL_WORKER_POSITIONS", "TOTAL_WORKERS", "TOTAL WORKERS", "NBR_IMMIGRANTS"],
}


def col_index(ref):
    """'AB12' -> 27 (zero-based column number)."""
    letters = re.match(r"[A-Z]+", ref).group(0)
    n = 0
    for ch in letters:
        n = n * 26 + (ord(ch) - 64)
    return n - 1


def first_sheet_path(z):
    wb = ET.fromstring(z.read("xl/workbook.xml"))
    rid = wb.find(f"{NS}sheets/{NS}sheet").get(f"{REL}id")
    rels = ET.fromstring(z.read("xl/_rels/workbook.xml.rels"))
    for r in rels:
        if r.get("Id") == rid:
            target = r.get("Target").lstrip("/")
            return target if target.startswith("xl/") else "xl/" + target
    raise RuntimeError("first sheet not found")


def shared_strings(z):
    if "xl/sharedStrings.xml" not in z.namelist():
        return []
    out = []
    with z.open("xl/sharedStrings.xml") as f:
        for _, el in ET.iterparse(f):
            if el.tag == f"{NS}si":
                out.append("".join(t.text or "" for t in el.iter(f"{NS}t")))
                el.clear()
    return out


def main(src, dest):
    with zipfile.ZipFile(src) as z:
        strings = shared_strings(z)
        sheet = first_sheet_path(z)
        cols = None          # output name -> column number
        kept = total = 0
        with z.open(sheet) as f, open(dest, "w", newline="", encoding="utf-8") as out:
            w = csv.writer(out)
            w.writerow(list(WANTED))
            for _, el in ET.iterparse(f):
                if el.tag != f"{NS}row":
                    continue
                vals = {}
                for c in el.iter(f"{NS}c"):
                    v = c.find(f"{NS}v")
                    if v is None:
                        t = c.find(f"{NS}is/{NS}t")
                        text = t.text if t is not None else None
                    else:
                        text = strings[int(v.text)] if c.get("t") == "s" else v.text
                    if text is not None:
                        vals[col_index(c.get("r"))] = text.strip()
                el.clear()
                if cols is None:     # header row
                    names = {v.upper(): k for k, v in vals.items()}
                    cols = {}
                    for key, options in WANTED.items():
                        for o in options:
                            if o in names:
                                cols[key] = names[o]
                                break
                    if "visa_class" not in cols:
                        # Older H-1B-only files have no visa class column: nothing to keep
                        print(f"{src}: no visa class column; no E-3 rows", file=sys.stderr)
                        return
                    continue
                total += 1
                vc = vals.get(cols["visa_class"], "").upper().replace(" ", "")
                # FY2008-09 files code the program by letter: A = E-3 Australian
                # (R = H-1B, S = H-1B1 Singapore, C = H-1B1 Chile)
                if vc not in ("E-3AUSTRALIAN", "E-3", "E3", "E-3AUSTRALIA", "A"):
                    continue
                w.writerow([vals.get(cols[k], "") if k in cols else "" for k in WANTED])
                kept += 1
        print(f"{src}: {kept} E-3 rows of {total}", file=sys.stderr)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
