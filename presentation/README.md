# Role of Liver Support Systems in the ICU — The Peri-Transplant Setting

A 31-slide, 16:9 keynote (focus on **PLEX** and **CRRT**) covering recent advances and
futuristic directions in extracorporeal liver support.

## Deliverable
- **`Liver_Support_ICU_Keynote.pptx`** — open in **Keynote**, **PowerPoint**, or **Google Slides**.
  Fully editable: text, colours and the embedded charts can all be changed.

## How it was built
- `make_charts.py` — generates the 12 data visualizations (matplotlib, 300 dpi) into `charts/`.
- `build_deck.py` — assembles the slide deck (python-pptx) with a consistent design system.

Rebuild anytime:
```bash
pip install python-pptx matplotlib numpy
python3 make_charts.py && python3 build_deck.py
```

## Data sources (selected)
- Larsen FS et al. High-volume plasma exchange in ALF — RCT. *J Hepatol* 2016.
- Maiwall R et al. Standard-volume plasma exchange in ALF — RCT. *Clin Gastroenterol Hepatol* 2022.
- Kumar R et al. TPE in ACLF improves survival — updated meta-analysis. *Liver Int* 2025.
- Bañares R et al. RELIEF (MARS). *Hepatology* 2013;  Kribben A et al. HELIOS (Prometheus). *Gastroenterology* 2012.
- Cardoso FS et al. CRRT & hyperammonaemia in ALF.
- EASL CPG on acute (fulminant) liver failure;  APACHE trial (NCT03702920).

## ⚠️ Note on the figures
Charts are built from published trial and meta-analysis figures. A few values
(e.g. the era-by-era survival bar chart and the MARS/Prometheus survival bars)
are **illustrative for teaching** rather than exact reproductions. Please verify
all numbers against the primary sources before any clinical or formal use.
