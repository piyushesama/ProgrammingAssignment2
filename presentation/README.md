# BUYING TIME — Liver Support Systems in the ICU (TED-style keynote)

A 24-slide, cinematic **TED-talk-style** keynote on the *Role of Liver Support
Systems in the ICU — the peri-transplant setting* (PLEX & CRRT focus).

## Deliverable
- **`Liver_Support_ICU_TED_Keynote.pptx`** — full-bleed 16:9, opens in Keynote,
  PowerPoint or Google Slides. Each slide is a designed full-bleed image.

## Design
- Cinematic near-black canvas, one big idea per slide, dramatic display type
  (Anton + Fraunces + Inter), custom vector data-viz (no stock charts).
- Narrative arc: the problem → the big idea → CRRT → PLEX → the honest slide →
  the playbook → what just changed → the future → takeaways → close.

## How it's built (fully reproducible)
- `ted_engine.py`   — SVG slide engine + signature "liver-as-network" art.
- `build_ted_deck.py` — authors all 24 slides as SVG, renders to 2560×1440 PNGs.
- `assemble_pptx.py`  — packs the PNGs into a full-bleed .pptx.

```bash
pip install cairosvg python-pptx pillow
# fonts: place Anton, Fraunces, Inter, Bebas Neue .ttf in ./fonts and `cp` to ~/.fonts
python3 build_ted_deck.py && python3 assemble_pptx.py
```

## Data sources
Larsen FS et al. *J Hepatol* 2016 (HVP RCT) · Maiwall R et al. *Clin Gastroenterol
Hepatol* 2022 (standard-volume PLEX) · Kumar R et al. *Liver Int* 2025 (ACLF
meta-analysis) · RELIEF (MARS, *Hepatology* 2013) · HELIOS (Prometheus,
*Gastroenterology* 2012) · Cardoso FS et al. (CRRT & ammonia) · EASL CPG on
acute liver failure · APACHE trial (NCT03702920).

> ⚠️ Some figures (e.g. the "500+ jobs" and pre-ICU survival framing) are
> illustrative/teaching devices. Verify all numbers against the primary
> sources before clinical or formal use.

## Note
The earlier conventional version (`Liver_Support_ICU_Keynote.pptx` + `charts/`)
is retained for reference.
