#!/usr/bin/env python3
"""
Build the keynote:
"Role of Liver Support Systems in the ICU — The Peri-Transplant Setting"
~30 slides, 16:9, data- and visualization-driven. Outputs Liver_Support_ICU_Keynote.pptx
Opens natively in Keynote, PowerPoint and Google Slides.
"""
import os
from pptx import Presentation
from pptx.util import Inches, Pt, Emu
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.enum.shapes import MSO_SHAPE
from pptx.oxml.ns import qn

HERE = os.path.dirname(__file__)
CH = os.path.join(HERE, "charts")

# ---- palette --------------------------------------------------------
NAVY  = RGBColor(0x0B, 0x25, 0x45)
NAVY2 = RGBColor(0x13, 0x33, 0x5C)
TEAL  = RGBColor(0x1B, 0x99, 0x8B)
TEAL2 = RGBColor(0x13, 0xA8, 0x9E)
CORAL = RGBColor(0xE8, 0x74, 0x3B)
AMBER = RGBColor(0xF4, 0xA2, 0x59)
SLATE = RGBColor(0x5C, 0x6B, 0x7A)
LIGHT = RGBColor(0xF4, 0xF7, 0xFA)
LIGHT2= RGBColor(0xE6, 0xEC, 0xF2)
WHITE = RGBColor(0xFF, 0xFF, 0xFF)
INK   = RGBColor(0x1A, 0x2A, 0x39)

FONT = "Calibri"          # widely available; maps cleanly in Keynote
FONTH = "Calibri"

prs = Presentation()
prs.slide_width = Inches(13.333)
prs.slide_height = Inches(7.5)
SW, SH = prs.slide_width, prs.slide_height
BLANK = prs.slide_layouts[6]

def slide():
    return prs.slides.add_slide(BLANK)

def bg(s, color):
    s.background.fill.solid()
    s.background.fill.fore_color.rgb = color

def rect(s, x, y, w, h, color, line=None, shape=MSO_SHAPE.RECTANGLE, shadow=False):
    sp = s.shapes.add_shape(shape, x, y, w, h)
    sp.fill.solid(); sp.fill.fore_color.rgb = color
    if line is None:
        sp.line.fill.background()
    else:
        sp.line.color.rgb = line; sp.line.width = Pt(1)
    sp.shadow.inherit = False
    if shadow:
        el = sp._element.spPr
        ef = el.makeelement(qn('a:effectLst'), {})
        sh = ef.makeelement(qn('a:outerShdw'),
              {'blurRad':'90000','dist':'40000','dir':'5400000','rotWithShape':'0'})
        c = sh.makeelement(qn('a:srgbClr'), {'val':'0B2545'})
        a = c.makeelement(qn('a:alpha'), {'val':'24000'})
        c.append(a); sh.append(c); ef.append(sh); el.append(ef)
    return sp

def txt(s, x, y, w, h, runs, align=PP_ALIGN.LEFT, anchor=MSO_ANCHOR.TOP,
        space_after=6, line_spacing=1.0):
    """runs: list of paragraphs; each paragraph is list of (text,size,color,bold,italic)"""
    tb = s.shapes.add_textbox(x, y, w, h)
    tf = tb.text_frame; tf.word_wrap = True
    tf.vertical_anchor = anchor
    tf.margin_left = 0; tf.margin_right = 0; tf.margin_top = 0; tf.margin_bottom = 0
    for i, para in enumerate(runs):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.alignment = align
        p.space_after = Pt(space_after); p.space_before = Pt(0)
        p.line_spacing = line_spacing
        for (t, sz, col, bold, ital) in para:
            r = p.add_run(); r.text = t
            r.font.size = Pt(sz); r.font.color.rgb = col
            r.font.bold = bold; r.font.italic = ital; r.font.name = FONT
    return tb

def R(t, sz, col, bold=False, ital=False):
    return (t, sz, col, bold, ital)

def kicker(s, x, y, text, color=TEAL):
    rect(s, x, y+Inches(0.02), Inches(0.34), Inches(0.07), color)
    txt(s, x+Inches(0.46), y-Inches(0.10), Inches(8), Inches(0.4),
        [[R(text.upper(), 13, color, True)]])

def pagenum(s, n):
    txt(s, SW-Inches(1.0), SH-Inches(0.5), Inches(0.7), Inches(0.3),
        [[R(f"{n:02d}", 11, SLATE, True)]], align=PP_ALIGN.RIGHT)

def footer(s):
    txt(s, Inches(0.6), SH-Inches(0.5), Inches(9), Inches(0.3),
        [[R("Role of Liver Support Systems in the ICU  •  Peri-Transplant Setting", 9, SLATE, False)]])

def bullet(s, x, y, w, items, gap=0.62, size=15.5, marker_col=TEAL):
    for i, it in enumerate(items):
        yy = y + Inches(gap*i)
        rect(s, x, yy+Inches(0.10), Inches(0.13), Inches(0.13), marker_col, shape=MSO_SHAPE.OVAL)
        if isinstance(it, tuple):
            head, body = it
            txt(s, x+Inches(0.34), yy, w-Inches(0.34), Inches(gap),
                [[R(head+"  ", size, NAVY, True), R(body, size, INK)]], line_spacing=1.0)
        else:
            txt(s, x+Inches(0.34), yy, w-Inches(0.34), Inches(gap),
                [[R(it, size, INK)]], line_spacing=1.0)

def stat_card(s, x, y, w, h, big, label, accent=TEAL):
    rect(s, x, y, w, h, WHITE, shadow=True)
    rect(s, x, y, Inches(0.10), h, accent)
    txt(s, x+Inches(0.28), y+Inches(0.18), w-Inches(0.4), Inches(0.7),
        [[R(big, 33, accent, True)]])
    txt(s, x+Inches(0.30), y+h-Inches(0.78), w-Inches(0.5), Inches(0.7),
        [[R(label, 12.5, SLATE, False)]], line_spacing=0.95)

def content_header(s, n, kick, title, sub=None):
    bg(s, WHITE)
    rect(s, 0, 0, SW, Inches(0.16), TEAL)
    kicker(s, Inches(0.6), Inches(0.5), kick)
    rows = [[R(title, 30, NAVY, True)]]
    txt(s, Inches(0.6), Inches(0.86), Inches(12), Inches(0.9), rows)
    if sub:
        txt(s, Inches(0.6), Inches(1.5), Inches(12.1), Inches(0.5),
            [[R(sub, 15, SLATE, False, True)]])
    footer(s); pagenum(s, n)

def chart(s, name, x, y, w, h=None):
    p = os.path.join(CH, name)
    if h is not None:
        s.shapes.add_picture(p, x, y, height=h)
    else:
        s.shapes.add_picture(p, x, y, width=w)

# =====================================================================
# SLIDE 1 — TITLE
# =====================================================================
s = slide(); bg(s, NAVY)
rect(s, 0, 0, SW, SH, NAVY)
# decorative band
rect(s, 0, Inches(4.55), SW, Inches(0.10), TEAL)
rect(s, Inches(0.0), Inches(4.65), Inches(4.4), Inches(0.06), CORAL)
kicker(s, Inches(0.9), Inches(1.25), "ICU • Hepatology • Transplant Critical Care", TEAL2)
txt(s, Inches(0.9), Inches(1.9), Inches(11.5), Inches(2.2),
    [[R("Role of Liver Support Systems", 46, WHITE, True)],
     [R("in the ICU", 46, WHITE, True)]], line_spacing=1.02)
txt(s, Inches(0.9), Inches(3.75), Inches(11), Inches(0.8),
    [[R("The Peri-Transplant Setting — with a focus on PLEX & CRRT", 22, AMBER, False, True)]])
txt(s, Inches(0.9), Inches(4.95), Inches(11), Inches(1.0),
    [[R("Recent advances and futuristic directions in extracorporeal liver support", 16, LIGHT2, False)]])
txt(s, Inches(0.9), Inches(6.35), Inches(11), Inches(0.6),
    [[R("Prepared for Dr. Amit  •  Gleneagles  •  ", 13, SLATE, False),
      R("Keynote", 13, TEAL2, True)]])

# =====================================================================
# SLIDE 2 — AGENDA / ROADMAP
# =====================================================================
s = slide(); content_header(s, 2, "Roadmap", "What this talk covers")
cards = [
    ("01", "The clinical problem", "ALF, ACLF & the peri-transplant window", TEAL),
    ("02", "Why support the liver?", "Toxins, immune storm & multi-organ failure", TEAL2),
    ("03", "CRRT", "Ammonia control & metabolic rescue", CORAL),
    ("04", "Plasma exchange (PLEX)", "The strongest survival evidence", TEAL),
    ("05", "Albumin dialysis", "MARS / Prometheus — lessons learned", AMBER),
    ("06", "Future directions", "Bioartificial liver, cells & AI", NAVY2),
]
x0, y0 = Inches(0.6), Inches(2.1)
cw, chh, gx, gy = Inches(3.95), Inches(2.25), Inches(0.27), Inches(0.3)
for i, (num, head, body, col) in enumerate(cards):
    cx = x0 + (cw+gx)*(i % 3)
    cy = y0 + (chh+gy)*(i // 3)
    rect(s, cx, cy, cw, chh, LIGHT, shadow=True)
    rect(s, cx, cy, cw, Inches(0.10), col)
    txt(s, cx+Inches(0.3), cy+Inches(0.28), cw, Inches(0.8), [[R(num, 30, col, True)]])
    txt(s, cx+Inches(0.3), cy+Inches(1.05), cw-Inches(0.5), Inches(0.5), [[R(head, 17, NAVY, True)]])
    txt(s, cx+Inches(0.3), cy+Inches(1.5), cw-Inches(0.55), Inches(0.7), [[R(body, 13, SLATE, False)]], line_spacing=0.95)

# =====================================================================
# SLIDE 3 — THE CLINICAL PROBLEM
# =====================================================================
s = slide(); content_header(s, 3, "The clinical problem",
    "Liver failure is a multi-organ emergency",
    "Rapid loss of hepatic function → encephalopathy, coagulopathy and circulatory collapse")
bullet(s, Inches(0.7), Inches(2.2), Inches(6.6), [
    ("Detoxification fails —", "ammonia, bilirubin, bile acids & toxins accumulate"),
    ("Synthesis fails —", "coagulopathy, hypoalbuminaemia, hypoglycaemia"),
    ("Immune dysregulation —", "a systemic inflammatory / cytokine storm"),
    ("Multi-organ failure —", "brain, kidney, circulation & lungs follow"),
    ("The liver can regenerate —", "if the patient survives the acute hit"),
], gap=0.82, size=16)
# right stat column
stat_card(s, Inches(7.7), Inches(2.15), Inches(5.0), Inches(1.45), "Hours–days", "Window from presentation to irreversible multi-organ failure", CORAL)
stat_card(s, Inches(7.7), Inches(3.75), Inches(5.0), Inches(1.45), "2 goals", "Bridge to recovery  OR  bridge to transplantation", TEAL)
txt(s, Inches(7.7), Inches(5.45), Inches(5.0), Inches(1.2),
    [[R("Liver support buys time — ", 15, NAVY, True),
      R("either for the native liver to regenerate, or for a graft to become available.",15, INK)]], line_spacing=1.05)

# =====================================================================
# SLIDE 4 — DEFINITIONS ALF vs ACLF
# =====================================================================
s = slide(); content_header(s, 4, "Definitions", "ALF vs ACLF vs the peri-transplant patient")
defs = [
    ("Acute liver failure (ALF)", "Sudden failure in a previously healthy liver. Coagulopathy + encephalopathy within weeks. Often a single insult (paracetamol, virus).", TEAL),
    ("Acute-on-chronic liver failure (ACLF)", "Acute decompensation on cirrhosis with organ failures and high short-term mortality. Driven by systemic inflammation.", CORAL),
    ("Peri-transplant ICU patient", "On the waiting list or post-graft — needing organ support to stay a viable transplant candidate or to recover the new graft.", NAVY2),
]
y = Inches(2.15)
for head, body, col in defs:
    rect(s, Inches(0.6), y, Inches(12.1), Inches(1.45), LIGHT, shadow=True)
    rect(s, Inches(0.6), y, Inches(0.12), Inches(1.45), col)
    txt(s, Inches(0.95), y+Inches(0.18), Inches(11.4), Inches(0.5), [[R(head, 19, col, True)]])
    txt(s, Inches(0.95), y+Inches(0.66), Inches(11.5), Inches(0.7), [[R(body, 15, INK, False)]], line_spacing=1.0)
    y += Inches(1.66)

# =====================================================================
# SLIDE 5 — EPIDEMIOLOGY (chart)
# =====================================================================
s = slide(); content_header(s, 5, "Epidemiology", "Rare, but with very high mortality")
chart(s, "02_incidence.png", Inches(0.5), Inches(2.0), Inches(7.4))
stat_card(s, Inches(8.3), Inches(2.1), Inches(4.4), Inches(1.35), "~1–6 / million", "Annual ALF incidence in the developed world", TEAL)
stat_card(s, Inches(8.3), Inches(3.6), Inches(4.4), Inches(1.35), "27%", "30-day mortality in a population-based ALF cohort", CORAL)
stat_card(s, Inches(8.3), Inches(5.1), Inches(4.4), Inches(1.35), "<0.01%", "Received a transplant where access is limited", NAVY2)

# =====================================================================
# SLIDE 6 — AETIOLOGY (chart)
# =====================================================================
s = slide(); content_header(s, 6, "Aetiology", "Causes drive the therapeutic choice")
chart(s, "01_etiology.png", Inches(0.4), Inches(1.95), Inches(7.6))
bullet(s, Inches(8.2), Inches(2.3), Inches(4.6), [
    ("Paracetamol / DILI —", "the dominant Western cause; supportive ± NAC"),
    ("Viral (HBV, HEV) —", "dominant across Asia; antiviral + support"),
    ("Indeterminate —", "a large subgroup; high transplant need"),
    ("Immune & vascular —", "may respond to PLEX / steroids"),
], gap=1.0, size=14.5)

# =====================================================================
# SLIDE 7 — MORTALITY TREND (chart)
# =====================================================================
s = slide(); content_header(s, 7, "Why it matters", "Intensive support has transformed survival")
chart(s, "03_mortality_trend.png", Inches(0.7), Inches(2.0), Inches(8.0))
txt(s, Inches(9.0), Inches(2.3), Inches(3.8), Inches(3.5),
    [[R("Each layer of organ support — ", 15.5, NAVY, True),
      R("modern ICU care, then extracorporeal liver support, then transplantation — ", 15.5, INK),
      R("has incrementally improved survival in liver failure.", 15.5, INK)]], line_spacing=1.15)

# =====================================================================
# SLIDE 8 — SECTION DIVIDER: rationale
# =====================================================================
def divider(n, kick, big, sub, accent=TEAL):
    s = slide(); bg(s, NAVY)
    rect(s, Inches(0.9), Inches(2.6), Inches(0.9), Inches(0.10), accent)
    txt(s, Inches(0.9), Inches(2.0), Inches(11), Inches(0.5), [[R(kick.upper(), 15, accent, True)]])
    txt(s, Inches(0.9), Inches(2.95), Inches(11.5), Inches(1.6), [[R(big, 40, WHITE, True)]], line_spacing=1.0)
    txt(s, Inches(0.9), Inches(4.6), Inches(10.5), Inches(1.0), [[R(sub, 18, LIGHT2, False, True)]], line_spacing=1.1)
    pagenum(s, n)
    return s

divider(8, "Mechanism", "The rationale for\nextracorporeal liver support",
        "Remove what the failing liver can't — and modulate the inflammatory storm")

# =====================================================================
# SLIDE 9 — WHAT WE TARGET
# =====================================================================
s = slide(); content_header(s, 9, "Targets", "What liver support actually removes")
targets = [
    ("Water-soluble toxins", "Ammonia, urea, small molecules", "→ CRRT", TEAL),
    ("Protein-bound toxins", "Bilirubin, bile acids, aromatic amino acids", "→ PLEX / albumin dialysis", CORAL),
    ("Inflammatory mediators", "Cytokines, DAMPs, chemokines", "→ PLEX (immune reset)", NAVY2),
    ("Missing factors", "Coagulation factors, albumin, opsonins", "→ PLEX replaces them", AMBER),
]
x0 = Inches(0.6); cw = Inches(2.95); gx = Inches(0.18)
for i,(h,b,tag,col) in enumerate(targets):
    cx = x0 + (cw+gx)*i
    rect(s, cx, Inches(2.2), cw, Inches(3.6), LIGHT, shadow=True)
    rect(s, cx, Inches(2.2), cw, Inches(0.85), col)
    txt(s, cx+Inches(0.22), Inches(2.36), cw-Inches(0.4), Inches(0.6), [[R(h, 15.5, WHITE, True)]], line_spacing=0.95)
    txt(s, cx+Inches(0.22), Inches(3.25), cw-Inches(0.42), Inches(1.8), [[R(b, 14, INK)]], line_spacing=1.1)
    txt(s, cx+Inches(0.22), Inches(5.25), cw-Inches(0.4), Inches(0.5), [[R(tag, 14, col, True)]])
txt(s, Inches(0.6), Inches(6.05), Inches(12), Inches(0.6),
    [[R("Key idea:  ", 15, CORAL, True), R("no single device replaces the liver — modalities are combined to cover different toxin classes.", 15, NAVY, False)]])

# =====================================================================
# SLIDE 10 — CLASSIFICATION (matrix chart)
# =====================================================================
s = slide(); content_header(s, 10, "Classification", "A mechanistic map of the options")
chart(s, "11_modality_matrix.png", Inches(2.55), Inches(2.0), None, h=Inches(4.45))
txt(s, Inches(0.8), Inches(6.6), Inches(12), Inches(0.5),
    [[R("Artificial (cell-free): CRRT, PLEX, MARS, Prometheus    •    Bioartificial (cell-based): the emerging frontier", 13.5, SLATE, False, True)]])

# =====================================================================
# SLIDE 11 — SECTION: CRRT
# =====================================================================
divider(11, "Modality 01", "CRRT", "Continuous renal replacement therapy — the metabolic workhorse of the liver ICU", accent=CORAL)

# =====================================================================
# SLIDE 12 — CRRT principles & indications
# =====================================================================
s = slide(); content_header(s, 12, "CRRT", "Principles & indications in liver failure")
bullet(s, Inches(0.7), Inches(2.15), Inches(6.4), [
    ("Continuous & gentle —", "slow solute removal suits the haemodynamically fragile"),
    ("Ammonia control —", "the key non-renal indication in ALF"),
    ("Avoids ICP swings —", "preferred over intermittent HD in cerebral oedema"),
    ("Manages AKI / HRS —", "fluid, acid-base & electrolyte control"),
    ("Platform for combination —", "runs alongside PLEX in many protocols"),
], gap=0.82, size=15.5)
stat_card(s, Inches(7.7), Inches(2.15), Inches(5.0), Inches(1.5),
          "Start early", "CRRT is often begun BEFORE stage-3 AKI — to control ammonia, not just for the kidneys", CORAL)
stat_card(s, Inches(7.7), Inches(3.85), Inches(5.0), Inches(1.5),
          "CVVHDF", "High-flow continuous veno-venous haemodiafiltration maximises ammonia clearance", TEAL)
txt(s, Inches(7.7), Inches(5.55), Inches(5.0), Inches(1.2),
    [[R("Ammonia clearance is lower than urea/creatinine ", 14.5, NAVY, True),
      R("→ higher effluent doses are used in hyperammonaemia.", 14.5, INK)]], line_spacing=1.05)

# =====================================================================
# SLIDE 13 — CRRT ammonia survival (chart)
# =====================================================================
s = slide(); content_header(s, 13, "CRRT • Evidence", "Controlling ammonia changes outcomes")
chart(s, "08_crrt_ammonia.png", Inches(0.5), Inches(2.0), Inches(7.7))
stat_card(s, Inches(8.5), Inches(2.2), Inches(4.2), Inches(1.5), "84%", "Achieved control of extreme hyperammonaemia (>140 µmol/L) after day 1 with CRRT", TEAL)
stat_card(s, Inches(8.5), Inches(3.9), Inches(4.2), Inches(1.5), "4×", "Higher transplant-free survival when extreme hyperammonaemia was prevented (55% vs 13%)", CORAL)

# =====================================================================
# SLIDE 14 — CRRT non-renal indication (chart)
# =====================================================================
s = slide(); content_header(s, 14, "CRRT • Insight", "Often a metabolic therapy, not a renal one")
chart(s, "09_crrt_indication.png", Inches(0.7), Inches(1.95), Inches(6.6))
bullet(s, Inches(7.6), Inches(2.4), Inches(5.1), [
    ("Three-quarters started", "without stage-3 AKI — driven by ammonia & encephalopathy"),
    ("Neuroprotection", "lowering ammonia reduces intracranial hypertension risk"),
    ("Peri-transplant", "stabilises HRS and volume status before / after graft"),
], gap=1.05, size=15)

# =====================================================================
# SLIDE 15 — SECTION: PLEX
# =====================================================================
divider(15, "Modality 02", "Plasma exchange", "Therapeutic plasma exchange (PLEX / TPE) — the strongest survival evidence to date", accent=TEAL)

# =====================================================================
# SLIDE 16 — PLEX mechanism
# =====================================================================
s = slide(); content_header(s, 16, "PLEX", "One therapy, three simultaneous actions")
acts = [
    ("REMOVE", "Clears protein-bound toxins, bilirubin, bile acids & aromatic amino acids", TEAL),
    ("REPLACE", "Restores coagulation factors, albumin & opsonins via fresh plasma", CORAL),
    ("RESET", "Dampens the cytokine / DAMP-driven inflammatory storm", NAVY2),
]
x0 = Inches(0.7); cw = Inches(3.85); gx = Inches(0.28)
for i,(h,b,col) in enumerate(acts):
    cx = x0+(cw+gx)*i
    rect(s, cx, Inches(2.3), cw, Inches(2.9), LIGHT, shadow=True)
    rect(s, cx, Inches(2.3), cw, Inches(0.95), col)
    txt(s, cx, Inches(2.46), cw, Inches(0.7), [[R(h, 26, WHITE, True)]], align=PP_ALIGN.CENTER)
    txt(s, cx+Inches(0.28), Inches(3.5), cw-Inches(0.56), Inches(1.5), [[R(b, 15.5, INK)]], line_spacing=1.15)
txt(s, Inches(0.7), Inches(5.5), Inches(12), Inches(1.2),
    [[R("Unlike CRRT or albumin dialysis, PLEX is the only modality that simultaneously detoxifies, replaces synthetic function and modulates immunity — ", 16, NAVY, True),
      R("which likely explains its survival signal.", 16, INK)]], line_spacing=1.1)

# =====================================================================
# SLIDE 17 — Larsen RCT (chart)
# =====================================================================
s = slide(); content_header(s, 17, "PLEX • Landmark trial", "High-volume PLEX improves survival in ALF")
chart(s, "04_larsen_rct.png", Inches(0.6), Inches(2.0), Inches(7.2))
bullet(s, Inches(8.1), Inches(2.3), Inches(4.7), [
    ("First RCT of PLEX in ALF", "182 patients; J Hepatol 2016"),
    ("+11% absolute survival", "transplant-free survival vs standard care"),
    ("Mechanism confirmed", "attenuated innate immune activation"),
    ("Now in EASL guidance", "Level I, Grade 1 recommendation for ALF"),
], gap=1.0, size=14.5)

# =====================================================================
# SLIDE 18 — Standard vs high volume
# =====================================================================
s = slide(); content_header(s, 18, "PLEX • Refinement", "Standard-volume PLEX may be enough")
bullet(s, Inches(0.7), Inches(2.2), Inches(11.8), [
    ("High-volume PLEX (HVP) —", "8–12 L/day exchange established the survival benefit in the landmark trial"),
    ("Standard-volume PLEX —", "a later RCT showed comparable improvement in outcomes with less plasma"),
    ("Practical implications —", "lower plasma requirement, fewer resources, better feasibility in most ICUs"),
    ("Timing is critical —", "earlier initiation is associated with better response — 'the battle can be lost if late'"),
], gap=0.95, size=16)
rect(s, Inches(0.7), Inches(6.3), Inches(11.9), Inches(0.85), LIGHT, shadow=True)
txt(s, Inches(1.0), Inches(6.46), Inches(11.4), Inches(0.6),
    [[R("Take-home:  ", 15.5, CORAL, True),
      R("efficacy is robust across HVP and standard-volume protocols — favour earlier, resource-appropriate PLEX.", 15.5, NAVY)]])

# =====================================================================
# SLIDE 19 — ACLF meta-analysis (chart)
# =====================================================================
s = slide(); content_header(s, 19, "PLEX • ACLF evidence", "Mortality benefit out to one year")
chart(s, "05_aclf_meta.png", Inches(0.5), Inches(2.0), Inches(8.4))
stat_card(s, Inches(9.1), Inches(2.2), Inches(3.6), Inches(1.45), "5,336", "ACLF patients across 23 studies in the pooled analysis", TEAL)
stat_card(s, Inches(9.1), Inches(3.8), Inches(3.6), Inches(1.45), "30% ↓", "Relative risk of death at 30 days with PLEX", CORAL)
stat_card(s, Inches(9.1), Inches(5.4), Inches(3.6), Inches(1.3), "Golden 2 wks", "APASL window for early TPE in ACLF", NAVY2)

# =====================================================================
# SLIDE 20 — PLEX by etiology (chart)
# =====================================================================
s = slide(); content_header(s, 20, "PLEX • Subgroups", "Consistent across major aetiologies")
chart(s, "06_plex_etiology.png", Inches(0.7), Inches(2.0), Inches(8.0))
txt(s, Inches(9.0), Inches(2.4), Inches(3.8), Inches(3.8),
    [[R("Benefit is seen in HBV- and alcohol-related ACLF, and a 2024 multicentre cohort reported ", 15, INK),
      R("78% one-month transplant-free survival ", 15, CORAL, True),
      R("in severe alcoholic hepatitis treated with PLEX.", 15, INK)]], line_spacing=1.2)

# =====================================================================
# SLIDE 21 — Bridge to transplant (chart)
# =====================================================================
s = slide(); content_header(s, 21, "PLEX • Peri-transplant", "PLEX as a bridge to transplantation")
chart(s, "07_bridge.png", Inches(0.6), Inches(2.0), Inches(7.0))
bullet(s, Inches(8.0), Inches(2.3), Inches(4.8), [
    ("Keeps candidates viable", "controls HE, coagulopathy & inflammation while waiting"),
    ("65% lower 30-day mortality", "in transplant candidates vs non-candidates (p=0.024)"),
    ("ABO-incompatible grafts", "PLEX lowers isoagglutinin titres peri-operatively"),
    ("Bridge to recovery too", "buys time for native liver regeneration"),
], gap=1.0, size=14.5)

# =====================================================================
# SLIDE 22 — SECTION: albumin dialysis
# =====================================================================
divider(22, "Modality 03", "Albumin dialysis", "MARS & Prometheus — strong biochemistry, elusive survival benefit", accent=AMBER)

# =====================================================================
# SLIDE 23 — MARS / Prometheus (chart)
# =====================================================================
s = slide(); content_header(s, 23, "Albumin dialysis", "What the big RCTs taught us")
chart(s, "10_albumin_dialysis.png", Inches(0.5), Inches(2.0), Inches(7.8))
bullet(s, Inches(8.6), Inches(2.3), Inches(4.2), [
    ("RELIEF (MARS)", "largest ACLF RCT — no overall survival benefit"),
    ("HELIOS (Prometheus)", "lower bilirubin, no 28/90-day survival gain"),
    ("Subgroups benefit", "HRS-1 and MELD >30 signals"),
    ("Role today", "selected refractory cases; pruritus, HE relief"),
], gap=1.0, size=14)

# =====================================================================
# SLIDE 24 — COMPARATIVE SUMMARY TABLE
# =====================================================================
s = slide(); content_header(s, 24, "Synthesis", "Choosing the right tool for the patient")
rows = [
    ["Modality", "Best for", "Key evidence", "Peri-transplant role"],
    ["CRRT", "Ammonia, AKI, ICP, volume", "↑ survival via ammonia control", "Workhorse — pre/post graft"],
    ["PLEX / TPE", "Toxins + immunity + factors", "RCT & meta-analysis survival benefit", "Bridge to LT / recovery"],
    ["MARS / Prometheus", "Refractory jaundice, HE, pruritus", "Biochemistry ↑, survival neutral", "Selected subgroups"],
    ["Bioartificial", "Synthetic function (future)", "Investigational", "Emerging"],
]
nrows, ncols = len(rows), 4
tx, ty = Inches(0.6), Inches(2.1)
tw, th = Inches(12.1), Inches(4.4)
gtbl = s.shapes.add_table(nrows, ncols, tx, ty, tw, th).table
colw = [Inches(2.5), Inches(3.5), Inches(3.6), Inches(2.5)]
for j,w in enumerate(colw): gtbl.columns[j].width = w
# disable default style banding via first row formatting
for i in range(nrows):
    gtbl.rows[i].height = Inches(0.88) if i else Inches(0.6)
    for j in range(ncols):
        c = gtbl.cell(i,j)
        c.margin_left = Inches(0.12); c.margin_right = Inches(0.08)
        c.margin_top = Inches(0.05); c.margin_bottom = Inches(0.05)
        c.vertical_anchor = MSO_ANCHOR.MIDDLE
        tf = c.text_frame; tf.word_wrap = True
        p = tf.paragraphs[0]; r = p.add_run(); r.text = rows[i][j]
        r.font.name = FONT
        if i == 0:
            c.fill.solid(); c.fill.fore_color.rgb = NAVY
            r.font.color.rgb = WHITE; r.font.bold = True; r.font.size = Pt(15)
        else:
            c.fill.solid(); c.fill.fore_color.rgb = LIGHT if i % 2 else WHITE
            r.font.color.rgb = INK; r.font.size = Pt(13.5)
            if j == 0: r.font.bold = True; r.font.color.rgb = TEAL
# accent line under header handled by table

# =====================================================================
# SLIDE 25 — INTEGRATED ALGORITHM
# =====================================================================
s = slide(); content_header(s, 25, "Algorithm", "An integrated peri-transplant pathway")
steps = [
    ("Recognise", "ALF / ACLF\n+ organ failures", TEAL),
    ("Stabilise", "ICU care, NAC,\nantivirals, sepsis Rx", TEAL2),
    ("Support", "CRRT for ammonia/AKI\n+ PLEX for toxins/immunity", CORAL),
    ("Decide", "Recovery likely?\nList for transplant?", AMBER),
    ("Bridge / Transplant", "PLEX bridge → LT\nor native recovery", NAVY2),
]
x0 = Inches(0.55); cw = Inches(2.3); gx = Inches(0.18); y = Inches(2.7)
for i,(h,b,col) in enumerate(steps):
    cx = x0+(cw+gx)*i
    rect(s, cx, y, cw, Inches(2.0), LIGHT, shadow=True)
    rect(s, cx, y, cw, Inches(0.6), col)
    txt(s, cx, y+Inches(0.12), cw, Inches(0.4), [[R(h, 14.5, WHITE, True)]], align=PP_ALIGN.CENTER)
    txt(s, cx+Inches(0.16), y+Inches(0.75), cw-Inches(0.3), Inches(1.1), [[R(b, 12.5, INK)]], align=PP_ALIGN.CENTER, line_spacing=1.05)
    if i < len(steps)-1:
        ar = rect(s, cx+cw-Inches(0.02), y+Inches(0.85), gx+Inches(0.06), Inches(0.3), col, shape=MSO_SHAPE.RIGHT_ARROW)
txt(s, Inches(0.6), Inches(5.3), Inches(12), Inches(1.0),
    [[R("Throughout: ", 15.5, CORAL, True),
      R("treat the precipitant, support every failing organ, and re-assess transplant candidacy daily.", 15.5, NAVY)]])

# =====================================================================
# SLIDE 26 — RECENT ADVANCES
# =====================================================================
s = slide(); content_header(s, 26, "Recent advances", "What's changed in the last few years")
adv = [
    ("PLEX enters guidelines", "High-volume PLEX is now a Grade-1 recommendation for ALF", TEAL),
    ("Standard-volume PLEX", "Comparable benefit with fewer resources — broader feasibility", TEAL2),
    ("ACLF survival data matures", "Large 2025 meta-analysis confirms 30-/90-day & 1-year benefit", CORAL),
    ("Ammonia-targeted CRRT", "Early, high-dose CRRT for hyperammonaemia gains traction", AMBER),
    ("Bridge-to-LT evidence", "PLEX improves outcomes in listed candidates", NAVY2),
    ("Etiology-specific use", "Strong signals in alcohol-associated & HBV ACLF", SLATE),
]
x0,y0 = Inches(0.6), Inches(2.15); cw=Inches(3.95); chh=Inches(2.0); gx=Inches(0.27); gy=Inches(0.28)
for i,(h,b,col) in enumerate(adv):
    cx = x0+(cw+gx)*(i%3); cy = y0+(chh+gy)*(i//3)
    rect(s, cx, cy, cw, chh, LIGHT, shadow=True)
    rect(s, cx, cy, Inches(0.1), chh, col)
    txt(s, cx+Inches(0.3), cy+Inches(0.25), cw-Inches(0.5), Inches(0.7), [[R(h, 16, NAVY, True)]], line_spacing=0.95)
    txt(s, cx+Inches(0.3), cy+Inches(1.0), cw-Inches(0.55), Inches(1.0), [[R(b, 13, INK)]], line_spacing=1.05)

# =====================================================================
# SLIDE 27 — SECTION: FUTURE
# =====================================================================
divider(27, "Looking ahead", "Futuristic directions", "From detoxification today to true liver replacement and regeneration tomorrow", accent=TEAL2)

# =====================================================================
# SLIDE 28 — FUTURE TIMELINE (chart)
# =====================================================================
s = slide(); content_header(s, 28, "The frontier", "Where liver support is heading")
chart(s, "12_future_timeline.png", Inches(2.0), Inches(1.95), None, h=Inches(4.05))
bullet(s, Inches(0.8), Inches(6.15), Inches(11.8), [
    ("Bioartificial liver —", "organoid / hepatocyte bioreactors that add synthetic function, not just detoxification"),
], gap=0.5, size=14.5)

# =====================================================================
# SLIDE 29 — ONGOING TRIALS / FUTURE
# =====================================================================
s = slide(); content_header(s, 29, "On the horizon", "Trials & technologies to watch")
fut = [
    ("APACHE phase-3 RCT", "High-volume PLEX with 5% albumin in moderate-severe ACLF — could redefine the standard of care.", CORAL),
    ("Bioartificial liver devices", "3-D hepatocyte/organoid bioreactors aim to replace synthetic function, not just detoxify.", TEAL),
    ("Cell-based & gene therapy", "Hepatocyte transplantation and stem-cell-derived liver cells as a bridge or alternative to LT.", NAVY2),
    ("AI-guided & perfusion tech", "Machine perfusion expands the donor pool; AI may individualise support timing & dosing.", AMBER),
]
y = Inches(2.1)
for h,b,col in fut:
    rect(s, Inches(0.6), y, Inches(12.1), Inches(1.12), LIGHT, shadow=True)
    rect(s, Inches(0.6), y, Inches(0.12), Inches(1.12), col)
    txt(s, Inches(0.95), y+Inches(0.13), Inches(11.5), Inches(0.45), [[R(h, 17, col, True)]])
    txt(s, Inches(0.95), y+Inches(0.56), Inches(11.5), Inches(0.5), [[R(b, 14, INK)]], line_spacing=1.0)
    y += Inches(1.28)

# =====================================================================
# SLIDE 30 — KEY TAKEAWAYS
# =====================================================================
s = slide(); bg(s, NAVY)
rect(s, 0, 0, SW, Inches(0.16), TEAL)
txt(s, Inches(0.9), Inches(0.7), Inches(11), Inches(0.5), [[R("KEY TAKEAWAYS", 15, TEAL2, True)]])
txt(s, Inches(0.9), Inches(1.15), Inches(11.5), Inches(0.9), [[R("Five messages to take to the bedside", 30, WHITE, True)]])
takeaways = [
    ("Buy time, target the toxin.", "Liver support bridges to recovery or transplant — match the modality to the toxin class."),
    ("PLEX has the strongest evidence.", "Survival benefit in ALF (RCT) and ACLF (meta-analysis) — start early."),
    ("CRRT is a metabolic therapy.", "Use it for ammonia and ICP control, not only for the kidneys."),
    ("Albumin dialysis: pick your patient.", "Biochemistry improves; reserve for selected refractory subgroups."),
    ("The future is regenerative.", "Bioartificial livers, cell therapy and AI-guided support are coming."),
]
y = Inches(2.25)
for i,(h,b) in enumerate(takeaways):
    rect(s, Inches(0.9), y+Inches(0.06), Inches(0.5), Inches(0.5), TEAL if i%2==0 else CORAL, shape=MSO_SHAPE.OVAL)
    txt(s, Inches(0.9), y+Inches(0.13), Inches(0.5), Inches(0.4), [[R(str(i+1), 16, WHITE, True)]], align=PP_ALIGN.CENTER)
    txt(s, Inches(1.6), y, Inches(11), Inches(0.9),
        [[R(h+"  ", 17, WHITE, True), R(b, 15, LIGHT2)]], line_spacing=1.0)
    y += Inches(0.86)
pagenum(s, 30)

# =====================================================================
# SLIDE 31 — REFERENCES / THANK YOU
# =====================================================================
s = slide(); content_header(s, 31, "Evidence base", "Key references")
refs = [
    "Larsen FS et al. High-volume plasma exchange in acute liver failure: an open RCT. J Hepatol 2016;64:69–78.",
    "Maiwall R et al. Standard-volume plasma exchange improves outcomes in ALF: an RCT. Clin Gastroenterol Hepatol 2022.",
    "Kumar R et al. Therapeutic plasma exchange in ACLF improves survival — an updated meta-analysis. Liver Int 2025.",
    "Bañares R et al. (RELIEF) Extracorporeal albumin dialysis with MARS in ACLF. Hepatology 2013.",
    "Kribben A et al. (HELIOS) Effects of Prometheus in patients with ACLF. Gastroenterology 2012.",
    "Cardoso FS et al. CRRT & hyperammonaemia in acute liver failure. Crit Care / J Hepatol reports.",
    "EASL Clinical Practice Guidelines on the management of acute (fulminant) liver failure. J Hepatol.",
    "APACHE trial (NCT03702920): plasma exchange with albumin 5% in ACLF.",
]
y = Inches(2.0)
for r in refs:
    rect(s, Inches(0.65), y+Inches(0.07), Inches(0.1), Inches(0.1), TEAL, shape=MSO_SHAPE.OVAL)
    txt(s, Inches(0.95), y, Inches(11.6), Inches(0.5), [[R(r, 13, INK)]], line_spacing=0.98)
    y += Inches(0.52)
txt(s, Inches(0.95), y+Inches(0.15), Inches(11.5), Inches(0.6),
    [[R("Figures generated from published trial & meta-analysis data; some values are illustrative for teaching. Verify before clinical use.", 11.5, SLATE, False, True)]])

out = os.path.join(HERE, "Liver_Support_ICU_Keynote.pptx")
prs.save(out)
print("SAVED", out, "—", len(prs.slides._sldIdLst), "slides")
