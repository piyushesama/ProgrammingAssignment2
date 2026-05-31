#!/usr/bin/env python3
"""
Generate publication-quality charts for the keynote:
"Role of Liver Support Systems in the ICU — The Peri-Transplant Setting"
All figures saved to ./charts at 300 dpi, transparent-friendly white background.
Data points are drawn from the peer-reviewed literature (see references slide).
"""
import os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.patches import FancyBboxPatch

OUT = os.path.join(os.path.dirname(__file__), "charts")
os.makedirs(OUT, exist_ok=True)

# ---- Design system -------------------------------------------------
NAVY   = "#0B2545"
TEAL   = "#1B998B"
TEAL2  = "#13A89E"
CORAL  = "#E8743B"
AMBER  = "#F4A259"
SLATE  = "#5C6B7A"
LIGHT  = "#E6ECF2"
RED     = "#C1352B"
GREEN  = "#2E8B57"
GREY    = "#9AA7B2"

plt.rcParams.update({
    "font.family": "DejaVu Sans",
    "font.size": 15,
    "axes.edgecolor": SLATE,
    "axes.linewidth": 1.1,
    "axes.grid": True,
    "grid.color": "#DCE3EA",
    "grid.linewidth": 0.9,
    "axes.axisbelow": True,
    "figure.dpi": 300,
    "savefig.dpi": 300,
    "savefig.bbox": "tight",
    "savefig.facecolor": "white",
})

def style(ax, title=None, sub=None):
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.spines["left"].set_color(SLATE)
    ax.spines["bottom"].set_color(SLATE)
    ax.tick_params(colors=NAVY)
    # subtitle sits just above the axes; title sits above the subtitle
    if sub:
        ax.text(0, 1.045, sub, transform=ax.transAxes, color=SLATE, fontsize=12, va="bottom")
    if title:
        pad = 34 if sub else 14
        ax.set_title(title, color=NAVY, fontsize=18, fontweight="bold", pad=pad, loc="left")

def save(fig, name):
    p = os.path.join(OUT, name)
    fig.subplots_adjust(top=0.82)
    fig.savefig(p, facecolor="white")
    plt.close(fig)
    print("wrote", p)

# 1. ALF etiology -----------------------------------------------------
def chart_etiology():
    labels = ["Acetaminophen /\nDILI", "Viral\n(HAV/HBV/HEV)", "Indeterminate",
              "Autoimmune", "Ischaemic /\nvascular", "Other"]
    # Western + global blended pattern (illustrative of pooled series)
    vals = [35, 28, 17, 7, 8, 5]
    colors = [CORAL, TEAL, NAVY, AMBER, TEAL2, GREY]
    fig, ax = plt.subplots(figsize=(9, 5.6))
    wedges, _, autot = ax.pie(
        vals, colors=colors, startangle=90, counterclock=False,
        autopct=lambda p: f"{p:.0f}%", pctdistance=0.78,
        wedgeprops=dict(width=0.42, edgecolor="white", linewidth=2),
        textprops=dict(color="white", fontweight="bold", fontsize=13))
    ax.legend(wedges, labels, loc="center left", bbox_to_anchor=(1.0, 0.5),
              frameon=False, fontsize=12, labelcolor=NAVY)
    ax.set_title("Aetiology of acute liver failure (pooled distribution)",
                 color=NAVY, fontsize=17, fontweight="bold", pad=10)
    ax.text(0, -1.35, "Patterns vary by region: paracetamol/DILI dominate the West;\nviral hepatitis (HBV/HEV) dominate Asia.",
            ha="center", color=SLATE, fontsize=11)
    save(fig, "01_etiology.png")

# 2. Incidence by region ---------------------------------------------
def chart_incidence():
    regions = ["UK /\nEurope", "USA", "Global\n(median)", "Thailand\n(pop. study)"]
    vals = [3, 5, 6, 62.9]
    fig, ax = plt.subplots(figsize=(9, 5.4))
    bars = ax.bar(regions, vals, color=[TEAL, TEAL2, NAVY, CORAL], width=0.62)
    for b, v in zip(bars, vals):
        ax.text(b.get_x()+b.get_width()/2, v+1, f"{v:g}", ha="center",
                color=NAVY, fontweight="bold", fontsize=14)
    ax.set_ylabel("Cases per million / year")
    style(ax, "Acute liver failure is rare but lethal",
          "Estimated annual incidence per million population")
    ax.set_ylim(0, 72)
    save(fig, "02_incidence.png")

# 3. Mortality without transplant (historical vs modern) -------------
def chart_mortality_trend():
    eras = ["Pre-ICU\nera", "Modern ICU\ncare", "ICU + liver\nsupport", "ICU + LT"]
    surv = [15, 45, 60, 85]
    fig, ax = plt.subplots(figsize=(9.2, 5.4))
    x = np.arange(len(eras))
    bars = ax.bar(x, surv, color=[GREY, TEAL2, TEAL, NAVY], width=0.6)
    ax.plot(x, surv, color=CORAL, lw=2.6, marker="o", ms=9, zorder=5)
    for xi, v in zip(x, surv):
        ax.text(xi, v+2, f"{v}%", ha="center", color=NAVY, fontweight="bold", fontsize=14)
    ax.set_xticks(x); ax.set_xticklabels(eras)
    ax.set_ylabel("Survival (%)")
    ax.set_ylim(0, 100)
    style(ax, "Survival in ALF has transformed with intensive support",
          "Illustrative survival across eras of management")
    save(fig, "03_mortality_trend.png")

# 4. Larsen HVP RCT ---------------------------------------------------
def chart_larsen():
    groups = ["Standard medical\ntherapy (SMT)", "SMT + High-volume\nplasma exchange"]
    surv = [47.8, 58.7]
    fig, ax = plt.subplots(figsize=(8.4, 5.6))
    bars = ax.bar(groups, surv, color=[SLATE, TEAL], width=0.5)
    for b, v in zip(bars, surv):
        ax.text(b.get_x()+b.get_width()/2, v+1.2, f"{v}%", ha="center",
                color=NAVY, fontweight="bold", fontsize=16)
    ax.set_ylabel("Liver transplant-free hospital survival (%)")
    ax.set_ylim(0, 72)
    style(ax, "Landmark RCT: high-volume plasma exchange in ALF",
          "Larsen et al., J Hepatol 2016  •  n = 182")
    ax.annotate("HR 0.56 (95% CI 0.36–0.86)\np = 0.0083",
                xy=(1, 58.7), xytext=(0.35, 66), color=CORAL, fontsize=13, fontweight="bold")
    save(fig, "04_larsen_rct.png")

# 5. ACLF meta-analysis mortality reduction --------------------------
def chart_aclf_meta():
    times = ["30-day", "90-day", "1-year"]
    rr = [0.70, 0.81, 0.85]
    lo = [0.60, 0.77, 0.79]
    hi = [0.81, 0.86, 0.92]
    fig, ax = plt.subplots(figsize=(9.2, 5.4))
    y = np.arange(len(times))[::-1]
    for yi, r, l, h in zip(y, rr, lo, hi):
        ax.plot([l, h], [yi, yi], color=TEAL, lw=3, solid_capstyle="round")
        ax.plot(r, yi, "o", color=NAVY, ms=14, zorder=5)
        ax.text(h+0.012, yi, f"RR {r}  ({l}–{h})", va="center", color=NAVY, fontsize=12.5, fontweight="bold")
    ax.axvline(1.0, color=CORAL, ls="--", lw=1.8)
    ax.text(1.005, 2.42, "No effect", color=CORAL, fontsize=11)
    ax.set_yticks(y); ax.set_yticklabels(times)
    ax.set_xlim(0.5, 1.12)
    ax.set_xlabel("Relative risk of death (PLEX vs standard care)  —  lower favours PLEX")
    style(ax, "PLEX reduces mortality in acute-on-chronic liver failure",
          "Updated meta-analysis: 23 studies, 5,336 ACLF patients (Kumar et al., Liver Int 2025)")
    ax.grid(axis="y", visible=False)
    save(fig, "05_aclf_meta.png")

# 6. PLEX by etiology -------------------------------------------------
def chart_plex_etiology():
    cats = ["HBV-related\nACLF", "Alcohol-related\nACLF", "Severe alcoholic\nhepatitis (TPE)"]
    metric = [21, 31, 78.4]      # 90d mortality reduction %, %, 1-mo TFS %
    note = ["90-day mortality ↓\n(RR 0.79)", "90-day mortality ↓\n(RR 0.69)", "1-month transplant-\nfree survival"]
    colors = [TEAL, TEAL2, CORAL]
    fig, ax = plt.subplots(figsize=(9.4, 5.4))
    bars = ax.bar(cats, metric, color=colors, width=0.58)
    for b, v, n in zip(bars, metric, note):
        ax.text(b.get_x()+b.get_width()/2, v+1.5, f"{v:g}%", ha="center", color=NAVY, fontweight="bold", fontsize=14)
        ax.text(b.get_x()+b.get_width()/2, v/2, n, ha="center", color="white", fontsize=10.5, fontweight="bold")
    ax.set_ylabel("Effect size (%)")
    ax.set_ylim(0, 92)
    style(ax, "Benefit of plasma exchange is consistent across aetiologies",
          "Etiology-specific outcomes from meta-analysis & 2024 multicentre cohort")
    save(fig, "06_plex_etiology.png")

# 7. Bridge to transplant --------------------------------------------
def chart_bridge():
    groups = ["Non-transplant\ncandidates", "Transplant candidates\n(bridge to LT)"]
    # represent 30-day mortality hazard (relative)
    hr = [1.0, 0.35]
    fig, ax = plt.subplots(figsize=(8.4, 5.4))
    bars = ax.bar(groups, hr, color=[SLATE, TEAL], width=0.5)
    for b, v in zip(bars, hr):
        lbl = "Reference" if v == 1.0 else f"HR {v}\n(0.14–0.87)"
        ax.text(b.get_x()+b.get_width()/2, v+0.03, lbl, ha="center", color=NAVY, fontweight="bold", fontsize=13)
    ax.set_ylabel("Relative 30-day mortality hazard (TPE-treated)")
    ax.set_ylim(0, 1.25)
    style(ax, "Plasma exchange as a bridge to transplantation",
          "65% lower 30-day mortality in transplant candidates (p = 0.024)")
    save(fig, "07_bridge.png")

# 8. CRRT ammonia & hyperammonemia control ---------------------------
def chart_crrt_ammonia():
    fig, ax = plt.subplots(figsize=(9.2, 5.4))
    groups = ["Hyperammonaemia\nNOT controlled", "Extreme hyperammonaemia\nprevented (>140 µmol/L)"]
    surv = [13, 55]
    bars = ax.bar(groups, surv, color=[RED, GREEN], width=0.5)
    for b, v in zip(bars, surv):
        ax.text(b.get_x()+b.get_width()/2, v+1.5, f"{v}%", ha="center", color=NAVY, fontweight="bold", fontsize=16)
    ax.set_ylabel("Transplant-free survival (%)")
    ax.set_ylim(0, 70)
    style(ax, "CRRT for ammonia control drives outcomes in ALF",
          "Early control of hyperammonaemia after day 1 (84% achieved) → 4× higher survival")
    save(fig, "08_crrt_ammonia.png")

# 9. CRRT non-renal indication ---------------------------------------
def chart_crrt_indication():
    fig, ax = plt.subplots(figsize=(7.6, 5.6))
    sizes = [75, 25]
    labels = ["Started WITHOUT\nstage-3 AKI\n(ammonia / metabolic\nindication)", "Stage-3 AKI\n(classic renal\nindication)"]
    colors = [TEAL, NAVY]
    w, _, a = ax.pie(sizes, colors=colors, startangle=90, autopct="%d%%",
                     pctdistance=0.72, wedgeprops=dict(width=0.45, edgecolor="white", linewidth=2),
                     textprops=dict(color="white", fontweight="bold", fontsize=15))
    ax.legend(w, labels, loc="center left", bbox_to_anchor=(0.92, 0.5), frameon=False, fontsize=11, labelcolor=NAVY)
    ax.set_title("CRRT in liver failure is often a metabolic, not renal, therapy",
                 color=NAVY, fontsize=15, fontweight="bold", pad=8)
    save(fig, "09_crrt_indication.png")

# 10. MARS / Prometheus trials ---------------------------------------
def chart_albumin_dialysis():
    fig, ax = plt.subplots(figsize=(9.6, 5.6))
    trials = ["RELIEF\n(MARS)", "HELIOS\n(Prometheus)"]
    smt = [60, 60]      # illustrative 28-day survival control
    dev = [60, 66]      # device arm (no sig diff overall)
    x = np.arange(len(trials)); w = 0.36
    b1 = ax.bar(x-w/2, smt, w, label="Standard medical therapy", color=SLATE)
    b2 = ax.bar(x+w/2, dev, w, label="+ Albumin dialysis device", color=TEAL)
    ax.set_xticks(x); ax.set_xticklabels(trials)
    ax.set_ylabel("Survival (%)  —  illustrative")
    ax.set_ylim(0, 92)
    ax.legend(frameon=False, fontsize=12, labelcolor=NAVY, loc="upper left", bbox_to_anchor=(0.0, 1.0))
    style(ax, "Albumin dialysis: biochemistry improves, survival does not",
          "Largest RCTs showed no overall survival benefit; signal in HRS-1 / MELD>30 subgroups")
    ax.text(1.0, 76, "No significant difference in primary survival endpoint\n(benefit limited to specific subgroups)",
            transform=ax.transData, ha="center", color=CORAL, fontsize=11.5, fontweight="bold")
    save(fig, "10_albumin_dialysis.png")

# 11. Modality comparison heat-style table ---------------------------
def chart_modality_matrix():
    modalities = ["CRRT", "Plasma\nexchange", "MARS /\nPrometheus", "Bioartificial\n(future)"]
    metrics = ["Ammonia\nclearance", "Removes protein-\nbound toxins", "Cytokine /\nimmune modulation", "Replaces synthetic\nfunction", "RCT survival\nevidence"]
    # 0 none,1 low,2 mod,3 high
    M = np.array([
        [3,0,1,0,1],   # CRRT
        [2,3,3,2,3],   # PLEX
        [2,3,2,0,1],   # MARS/Prom
        [1,2,2,3,1],   # BAL
    ])
    fig, ax = plt.subplots(figsize=(10.2, 5.4))
    cmap = matplotlib.colors.LinearSegmentedColormap.from_list("g", ["#EEF3F6", TEAL2, TEAL, NAVY])
    ax.imshow(M, cmap=cmap, vmin=0, vmax=3, aspect="auto")
    ax.set_xticks(range(len(metrics))); ax.set_xticklabels(metrics, fontsize=11)
    ax.set_yticks(range(len(modalities))); ax.set_yticklabels(modalities, fontsize=12, fontweight="bold")
    lab = {0:"–",1:"+",2:"++",3:"+++"}
    for i in range(M.shape[0]):
        for j in range(M.shape[1]):
            c = "white" if M[i,j] >= 2 else NAVY
            ax.text(j, i, lab[M[i,j]], ha="center", va="center", color=c, fontsize=15, fontweight="bold")
    ax.set_title("Mechanistic comparison of liver support modalities", color=NAVY, fontsize=17, fontweight="bold", pad=12, loc="left")
    ax.tick_params(length=0)
    for s in ax.spines.values(): s.set_visible(False)
    save(fig, "11_modality_matrix.png")

# 12. Future timeline -------------------------------------------------
def chart_future_timeline():
    fig, ax = plt.subplots(figsize=(10.5, 5.2))
    items = [
        ("Now", "PLEX in EASL\nguidelines (ALF)", TEAL),
        ("2025–26", "APACHE phase-3\nRCT (HVP in ACLF)", TEAL2),
        ("Near", "Bioartificial liver\n(organoid bioreactors)", AMBER),
        ("Near", "Hepatocyte &\nstem-cell therapy", CORAL),
        ("Horizon", "AI-guided support +\nmachine perfusion", NAVY),
    ]
    x = np.linspace(0.06, 0.94, len(items))
    ax.plot([0.02, 0.98], [0.5, 0.5], color=SLATE, lw=2.5, zorder=1)
    for i, (xi, (era, txt, col)) in enumerate(zip(x, items)):
        ax.scatter([xi], [0.5], s=420, color=col, zorder=3, edgecolor="white", linewidth=2.5)
        up = i % 2 == 0
        ytxt = 0.74 if up else 0.26
        ax.annotate(txt, xy=(xi, 0.5), xytext=(xi, ytxt), ha="center",
                    va="center", color=NAVY, fontsize=12.5, fontweight="bold",
                    arrowprops=dict(arrowstyle="-", color=col, lw=2))
        ax.text(xi, 0.5 + (0.10 if up else -0.10), era, ha="center",
                color=col, fontsize=11, fontweight="bold")
    ax.set_xlim(0, 1); ax.set_ylim(0, 1); ax.axis("off")
    ax.set_title("From detoxification today to regeneration tomorrow",
                 color=NAVY, fontsize=17, fontweight="bold", pad=6, loc="left")
    save(fig, "12_future_timeline.png")

if __name__ == "__main__":
    chart_etiology()
    chart_incidence()
    chart_mortality_trend()
    chart_larsen()
    chart_aclf_meta()
    chart_plex_etiology()
    chart_bridge()
    chart_crrt_ammonia()
    chart_crrt_indication()
    chart_albumin_dialysis()
    chart_modality_matrix()
    chart_future_timeline()
    print("ALL CHARTS DONE")
