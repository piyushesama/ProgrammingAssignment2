#!/usr/bin/env python3
"""
Build the TED-style keynote:
  "BUYING TIME — How We Keep a Dying Liver Alive Long Enough to Save It"
  (Role of Liver Support Systems in the ICU — the peri-transplant setting)

Every slide is a bespoke full-bleed composition. Renders 24 PNGs (2560x1440)
then assembles a full-bleed .pptx.  Run:  python3 build_ted_deck.py
"""
import os, math, random
from ted_engine import (SVG, grad, rgrad, soft_glow, vignette, particles,
    kicker, footer, liver_network,
    W, H, INK, INK2, PANEL, TEAL, TEAL_D, AMBER, CORAL, RED, WHITE, MUTE, MUTE2, GOLD,
    ANTON, FRAUN, INTER, BEBAS)

HERE = os.path.dirname(os.path.abspath(__file__))
SLIDES = []   # ordered list of rendered file paths
TOTAL = 24

def base_defs(svg):
    svg.add_def(soft_glow("g1", 6))
    svg.add_def(soft_glow("g2", 14))
    svg.add_def(soft_glow("gbig", 36))
    svg.add_def(grad("liverfill", [("0%", TEAL, 0.16),("100%", INK, 0.0)], 0,0,1,1))

def new(bg=INK):
    s = SVG(bg=bg); base_defs(s); return s

def reg(svg, name):
    p = svg.render(name); SLIDES.append(p); return p

# backdrop helpers ---------------------------------------------------
def aurora(svg, cx, cy, color, r=520, op=0.5):
    gid = f"au{len(svg.defs)}"
    svg.add_def(rgrad(gid, [("0%", color, op),("70%", color, 0)]))
    svg.add(f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="url(#{gid})"/>')

def topbar(svg, color=TEAL):
    svg.rect(0,0,W,5,color)

def grid_dots(svg, color=MUTE2, gap=46, op=0.10):
    for x in range(70, W-40, gap):
        for y in range(90, H-60, gap):
            svg.circle(x, y, 0.9, color, opacity=op)

# =====================================================================
# 01 — COLD OPEN / TITLE
# =====================================================================
def s01():
    s = new(INK)
    aurora(s, 980, 300, TEAL_D, 620, 0.42)
    aurora(s, 1120, 600, AMBER, 360, 0.16)
    particles(s, 90, seed=11, color=TEAL, op=0.5)
    liver_network(s, 980, 360, scale=1.7, seed=4)
    # left dark gradient for text legibility
    s.add_def(grad("ltf", [("0%", INK, 1),("60%", INK, 0.65),("100%", INK, 0)], 0,0,1,0))
    s.rect(0,0,820,H,"url(#ltf)")
    topbar(s, TEAL)
    s.text(80, 150, "A TALK FOR THE LIVER ICU", 16, fill=TEAL, font=INTER, weight="700", spacing="5")
    s.text(76, 300, "BUYING", 150, fill=WHITE, font=ANTON, spacing="1")
    s.text(76, 440, "TIME", 150, fill=TEAL, font=ANTON, spacing="1")
    s.text(80, 520, "How we keep a dying liver alive", 30, fill=WHITE, font=FRAUN, italic=True)
    s.text(80, 562, "long enough to save it.", 30, fill=MUTE, font=FRAUN, italic=True)
    s.text(80, 640, "Liver Support Systems in the ICU  ·  the peri-transplant setting  ·  PLEX & CRRT",
           17, fill=MUTE2, font=INTER, weight="500", spacing="1")
    return reg(s, "01.png")

# =====================================================================
# 02 — THE CLOCK (hook)
# =====================================================================
def s02():
    s = new(INK)
    aurora(s, 640, 380, AMBER, 560, 0.20)
    particles(s, 60, seed=21, color=AMBER, op=0.4)
    # giant ticking ring
    cx, cy, r = 640, 360, 210
    s.circle(cx, cy, r, "none", stroke=PANEL, sw=14)
    # arc ~ 1/6 remaining
    import math
    a0=-90; a1=-90+300
    def pol(a): return (cx+r*math.cos(math.radians(a)), cy+r*math.sin(math.radians(a)))
    x0,y0=pol(a0); x1,y1=pol(a1)
    large = 1 if (a1-a0)%360>180 else 1
    s.path(f"M {x0:.1f} {y0:.1f} A {r} {r} 0 {large} 1 {x1:.1f} {y1:.1f}", stroke=AMBER, sw=14)
    # tick marks
    for i in range(60):
        a=math.radians(i*6-90)
        rr1=r+18; rr2=r+(30 if i%5==0 else 24)
        s.line(cx+rr1*math.cos(a), cy+rr1*math.sin(a), cx+rr2*math.cos(a), cy+rr2*math.sin(a),
               MUTE2, 1.5 if i%5==0 else 0.8, opacity=0.5)
    s.text(cx, cy-10, "HOURS", 26, fill=MUTE, font=INTER, weight="700", spacing="6", anchor="middle")
    s.text(cx, cy+70, "TO DAYS", 26, fill=MUTE, font=INTER, weight="700", spacing="6", anchor="middle")
    # right text
    s.text(820, 250, "When the liver fails,", 40, fill=WHITE, font=FRAUN, italic=True)
    s.text(820, 305, "the countdown is", 40, fill=WHITE, font=FRAUN, italic=True)
    s.text(820, 360, "measured in hours.", 40, fill=AMBER, font=FRAUN, italic=True, weight="600")
    s.text(820, 440, "Brain. Kidneys. Circulation. Lungs.", 19, fill=MUTE, font=INTER)
    s.text(820, 470, "They fail in sequence — fast.", 19, fill=MUTE, font=INTER)
    s.text(820, 540, "Our job: stop the clock long enough", 19, fill=TEAL, font=INTER, weight="600")
    s.text(820, 568, "for rescue or a transplant.", 19, fill=TEAL, font=INTER, weight="600")
    footer(s, 2, TOTAL, "the problem")
    return reg(s, "02.png")

# =====================================================================
# 03 — THE 500-JOB ORGAN
# =====================================================================
def s03():
    s = new(INK)
    grid_dots(s)
    kicker(s, 80, 120, "meet the patient's hardest-working organ")
    s.text(76, 250, "500+", 170, fill=TEAL, font=ANTON)
    s.text(80, 320, "jobs. one organ. no backup.", 30, fill=WHITE, font=FRAUN, italic=True)
    jobs = [("DETOX", "ammonia · drugs · toxins", TEAL),
            ("SYNTHESIS", "clotting factors · albumin", AMBER),
            ("METABOLISM", "glucose · lipids · energy", CORAL),
            ("IMMUNITY", "filters & calms the blood", TEAL)]
    x=80; y=420; bw=270; gap=20
    for i,(h,b,c) in enumerate(jobs):
        xx=x+(bw+gap)*i
        s.rect(xx, y, bw, 150, PANEL, rx=14)
        s.rect(xx, y, 6, 150, c, rx=3)
        s.text(xx+28, y+50, h, 24, fill=c, font=INTER, weight="800", spacing="1")
        s.text(xx+28, y+92, b, 16, fill=MUTE, font=INTER)
    s.text(80, 640, "Lose it, and every one of these fails at once. That is liver failure.",
           18, fill=MUTE, font=INTER, italic=True)
    footer(s, 3, TOTAL, "the problem")
    return reg(s, "03.png")

# =====================================================================
# 04 — THE CASCADE
# =====================================================================
def s04():
    s = new(INK)
    aurora(s, 300, 360, CORAL, 480, 0.16)
    kicker(s, 80, 120, "what failure looks like", CORAL)
    s.text(76, 200, "The domino fall", 64, fill=WHITE, font=ANTON)
    steps=[("LIVER", "detox & synthesis collapse", TEAL),
           ("BLOOD", "toxins rise · clotting fails", AMBER),
           ("BRAIN", "encephalopathy · swelling", CORAL),
           ("BODY", "kidneys, lungs, circulation", RED)]
    cx=170; cy=430; gap=290
    for i,(h,b,c) in enumerate(steps):
        x=cx+gap*i
        if i< len(steps)-1:
            s.line(x+70, cy, x+gap-70, cy, MUTE2, 2, opacity=0.5, dash="2 8")
            s.path(f"M {x+gap-78} {cy-7} L {x+gap-64} {cy} L {x+gap-78} {cy+7}", stroke=MUTE, sw=2)
        s.circle(x, cy, 60, INK2, stroke=c, sw=3)
        s.circle(x, cy, 60, c, opacity=0.10)
        s.text(x, cy+8, h, 22, fill=c, font=INTER, weight="800", anchor="middle", spacing="1")
        s.text(x, cy+120, b, 15, fill=MUTE, font=INTER, anchor="middle")
        # wrap second line manually if long handled by short text
    s.text(80, 650, "Multi-organ failure is the cause of death — not the liver alone.",
           19, fill=WHITE, font=FRAUN, italic=True)
    footer(s, 4, TOTAL, "the problem")
    return reg(s, "04.png")

# =====================================================================
# 05 — TWO KINDS OF FAILURE (split)
# =====================================================================
def s05():
    s = new(INK)
    # split canvas
    s.add_def(grad("lf", [("0%", TEAL_D, 0.22),("100%", INK, 0)],0,0,0,1))
    s.add_def(grad("rf", [("0%", CORAL, 0.20),("100%", INK, 0)],0,0,0,1))
    s.rect(0,0,W/2,H,"url(#lf)")
    s.rect(W/2,0,W/2,H,"url(#rf)")
    s.line(W/2,90,W/2,H-80,MUTE2,1,opacity=0.3)
    kicker(s, 80, 120, "two different emergencies")
    # left ALF
    s.text(80, 230, "ALF", 110, fill=TEAL, font=ANTON)
    s.text(80, 285, "ACUTE LIVER FAILURE", 18, fill=WHITE, font=INTER, weight="700", spacing="2")
    for i,t in enumerate(["A healthy liver, hit suddenly","Paracetamol · viruses · toxins",
                          "Days, not years","Can fully recover — if it survives"]):
        s.circle(92, 350+i*55-5, 3, TEAL); s.text(110, 350+i*55, t, 19, fill=MUTE, font=INTER)
    # right ACLF
    s.text(700, 230, "ACLF", 110, fill=CORAL, font=ANTON)
    s.text(700, 285, "ACUTE-ON-CHRONIC LIVER FAILURE", 18, fill=WHITE, font=INTER, weight="700", spacing="2")
    for i,t in enumerate(["Cirrhosis that suddenly tips over","Driven by systemic inflammation",
                          "Organ failures stack up","Brutal short-term mortality"]):
        s.circle(712, 350+i*55-5, 3, CORAL); s.text(730, 350+i*55, t, 19, fill=MUTE, font=INTER)
    s.text(W/2, 650, "Same ICU. Same machines. Different battle.", 20, fill=WHITE,
           font=FRAUN, italic=True, anchor="middle")
    footer(s, 5, TOTAL, "the problem")
    return reg(s, "05.png")

# =====================================================================
# 06 — THE BRUTAL MATH (big stat)
# =====================================================================
def s06():
    s = new(INK)
    aurora(s, 360, 360, CORAL, 560, 0.18)
    kicker(s, 80, 120, "the brutal math", CORAL)
    s.text(76, 360, "Without support,", 44, fill=WHITE, font=FRAUN, italic=True)
    s.text(76, 430, "most don't make it.", 44, fill=WHITE, font=FRAUN, italic=True)
    # big number lower-left
    s.text(76, 560, "15", 110, fill=TEAL, font=ANTON)
    s.text(210, 530, "in 100 survived", 22, fill=WHITE, font=INTER, weight="700")
    s.text(210, 562, "in the pre-ICU era.", 19, fill=MUTE, font=INTER)
    s.text(76, 625, "Rare — but historically, lethal.", 18, fill=MUTE, font=INTER, italic=True)
    # right: 10x10 survival grid
    ox, oy = 770, 200; cols=10; r=11; gx=42; gy=42
    survive=15; k=0
    for row in range(10):
        for col in range(10):
            x=ox+col*gx; y=oy+row*gy
            alive = k < survive
            s.circle(x,y,r, TEAL if alive else PANEL,
                     stroke=TEAL if alive else MUTE2, sw=1)
            k+=1
    s.text(ox, oy-28, "PRE-ICU ERA · 100 PATIENTS", 14, fill=MUTE, font=INTER, weight="700", spacing="2")
    footer(s, 6, TOTAL, "the problem")
    return reg(s, "06.png")

# =====================================================================
# 07 — THE TRANSPLANT GAP
# =====================================================================
def s07():
    s = new(INK)
    kicker(s, 80, 120, "the old answer wasn't enough")
    s.text(76, 210, "Transplant saves lives.", 52, fill=WHITE, font=ANTON)
    s.text(80, 270, "But the graft rarely arrives in time.", 26, fill=AMBER, font=FRAUN, italic=True)
    # two big arcs: need vs available
    s.text(330, 380, "NEED A LIVER", 16, fill=MUTE, font=INTER, weight="700", spacing="2", anchor="middle")
    s.text(950, 380, "GET ONE IN TIME", 16, fill=MUTE, font=INTER, weight="700", spacing="2", anchor="middle")
    # big circle vs tiny slice
    s.circle(330, 500, 90, CORAL, opacity=0.85)
    s.text(330, 515, "MANY", 26, fill=INK, font=INTER, weight="800", anchor="middle")
    s.circle(950, 500, 30, TEAL, opacity=0.9)
    s.text(950, 508, "FEW", 16, fill=INK, font=INTER, weight="800", anchor="middle")
    # arrow gap
    s.line(440, 500, 900, 500, MUTE2, 2, dash="2 10")
    s.text(670, 478, "THE GAP", 16, fill=AMBER, font=INTER, weight="800", spacing="3", anchor="middle")
    s.text(80, 650, "Between listing and transplant lies a deadly waiting game. Something has to hold the line.",
           18, fill=MUTE, font=INTER, italic=True)
    footer(s, 7, TOTAL, "the problem")
    return reg(s, "07.png")

# =====================================================================
# 08 — THE BIG IDEA (manifesto)
# =====================================================================
def s08():
    s = new(INK)
    aurora(s, 640, 360, TEAL_D, 680, 0.4)
    particles(s, 80, seed=33, color=TEAL, op=0.5)
    s.text(W/2, 250, "THE BIG IDEA", 16, fill=TEAL, font=INTER, weight="700", spacing="6", anchor="middle")
    s.text(W/2, 360, "Don't replace the liver.", 60, fill=WHITE, font=ANTON, anchor="middle")
    s.text(W/2, 440, "Borrow its time.", 60, fill=TEAL, font=ANTON, anchor="middle")
    s.text(W/2, 520, "Extracorporeal liver support buys the hours and days the body needs",
           20, fill=MUTE, font=INTER, anchor="middle")
    s.text(W/2, 552, "to recover — or to reach a transplant.", 20, fill=MUTE, font=INTER, anchor="middle")
    footer(s, 8, TOTAL, "the idea")
    return reg(s, "08.png")

# =====================================================================
# 09 — WHAT'S KILLING THEM (toxins + storm)
# =====================================================================
def s09():
    s = new(INK)
    kicker(s, 80, 120, "what we're actually fighting")
    s.text(76, 200, "Two enemies in the blood", 54, fill=WHITE, font=ANTON)
    # left card toxins
    s.rect(80, 280, 520, 320, PANEL, rx=18)
    s.rect(80, 280, 520, 70, TEAL_D, rx=18)
    s.rect(80, 320, 520, 30, PANEL)
    s.text(110, 326, "THE POISONS", 26, fill=WHITE, font=INTER, weight="800", spacing="1")
    for i,(t,c) in enumerate([("Ammonia → attacks the brain", TEAL),
                              ("Bilirubin & bile acids", TEAL),
                              ("Protein-bound toxins", AMBER),
                              ("Aromatic amino acids", AMBER)]):
        s.circle(118, 400+i*46-5, 4, c); s.text(140, 400+i*46, t, 19, fill=MUTE, font=INTER)
    # right card storm
    s.rect(660, 280, 520, 320, PANEL, rx=18)
    s.rect(660, 280, 520, 70, CORAL, rx=18)
    s.rect(660, 320, 520, 30, PANEL)
    s.text(690, 326, "THE STORM", 26, fill=WHITE, font=INTER, weight="800", spacing="1")
    for i,(t,c) in enumerate([("Cytokine flood (inflammation)", CORAL),
                              ("DAMPs & danger signals", CORAL),
                              ("Immune cells run amok", AMBER),
                              ("Drives multi-organ failure", RED)]):
        s.circle(698, 400+i*46-5, 4, c); s.text(720, 400+i*46, t, 19, fill=MUTE, font=INTER)
    s.text(80, 660, "Beat both, and you change the outcome. No single trick does it — so we combine tools.",
           18, fill=WHITE, font=FRAUN, italic=True)
    footer(s, 9, TOTAL, "the idea")
    return reg(s, "09.png")

# =====================================================================
# 10 — THE TOOLKIT (modality reveal)
# =====================================================================
def s10():
    s = new(INK)
    kicker(s, 80, 120, "the toolkit")
    s.text(76, 200, "Four ways to support a liver", 50, fill=WHITE, font=ANTON)
    tools=[("CRRT","Cleans water-soluble toxins.\nThe ammonia & kidney workhorse.", TEAL, "★★★"),
           ("PLEX","Removes toxins, replaces factors,\nresets the immune storm.", AMBER, "★★★★"),
           ("MARS / Prometheus","Albumin dialysis for the\nprotein-bound poisons.", CORAL, "★★"),
           ("BIOARTIFICIAL","Living cells that do the\nliver's chemistry. (Coming.)", MUTE, "future")]
    x=80; y=300; bw=270; gap=20
    for i,(h,b,c,score) in enumerate(tools):
        xx=x+(bw+gap)*i
        s.rect(xx, y, bw, 300, PANEL, rx=16)
        s.rect(xx, y, bw, 8, c, rx=4)
        s.text(xx+24, y+70, f"0{i+1}", 30, fill=c, font=ANTON)
        s.text(xx+24, y+120, h, 21, fill=WHITE, font=INTER, weight="800")
        for j,ln in enumerate(b.split("\n")):
            s.text(xx+24, y+160+j*26, ln, 15, fill=MUTE, font=INTER)
        s.text(xx+24, y+270, "EVIDENCE  "+score, 13, fill=c, font=INTER, weight="700", spacing="1")
    s.text(80, 660, "Today we focus on the two that change survival the most: CRRT and PLEX.",
           18, fill=MUTE, font=INTER, italic=True)
    footer(s, 10, TOTAL, "the idea")
    return reg(s, "10.png")

# =====================================================================
# 11 — SECTION: CRRT
# =====================================================================
def section(n, label, big, sub, color, fname):
    s = new(INK)
    aurora(s, 980, 360, color, 560, 0.34)
    particles(s, 60, seed=n*3, color=color, op=0.45)
    s.rect(80, 250, 70, 8, color)
    s.text(80, 320, label.upper(), 18, fill=color, font=INTER, weight="700", spacing="5")
    s.text(76, 440, big, 130, fill=WHITE, font=ANTON)
    s.text(80, 510, sub, 24, fill=MUTE, font=FRAUN, italic=True)
    s.text(W-80, 320, f"{n:02d}", 120, fill=color, font=ANTON, anchor="end", opacity=0.25)
    footer(s, n, TOTAL)
    return reg(s, fname)

def s11(): return section(11, "Modality · 01", "CRRT", "The metabolic workhorse of the liver ICU.", TEAL, "11.png")

# =====================================================================
# 12 — CRRT: the brain connection (ammonia)
# =====================================================================
def s12():
    s = new(INK)
    kicker(s, 80, 120, "crrt · why it matters")
    s.text(76, 200, "Ammonia is a brain poison", 48, fill=WHITE, font=ANTON)
    s.text(80, 250, "CRRT pulls it out — continuously, gently, around the clock.", 20, fill=MUTE, font=FRAUN, italic=True)
    # flow diagram: blood -> filter -> clean blood, ammonia drips out
    y=420
    s.circle(220, y, 70, CORAL, opacity=0.18, stroke=CORAL, sw=2)
    s.text(220, y-4, "DIRTY", 18, fill=CORAL, font=INTER, weight="800", anchor="middle")
    s.text(220, y+22, "BLOOD", 18, fill=CORAL, font=INTER, weight="800", anchor="middle")
    s.text(220, y+120, "NH₃ ↑  toxins ↑", 16, fill=MUTE, font=INTER, anchor="middle")
    # filter
    s.rect(470, y-80, 120, 160, PANEL, rx=12, stroke=TEAL, sw=2)
    for i in range(7):
        s.line(470, y-60+i*20, 590, y-60+i*20, TEAL, 1.5, opacity=0.5)
    s.text(530, y-100, "CRRT FILTER", 14, fill=TEAL, font=INTER, weight="800", anchor="middle", spacing="1")
    # clean
    s.circle(870, y, 70, TEAL, opacity=0.20, stroke=TEAL, sw=2)
    s.text(870, y-4, "CLEAN", 18, fill=TEAL, font=INTER, weight="800", anchor="middle")
    s.text(870, y+22, "BLOOD", 18, fill=TEAL, font=INTER, weight="800", anchor="middle")
    # arrows
    s.line(295, y, 465, y, MUTE, 2); s.path(f"M 458 {y-7} L 472 {y} L 458 {y+7}", stroke=MUTE, sw=2)
    s.line(595, y, 795, y, MUTE, 2); s.path(f"M 788 {y-7} L 802 {y} L 788 {y+7}", stroke=MUTE, sw=2)
    # ammonia draining down
    s.line(530, y+80, 530, y+150, AMBER, 2, dash="2 8")
    s.text(530, y+175, "ammonia removed", 14, fill=AMBER, font=INTER, anchor="middle")
    # right note
    s.rect(1000, y-90, 200, 200, PANEL, rx=14)
    s.text(1020, y-50, "BONUS", 14, fill=TEAL, font=INTER, weight="800", spacing="2")
    s.text(1020, y-15, "Gentle flow", 17, fill=WHITE, font=INTER, weight="700")
    s.text(1020, y+12, "protects the", 16, fill=MUTE, font=INTER)
    s.text(1020, y+36, "swollen brain", 16, fill=MUTE, font=INTER)
    s.text(1020, y+62, "from pressure", 16, fill=MUTE, font=INTER)
    s.text(1020, y+86, "swings.", 16, fill=MUTE, font=INTER)
    footer(s, 12, TOTAL, "crrt")
    return reg(s, "12.png")

# =====================================================================
# 13 — CRRT: the 4x survival stat (figure people)
# =====================================================================
def s13():
    s = new(INK)
    kicker(s, 80, 120, "crrt · the payoff", TEAL)
    s.text(76, 200, "Control the ammonia,", 46, fill=WHITE, font=ANTON)
    s.text(76, 256, "quadruple the survival.", 46, fill=TEAL, font=ANTON)
    # two columns of 20 figures: 13% vs 55%
    def people(ox, label, pct, color):
        s.text(ox, 330, label, 15, fill=MUTE, font=INTER, weight="700", spacing="1")
        n_alive = round(pct/100*20)
        for i in range(20):
            col=i%5; row=i//5
            x=ox+col*42; y=360+row*54
            c = color if i < n_alive else PANEL
            # simple person glyph
            s.circle(x+10, y, 8, c)
            s.path(f"M {x} {y+30} q 10 -18 20 0 Z", fill=c)
        s.text(ox, 360+4*54+50, f"{pct}%", 60, fill=color, font=ANTON)
        s.text(ox+ (len(str(pct))+1)*36, 360+4*54+50, "survive", 16, fill=MUTE, font=INTER)
    people(120, "HYPERAMMONAEMIA NOT CONTROLLED", 13, CORAL)
    people(470, "EXTREME RISE PREVENTED WITH CRRT", 55, TEAL)
    # right callout
    s.rect(820, 320, 380, 250, PANEL, rx=18)
    s.text(850, 380, "84%", 70, fill=TEAL, font=ANTON)
    s.text(850, 420, "of patients had dangerous", 17, fill=WHITE, font=INTER)
    s.text(850, 446, "ammonia controlled after", 17, fill=WHITE, font=INTER)
    s.text(850, 472, "day 1 on CRRT.", 17, fill=WHITE, font=INTER)
    s.text(850, 525, "Often started for the brain —", 15, fill=MUTE, font=INTER, italic=True)
    s.text(850, 548, "not the kidneys.", 15, fill=MUTE, font=INTER, italic=True)
    footer(s, 13, TOTAL, "crrt")
    return reg(s, "13.png")

# =====================================================================
# 14 — SECTION: PLEX
# =====================================================================
def s14(): return section(14, "Modality · 02", "PLEX", "Plasma exchange — the strongest survival evidence we have.", AMBER, "14.png")

# =====================================================================
# 15 — PLEX: triple action
# =====================================================================
def s15():
    s = new(INK)
    kicker(s, 80, 120, "plex · the magic", AMBER)
    s.text(76, 200, "One therapy. Three blows.", 50, fill=WHITE, font=ANTON)
    acts=[("REMOVE","Swaps out the patient's toxic plasma — bilirubin, bile acids, poisons.", TEAL),
          ("REPLACE","Fresh plasma restores clotting factors & albumin the liver can't make.", AMBER),
          ("RESET","Washes out the cytokine storm — calming the inflammation that kills.", CORAL)]
    y=300
    for i,(h,b,c) in enumerate(acts):
        yy=y+i*125
        s.circle(150, yy+40, 50, c, opacity=0.15, stroke=c, sw=2)
        s.text(150, yy+52, str(i+1), 44, fill=c, font=ANTON, anchor="middle")
        s.text(240, yy+30, h, 30, fill=c, font=INTER, weight="800", spacing="1")
        s.text(240, yy+68, b, 19, fill=MUTE, font=INTER)
    s.text(80, 670, "It's the only modality that detoxifies, replaces, AND calms — all at once.",
           18, fill=WHITE, font=FRAUN, italic=True)
    footer(s, 15, TOTAL, "plex")
    return reg(s, "15.png")

# =====================================================================
# 16 — PLEX: the landmark RCT (survival bridge viz)
# =====================================================================
def s16():
    s = new(INK)
    kicker(s, 80, 120, "plex · the landmark trial", AMBER)
    s.text(76, 195, "The first proof it saves lives", 44, fill=WHITE, font=ANTON)
    s.text(80, 240, "Larsen et al., Journal of Hepatology 2016  ·  182 patients with acute liver failure",
           16, fill=MUTE, font=INTER)
    # two rising bars as "bridges"
    base=560; bw=150
    # SMT 47.8
    x1=320
    h1=(47.8/70)*300
    s.rect(x1-bw/2, base-h1, bw, h1, MUTE2, rx=8, opacity=0.6)
    s.text(x1, base-h1-20, "47.8%", 40, fill=MUTE, font=ANTON, anchor="middle")
    s.text(x1, base+30, "STANDARD CARE", 15, fill=MUTE, font=INTER, weight="700", anchor="middle", spacing="1")
    # PLEX 58.7
    x2=720
    h2=(58.7/70)*300
    s.add_def(grad("plexbar", [("0%", AMBER, 1),("100%", TEAL_D, 0.7)],0,0,0,1))
    s.rect(x2-bw/2, base-h2, bw, h2, "url(#plexbar)", rx=8)
    s.text(x2, base-h2-20, "58.7%", 48, fill=AMBER, font=ANTON, anchor="middle")
    s.text(x2, base+30, "+ PLASMA EXCHANGE", 15, fill=AMBER, font=INTER, weight="700", anchor="middle", spacing="1")
    # delta arrow
    s.line(x1, base-h1-70, x2, base-h2-70, TEAL, 2, dash="2 8")
    s.text((x1+x2)/2, base-h2-100, "+11 POINTS", 20, fill=TEAL, font=INTER, weight="800", anchor="middle", spacing="2")
    s.text((x1+x2)/2, base-h2-78, "transplant-free survival", 14, fill=MUTE, font=INTER, anchor="middle")
    s.line(180, base, 860, base, MUTE2, 1, opacity=0.4)
    # right stat
    s.rect(960, 300, 240, 260, PANEL, rx=18)
    s.text(985, 360, "HR", 22, fill=MUTE, font=INTER, weight="700")
    s.text(985, 430, "0.56", 64, fill=TEAL, font=ANTON)
    s.text(985, 470, "95% CI 0.36–0.86", 15, fill=MUTE, font=INTER)
    s.text(985, 510, "p = 0.0083", 18, fill=WHITE, font=INTER, weight="700")
    s.text(985, 540, "Now in EASL guidelines.", 13, fill=AMBER, font=INTER, italic=True)
    footer(s, 16, TOTAL, "plex")
    return reg(s, "16.png")

# =====================================================================
# 17 — PLEX in ACLF: forest plot reimagined
# =====================================================================
def s17():
    s = new(INK)
    kicker(s, 80, 120, "plex · does it last?", AMBER)
    s.text(76, 195, "Lower risk of death — for a full year", 40, fill=WHITE, font=ANTON)
    s.text(80, 240, "Pooled analysis · 23 studies · 5,336 patients with acute-on-chronic liver failure",
           16, fill=MUTE, font=INTER)
    # number line 0.5 .. 1.1 ; left favours PLEX
    x0=200; x1=1040; y=400
    def X(rr): return x0 + (rr-0.5)/(1.12-0.5)*(x1-x0)
    rows=[("30-DAY", 0.70, 0.60, 0.81, TEAL),
          ("90-DAY", 0.81, 0.77, 0.86, AMBER),
          ("1-YEAR", 0.85, 0.79, 0.92, GOLD)]
    axis_y = y - 75 + len(rows)*72 + 24
    # no-effect line
    s.line(X(1.0), y-110, X(1.0), axis_y, CORAL, 1.5, dash="3 7", opacity=0.8)
    s.text(X(1.0), y-122, "NO EFFECT", 13, fill=CORAL, font=INTER, weight="700", anchor="middle", spacing="2")
    for i,(lab,rr,lo,hi,c) in enumerate(rows):
        yy=y-75+i*72
        s.text(150, yy+6, lab, 16, fill=WHITE, font=INTER, weight="800", anchor="end", spacing="1")
        s.line(X(lo), yy, X(hi), yy, c, 4)
        s.line(X(lo), yy-8, X(lo), yy+8, c, 3); s.line(X(hi), yy-8, X(hi), yy+8, c, 3)
        s.circle(X(rr), yy, 11, c)
        s.circle(X(rr), yy, 5, INK)
        s.text(X(hi)+20, yy+6, f"RR {rr}", 18, fill=c, font=INTER, weight="800")
    # axis
    s.line(x0, axis_y, x1, axis_y, MUTE2, 1, opacity=0.4)
    for v in [0.5,0.6,0.7,0.8,0.9,1.0,1.1]:
        s.line(X(v), axis_y-5, X(v), axis_y+5, MUTE2, 1, opacity=0.5)
        s.text(X(v), axis_y+28, f"{v:.1f}", 14, fill=MUTE2, font=INTER, anchor="middle")
    s.text(x0, axis_y+72, "← lower means PLEX saves more lives", 15, fill=TEAL, font=INTER, italic=True)
    footer(s, 17, TOTAL, "plex")
    return reg(s, "17.png")

# =====================================================================
# 18 — PLEX as a bridge (literal bridge viz)
# =====================================================================
def s18():
    s = new(INK)
    aurora(s, 640, 600, TEAL_D, 600, 0.18)
    kicker(s, 80, 120, "plex · the bridge", AMBER)
    s.text(76, 200, "A bridge to transplant", 50, fill=WHITE, font=ANTON)
    s.text(80, 248, "Keeping candidates alive — and operable — until a liver arrives.", 19, fill=MUTE, font=FRAUN, italic=True)
    # cliff - bridge - cliff
    base=470
    s.rect(80, base, 240, 180, PANEL, rx=10)
    s.text(200, base+70, "CRISIS", 24, fill=CORAL, font=INTER, weight="800", anchor="middle", spacing="1")
    s.text(200, base+105, "listed, deteriorating", 15, fill=MUTE, font=INTER, anchor="middle")
    s.rect(960, base, 240, 180, PANEL, rx=10)
    s.text(1080, base+70, "TRANSPLANT", 22, fill=TEAL, font=INTER, weight="800", anchor="middle", spacing="1")
    s.text(1080, base+105, "a new liver", 15, fill=MUTE, font=INTER, anchor="middle")
    # bridge arc
    s.add_def(grad("bridge", [("0%", CORAL, 0.9),("50%", AMBER, 1),("100%", TEAL, 0.9)],0,0,1,0))
    s.path(f"M 320 {base} Q 640 {base-150} 960 {base}", stroke="url(#bridge)", sw=8)
    # suspension lines
    for t in [0.15,0.3,0.45,0.6,0.75,0.9]:
        bx=320+(960-320)*t
        by=base-150*4*t*(1-t)*0.75 - 0  # approx
        qy = base + (-150)*2*t*(1-t)  # quadratic point y
        s.line(bx, qy, bx, base, AMBER, 1, opacity=0.4)
    s.text(640, base-120, "PLEX", 34, fill=AMBER, font=ANTON, anchor="middle")
    # big stat
    s.rect(420, 165, 440, 0, PANEL)  # spacer (noop)
    s.text(W/2, 640, "65% lower 30-day mortality in transplant candidates  (HR 0.35, p = 0.024)",
           20, fill=TEAL, font=INTER, weight="700", anchor="middle")
    footer(s, 18, TOTAL, "plex")
    return reg(s, "18.png")

# =====================================================================
# 19 — ALBUMIN DIALYSIS: the honest disappointment
# =====================================================================
def s19():
    s = new(INK)
    kicker(s, 80, 120, "the honest slide", CORAL)
    s.text(76, 200, "When the numbers say 'no'", 46, fill=WHITE, font=ANTON)
    s.text(80, 248, "MARS & Prometheus — albumin dialysis. Big trials. Sobering results.", 19, fill=MUTE, font=FRAUN, italic=True)
    # two-line: biochemistry up, survival flat
    # up arrow
    s.rect(150, 320, 460, 250, PANEL, rx=16)
    s.text(180, 370, "BIOCHEMISTRY", 16, fill=TEAL, font=INTER, weight="800", spacing="1")
    s.path("M 200 530 L 320 420 L 420 460 L 560 360", stroke=TEAL, sw=4)
    s.path("M 540 360 L 560 360 L 560 380", stroke=TEAL, sw=4)
    s.text(180, 410, "bilirubin falls ✓", 17, fill=MUTE, font=INTER)
    # flat
    s.rect(670, 320, 460, 250, PANEL, rx=16)
    s.text(700, 370, "SURVIVAL", 16, fill=CORAL, font=INTER, weight="800", spacing="1")
    s.line(700, 470, 1100, 470, CORAL, 4)
    s.text(700, 410, "no overall benefit ✗", 17, fill=MUTE, font=INTER)
    s.text(700, 540, "(signal only in HRS-1 / MELD>30)", 14, fill=MUTE2, font=INTER, italic=True)
    s.text(80, 660, "Good science means reporting what didn't work. Reserve these for selected patients.",
           18, fill=WHITE, font=FRAUN, italic=True)
    footer(s, 19, TOTAL, "evidence")
    return reg(s, "19.png")

# =====================================================================
# 20 — THE PLAYBOOK (integrated algorithm)
# =====================================================================
def s20():
    s = new(INK)
    kicker(s, 80, 120, "putting it together")
    s.text(76, 200, "The peri-transplant playbook", 48, fill=WHITE, font=ANTON)
    steps=[("RECOGNISE","ALF / ACLF +\norgan failures", TEAL),
           ("STABILISE","ICU · NAC ·\nantivirals · sepsis", TEAL),
           ("SUPPORT","CRRT for ammonia\n+ PLEX for toxins", AMBER),
           ("DECIDE","recover? or\nlist for transplant?", AMBER),
           ("BRIDGE","PLEX → transplant\nor native recovery", CORAL)]
    n=len(steps); y=400; x0=130; gap=(W-2*x0)/(n-1)
    # connector
    s.line(x0, y, x0+gap*(n-1), y, MUTE2, 2, opacity=0.4)
    for i,(h,b,c) in enumerate(steps):
        x=x0+gap*i
        s.circle(x, y, 46, INK2, stroke=c, sw=3)
        s.circle(x, y, 46, c, opacity=0.12)
        s.text(x, y+8, str(i+1), 34, fill=c, font=ANTON, anchor="middle")
        s.text(x, y-75, h, 18, fill=WHITE, font=INTER, weight="800", anchor="middle", spacing="1")
        for j,ln in enumerate(b.split("\n")):
            s.text(x, y+90+j*24, ln, 14, fill=MUTE, font=INTER, anchor="middle")
    s.text(W/2, 650, "Treat the cause · support every organ · re-assess transplant candidacy daily.",
           18, fill=TEAL, font=FRAUN, italic=True, anchor="middle")
    footer(s, 20, TOTAL, "the playbook")
    return reg(s, "20.png")

# =====================================================================
# 21 — WHAT JUST CHANGED
# =====================================================================
def s21():
    s = new(INK)
    kicker(s, 80, 120, "the field is moving fast")
    s.text(76, 200, "What changed — just now", 48, fill=WHITE, font=ANTON)
    items=[("PLEX is now guideline therapy","High-volume PLEX earns a top recommendation in acute liver failure.", TEAL),
           ("Less can be just as good","Standard-volume PLEX matches high-volume — cheaper, more feasible.", AMBER),
           ("ACLF evidence matured","A 2025 meta-analysis confirms survival benefit out to one year.", CORAL),
           ("Ammonia-targeted CRRT","Early, high-dose CRRT for the brain is gaining ground.", TEAL),
           ("Etiology-specific wins","Strong signals in alcohol-associated & hepatitis-B ACLF.", AMBER)]
    y=290
    for i,(h,b,c) in enumerate(items):
        yy=y+i*78
        s.rect(80, yy, 12, 58, c, rx=6)
        s.text(110, yy+24, h, 22, fill=WHITE, font=INTER, weight="800")
        s.text(110, yy+52, b, 16, fill=MUTE, font=INTER)
    footer(s, 21, TOTAL, "advances")
    return reg(s, "21.png")

# =====================================================================
# 22 — THE FUTURE (glowing horizon path)
# =====================================================================
def s22():
    s = new(INK)
    aurora(s, 640, 720, TEAL_D, 760, 0.4)
    aurora(s, 1050, 200, AMBER, 380, 0.16)
    particles(s, 90, seed=55, color=TEAL, op=0.5)
    kicker(s, 80, 120, "the horizon", GOLD)
    s.text(76, 200, "From cleaning blood", 46, fill=WHITE, font=ANTON)
    s.text(76, 256, "to growing livers", 46, fill=GOLD, font=ANTON)
    # rising path with milestones
    pts=[(150,560,"NOW","PLEX & CRRT in guidelines",TEAL),
         (430,470,"NEXT","Phase-3 PLEX trials in ACLF",AMBER),
         (710,380,"SOON","Bioartificial liver — living cells",CORAL),
         (990,290,"BEYOND","Stem-cell & AI-guided support",GOLD)]
    # path
    d="M "+ " L ".join(f"{x} {y}" for x,y,_,_,_ in pts)
    s.path(d, stroke="url(#bridge2)", sw=4, opacity=0.9)
    s.add_def(grad("bridge2", [("0%", TEAL, 1),("100%", GOLD, 1)],0,0,1,0))
    for x,y,tag,txt,c in pts:
        s.circle(x,y,12,c)
        s.circle(x,y,5,INK)
        s.text(x, y-50, tag, 16, fill=c, font=INTER, weight="800", anchor="middle", spacing="2")
        # wrap text
        words=txt.split();
        s.text(x, y-26, txt, 14, fill=MUTE, font=INTER, anchor="middle")
    s.text(80, 660, "The endgame: machines that don't just buy time — they give the liver back.",
           18, fill=WHITE, font=FRAUN, italic=True)
    footer(s, 22, TOTAL, "the future")
    return reg(s, "22.png")

# =====================================================================
# 23 — TAKEAWAYS (5 punches)
# =====================================================================
def s23():
    s = new(INK)
    aurora(s, 980, 360, TEAL_D, 520, 0.25)
    kicker(s, 80, 120, "if you remember five things")
    s.text(76, 200, "Take this to the bedside", 48, fill=WHITE, font=ANTON)
    pts=[("Buy time, target the toxin.","Match the machine to what's poisoning the patient.", TEAL),
         ("PLEX has the strongest evidence.","Survival benefit in ALF and ACLF — start early.", AMBER),
         ("CRRT is a brain therapy too.","Use it for ammonia, not just the kidneys.", CORAL),
         ("Albumin dialysis: choose wisely.","Biochemistry improves; survival usually doesn't.", GOLD),
         ("The future is regenerative.","Bioartificial livers and cells are coming.", TEAL)]
    y=290
    for i,(h,b,c) in enumerate(pts):
        yy=y+i*76
        s.circle(108, yy+18, 26, c, opacity=0.16, stroke=c, sw=2)
        s.text(108, yy+26, str(i+1), 26, fill=c, font=ANTON, anchor="middle")
        s.text(160, yy+12, h, 23, fill=WHITE, font=INTER, weight="800")
        s.text(160, yy+44, b, 17, fill=MUTE, font=INTER)
    footer(s, 23, TOTAL, "takeaways")
    return reg(s, "23.png")

# =====================================================================
# 24 — CLOSE
# =====================================================================
def s24():
    s = new(INK)
    aurora(s, 640, 360, TEAL_D, 700, 0.4)
    particles(s, 70, seed=99, color=TEAL, op=0.5)
    liver_network(s, 640, 330, scale=1.4, seed=8, op=0.5)
    s.add_def(rgrad("cvg", [("40%", INK, 0),("100%", INK, 0.8)]))
    s.rect(0,0,W,H,"url(#cvg)")
    s.text(W/2, 320, "We can't stop the liver from failing.", 30, fill=MUTE, font=FRAUN, italic=True, anchor="middle")
    s.text(W/2, 400, "But we can buy it time.", 56, fill=WHITE, font=ANTON, anchor="middle")
    s.text(W/2, 450, "And time is everything.", 26, fill=TEAL, font=FRAUN, italic=True, anchor="middle")
    s.text(W/2, 560, "Thank you.", 22, fill=WHITE, font=INTER, weight="700", anchor="middle", spacing="2")
    s.text(W/2, 612, "References: Larsen 2016 · Maiwall 2022 · Kumar 2025 (meta-analysis) · RELIEF · HELIOS · EASL CPG · APACHE/NCT03702920",
           12, fill=MUTE2, font=INTER, anchor="middle")
    return reg(s, "24.png")

def main():
    fns=[s01,s02,s03,s04,s05,s06,s07,s08,s09,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24]
    for fn in fns:
        fn()
    print(f"rendered {len(SLIDES)} slides")

if __name__=="__main__":
    main()
