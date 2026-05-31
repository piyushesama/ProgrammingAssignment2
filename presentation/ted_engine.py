#!/usr/bin/env python3
"""
TED-style slide engine. Every slide is authored as an SVG (full typographic +
vector control) and rendered to a 2560x1440 PNG via cairosvg. The deck is then
assembled as full-bleed images in build_ted_deck.py.

Design language
  - Cinematic near-black canvas, one big idea per slide
  - Display type: Anton (impact) + Fraunces (editorial serif) + Inter (body)
  - Accent system: teal (life/therapy), amber (warning/time), coral (risk/death)
  - Custom vector data-viz, no default chart chrome
"""
import os, math, html, random
import cairosvg

HERE = os.path.dirname(os.path.abspath(__file__))
OUT  = os.path.join(HERE, "slides")
ASSET= os.path.join(HERE, "assets")
os.makedirs(OUT, exist_ok=True)

W, H = 1280, 720            # SVG user units (16:9)
SCALE = 2                    # -> 2560x1440 px output

# palette ------------------------------------------------------------
INK    = "#070B14"           # near-black canvas
INK2   = "#0C1322"
PANEL  = "#111B2E"
TEAL   = "#1FD1B6"
TEAL_D = "#11998B"
AMBER  = "#F6B73C"
CORAL  = "#FF6B57"
RED    = "#E8412C"
WHITE  = "#F4F8FB"
MUTE   = "#8FA3B8"
MUTE2  = "#5A6E83"
GOLD   = "#FFD27A"

ANTON  = "Anton"
FRAUN  = "Fraunces"
INTER  = "Inter"
BEBAS  = "Bebas Neue"

def esc(t): return html.escape(str(t), quote=True)

class SVG:
    def __init__(self, bg=INK):
        self.defs = []
        self.body = []
        self.bg = bg
    def add(self, s): self.body.append(s)
    def add_def(self, s): self.defs.append(s)

    # primitives ----------------------------------------------------
    def rect(self, x, y, w, h, fill, rx=0, opacity=1, stroke=None, sw=0):
        s = f'<rect x="{x:.2f}" y="{y:.2f}" width="{w:.2f}" height="{h:.2f}" rx="{rx}" fill="{fill}" opacity="{opacity}"'
        if stroke: s += f' stroke="{stroke}" stroke-width="{sw}"'
        self.add(s + '/>')
    def line(self, x1,y1,x2,y2, stroke, sw=1, opacity=1, dash=None, cap="butt"):
        d = f' stroke-dasharray="{dash}"' if dash else ""
        self.add(f'<line x1="{x1:.2f}" y1="{y1:.2f}" x2="{x2:.2f}" y2="{y2:.2f}" stroke="{stroke}" stroke-width="{sw}" opacity="{opacity}" stroke-linecap="{cap}"{d}/>')
    def circle(self, cx, cy, r, fill, opacity=1, stroke=None, sw=0):
        s=f'<circle cx="{cx:.2f}" cy="{cy:.2f}" r="{r:.2f}" fill="{fill}" opacity="{opacity}"'
        if stroke: s+=f' stroke="{stroke}" stroke-width="{sw}"'
        self.add(s+'/>')
    def path(self, d, fill="none", stroke=None, sw=0, opacity=1, dash=None, cap="round", join="round"):
        st = f' stroke="{stroke}" stroke-width="{sw}" stroke-linecap="{cap}" stroke-linejoin="{join}"' if stroke else ""
        da = f' stroke-dasharray="{dash}"' if dash else ""
        self.add(f'<path d="{d}" fill="{fill}" opacity="{opacity}"{st}{da}/>')
    def text(self, x, y, t, size, fill=WHITE, font=INTER, weight=None, anchor="start",
             spacing=None, opacity=1, italic=False):
        extra = ""
        if spacing is not None: extra += f' letter-spacing="{spacing}"'
        if weight: extra += f' font-weight="{weight}"'
        if italic: extra += ' font-style="italic"'
        self.add(f'<text x="{x:.2f}" y="{y:.2f}" font-family="{font}" font-size="{size}" '
                 f'fill="{fill}" text-anchor="{anchor}" opacity="{opacity}"{extra}>{esc(t)}</text>')
    def image(self, href, x, y, w, h, opacity=1):
        self.add(f'<image href="{href}" x="{x}" y="{y}" width="{w}" height="{h}" opacity="{opacity}" preserveAspectRatio="xMidYMid slice"/>')
    def group(self, content, opacity=1, transform=None):
        tr = f' transform="{transform}"' if transform else ""
        self.add(f'<g opacity="{opacity}"{tr}>{content}</g>')

    def render(self, name):
        svg = (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
               f'width="{W}" height="{H}" viewBox="0 0 {W} {H}">'
               f'<defs>{"".join(self.defs)}</defs>'
               f'<rect width="{W}" height="{H}" fill="{self.bg}"/>'
               + "".join(self.body) + '</svg>')
        path = os.path.join(OUT, name)
        cairosvg.svg2png(bytestring=svg.encode("utf-8"), write_to=path,
                         output_width=W*SCALE, output_height=H*SCALE)
        return path

# ---- shared decorative helpers -------------------------------------
def grad(id, stops, x1=0,y1=0,x2=0,y2=1):
    s = "".join(f'<stop offset="{o}" stop-color="{c}" stop-opacity="{a}"/>' for o,c,a in stops)
    return f'<linearGradient id="{id}" x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}">{s}</linearGradient>'

def rgrad(id, stops, cx="50%", cy="50%", r="60%"):
    s = "".join(f'<stop offset="{o}" stop-color="{c}" stop-opacity="{a}"/>' for o,c,a in stops)
    return f'<radialGradient id="{id}" cx="{cx}" cy="{cy}" r="{r}">{s}</radialGradient>'

def soft_glow(id, dev=8):
    return (f'<filter id="{id}" x="-60%" y="-60%" width="220%" height="220%">'
            f'<feGaussianBlur stdDeviation="{dev}"/></filter>')

def vignette(svg, color="#000000", op=0.55):
    svg.add_def(rgrad("vig", [("55%", color, 0),("100%", color, op)]))
    svg.rect(0,0,W,H,"url(#vig)")

def particles(svg, n=70, seed=7, color=TEAL, rmax=2.2, area=(0,0,W,H), op=0.5):
    rnd = random.Random(seed)
    x0,y0,x1,y1 = area
    pts=[]
    for _ in range(n):
        x=rnd.uniform(x0,x1); y=rnd.uniform(y0,y1)
        r=rnd.uniform(0.4,rmax); o=rnd.uniform(0.15,op)
        svg.circle(x,y,r,color,opacity=o)
        pts.append((x,y))
    return pts

def kicker(svg, x, y, text, color=TEAL):
    svg.rect(x, y-9, 26, 4, color)
    svg.text(x+38, y, text.upper(), 15, fill=color, font=INTER, weight="700", spacing="3")

def footer(svg, n, total=24, section=""):
    svg.line(70, H-46, W-70, H-46, MUTE2, 1, opacity=0.25)
    svg.text(70, H-26, "LIVER SUPPORT IN THE ICU", 11, fill=MUTE2, font=INTER, weight="600", spacing="2.5")
    if section:
        svg.text(W/2, H-26, section.upper(), 11, fill=MUTE2, font=INTER, weight="600", spacing="2.5", anchor="middle")
    svg.text(W-70, H-26, f"{n:02d} / {total:02d}", 11, fill=MUTE2, font=INTER, weight="600", spacing="2", anchor="end")

# ---- liver-as-network motif (signature vector art) -----------------
def liver_network(svg, cx, cy, scale=1.0, seed=3, color=TEAL, color2=AMBER, glow="g1", op=1.0):
    """A stylised liver silhouette filled with a glowing node/vessel network."""
    rnd = random.Random(seed)
    # liver silhouette path (stylised, two lobes)
    d = ("M -150 -40 C -150 -95 -80 -120 10 -118 "
         "C 90 -116 160 -100 168 -55 "
         "C 176 -8 150 55 70 80 "
         "C 10 98 -60 96 -110 70 "
         "C -150 50 -150 5 -150 -40 Z")
    g = []
    g.append(f'<path d="{d}" fill="url(#liverfill)" stroke="{color}" stroke-width="1.5" opacity="0.9"/>')
    # nodes inside bounding region, clipped to path
    nodes=[]
    tries=0
    while len(nodes) < 26 and tries < 800:
        tries+=1
        x=rnd.uniform(-150,170); y=rnd.uniform(-118,98)
        # rough inside test via sampling distance to center band
        if (x*x)/(165*165) + (y*y)/(105*105) < 0.92:
            nodes.append((x,y))
    # edges: connect near neighbours
    for i,(x,y) in enumerate(nodes):
        dd=sorted(nodes, key=lambda p:(p[0]-x)**2+(p[1]-y)**2)[1:4]
        for (x2,y2) in dd:
            g.append(f'<line x1="{x:.1f}" y1="{y:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="0.8" opacity="0.30"/>')
    for i,(x,y) in enumerate(nodes):
        c = color2 if i%5==0 else color
        r = rnd.uniform(1.6,3.6)
        g.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r:.1f}" fill="{c}" filter="url(#{glow})"/>')
        g.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r*0.5:.1f}" fill="{WHITE}"/>')
    svg.add(f'<g transform="translate({cx},{cy}) scale({scale})" opacity="{op}">{"".join(g)}</g>')

print("engine module ready")
