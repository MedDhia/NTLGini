# Script to generate ntlgini GitHub Social Preview (1280x640)
import os
from PIL import Image, ImageDraw, ImageFont, ImageFilter

W, H = 1280, 640
img = Image.new("RGBA", (W, H), (6, 10, 20, 255))
draw = ImageDraw.Draw(img)

# 1. Subtle background grid & gradient
for y in range(H):
    ratio = y / H
    r = int(7 * (1 - ratio) + 13 * ratio)
    g = int(12 * (1 - ratio) + 23 * ratio)
    b = int(24 * (1 - ratio) + 46 * ratio)
    draw.line([(0, y), (W, y)], fill=(r, g, b, 255))

# Subtle grid lines
grid_color = (255, 255, 255, 10)
grid_overlay = Image.new("RGBA", (W, H), (0, 0, 0, 0))
grid_draw = ImageDraw.Draw(grid_overlay)
for x in range(0, W, 40):
    grid_draw.line([(x, 0), (x, H)], fill=grid_color, width=1)
for y in range(0, H, 40):
    grid_draw.line([(0, y), (W, y)], fill=grid_color, width=1)
img = Image.alpha_composite(img, grid_overlay)

# 2. Soft radial glow behind logo
glow = Image.new("RGBA", (W, H), (0, 0, 0, 0))
glow_draw = ImageDraw.Draw(glow)
center_x, center_y = 260, 320
for radius in range(260, 0, -5):
    alpha = int(22 * (1 - (radius / 260)**1.5))
    glow_draw.ellipse(
        [center_x - radius, center_y - radius, center_x + radius, center_y + radius],
        fill=(244, 63, 94, alpha)
    )
glow = glow.filter(ImageFilter.GaussianBlur(30))
img = Image.alpha_composite(img, glow)

# 3. Load & paste Logo
base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
logo_path = os.path.join(base_dir, "man", "figures", "logo.png")
logo = Image.open(logo_path).convert("RGBA")
target_h = 430
target_w = int(logo.width * (target_h / logo.height))
logo_resized = logo.resize((target_w, target_h), Image.Resampling.LANCZOS)

# Soft shadow for logo
shadow = Image.new("RGBA", (W, H), (0, 0, 0, 0))
logo_x = 70
logo_y = (H - target_h) // 2
shadow_mask = logo_resized.split()[3].point(lambda a: a * 0.45)
shadow.paste((0, 0, 0, 180), (logo_x + 8, logo_y + 12), mask=shadow_mask)
shadow = shadow.filter(ImageFilter.GaussianBlur(16))
img = Image.alpha_composite(img, shadow)

# Paste logo
img.paste(logo_resized, (logo_x, logo_y), mask=logo_resized)

# 4. Typography
draw = ImageDraw.Draw(img)

font_path = "/System/Library/Fonts/Helvetica.ttc"
font_badge = ImageFont.truetype(font_path, 15, index=1)
font_title = ImageFont.truetype(font_path, 68, index=1)
font_sub = ImageFont.truetype(font_path, 23, index=0)
font_feature_bold = ImageFont.truetype(font_path, 17, index=1)
font_feature = ImageFont.truetype(font_path, 17, index=0)
font_pill = ImageFont.truetype(font_path, 13, index=1)

text_x = 515

# Badge
badge_y = 75
badge_text = "R PACKAGE  •  CRAN COMPLIANT  •  1992–2024"
badge_bbox = draw.textbbox((0, 0), badge_text, font=font_badge)
bw = badge_bbox[2] - badge_bbox[0] + 28
bh = badge_bbox[3] - badge_bbox[1] + 14

badge_img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
b_draw = ImageDraw.Draw(badge_img)
b_draw.rounded_rectangle([text_x, badge_y, text_x + bw, badge_y + bh], radius=7, fill=(244, 63, 94, 30), outline=(244, 63, 94, 180), width=1)
b_draw.text((text_x + 14, badge_y + 7), badge_text, font=font_badge, fill=(251, 113, 133, 255))
img = Image.alpha_composite(img, badge_img)
draw = ImageDraw.Draw(img)

# Title: ntlgini
title_y = 120
draw.text((text_x, title_y), "ntlgini", font=font_title, fill=(255, 255, 255, 255))

# Subtitle
sub_y = 205
draw.text((text_x, sub_y), "Global Subnational Nighttime-Light Gini Decompositions", font=font_sub, fill=(226, 232, 240, 255))

# Gradient separator rule
rule_y = 250
rule_overlay = Image.new("RGBA", (W, H), (0, 0, 0, 0))
r_draw = ImageDraw.Draw(rule_overlay)
for rx in range(text_x, text_x + 680):
    t = (rx - text_x) / 680
    r = int(244 * (1 - t) + 56 * t)
    g = int(63 * (1 - t) + 189 * t)
    b = int(94 * (1 - t) + 248 * t)
    alpha = int(240 * (1 - t**0.7))
    r_draw.line([(rx, rule_y), (rx, rule_y + 2)], fill=(r, g, b, alpha), width=2)
img = Image.alpha_composite(img, rule_overlay)
draw = ImageDraw.Draw(img)

# Features list
features = [
    ("228 Countries & Territories", " across ADM_1 to ADM_4 administrative boundaries"),
    ("Pyatt (1976) & Dagum (1997)", " additive spatial inequality decompositions"),
    ("33-Year Longitudinal Satellite Panel", " (Cheng et al. 2026, 500 m harmonized)"),
    ("High-Performance Cloud Streaming", " and polygon raster zonal extraction")
]

start_fy = 275
gap_y = 42

for idx, (lead, rest) in enumerate(features):
    fy = start_fy + idx * gap_y
    draw.polygon([
        (text_x + 6, fy + 5),
        (text_x + 12, fy + 11),
        (text_x + 6, fy + 17),
        (text_x, fy + 11)
    ], fill=(244, 63, 94, 255))
    draw.text((text_x + 24, fy), lead, font=font_feature_bold, fill=(248, 250, 252, 255))
    lead_w = draw.textbbox((0, 0), lead, font=font_feature_bold)[2]
    draw.text((text_x + 24 + lead_w, fy), rest, font=font_feature, fill=(148, 163, 184, 255))

# Bottom metadata pills
pills = [
    "500 m Resolution",
    "DMSP-OLS & VIIRS",
    "GADM 4.1 & HDX COD-AB",
    "Open Data (CC BY 4.0)"
]

pill_start_x = text_x
pill_y = 475
for p_text in pills:
    p_bbox = draw.textbbox((0, 0), p_text, font=font_pill)
    pw = p_bbox[2] - p_bbox[0] + 20
    ph = p_bbox[3] - p_bbox[1] + 12
    draw.rounded_rectangle([pill_start_x, pill_y, pill_start_x + pw, pill_y + ph], radius=6, fill=(15, 23, 42, 200), outline=(51, 65, 85, 255), width=1)
    draw.text((pill_start_x + 10, pill_y + 6), p_text, font=font_pill, fill=(148, 163, 184, 255))
    pill_start_x += pw + 12

out1 = os.path.join(base_dir, "man", "figures", "social_preview.png")
out2 = os.path.join(base_dir, ".github", "social_preview.png")

img.save(out1, "PNG", optimize=True)
img.save(out2, "PNG", optimize=True)
print("Saved social preview to", out1, "and", out2)
