# Script to generate ntlgini GitHub Social Preview (1280x640) - Minimalist Edition
import os
from PIL import Image, ImageDraw, ImageFont, ImageFilter

W, H = 1280, 640
font_path = "/System/Library/Fonts/Helvetica.ttc"

# Sleek minimalist background matching the hex sticker interior
canvas = Image.new("RGBA", (W, H), (10, 15, 29, 255)) # #0a0f1d

base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
logo_path = os.path.join(base_dir, "man", "figures", "logo.png")
logo = Image.open(logo_path).convert("RGBA")

# Logo sizing & positioning (centered)
target_h = 345
target_w = int(logo.width * (target_h / logo.height))
logo_resized = logo.resize((target_w, target_h), Image.Resampling.LANCZOS)

lx = (W - target_w) // 2
ly = 85

# Soft subtle shadow
shadow = Image.new("RGBA", (W, H), (0, 0, 0, 0))
s_mask = logo_resized.split()[3].point(lambda a: a * 0.45)
shadow.paste((0, 0, 0, 180), (lx, ly + 14), mask=s_mask)
shadow = shadow.filter(ImageFilter.GaussianBlur(22))
canvas = Image.alpha_composite(canvas, shadow)

# Paste logo
canvas.paste(logo_resized, (lx, ly), mask=logo_resized)

# Clean, quiet typography
draw = ImageDraw.Draw(canvas)
font_sub = ImageFont.truetype(font_path, 25, index=1)   # Bold
font_meta = ImageFont.truetype(font_path, 16, index=0)  # Regular

sub_text = "Global Subnational Nighttime-Light Inequality"
bbox_s = draw.textbbox((0, 0), sub_text, font=font_sub)
sw = bbox_s[2] - bbox_s[0]
draw.text(((W - sw) // 2, 472), sub_text, font=font_sub, fill=(255, 255, 255, 255))

meta_text = "1992 — 2024  •  R Package"
bbox_m = draw.textbbox((0, 0), meta_text, font=font_meta)
mw = bbox_m[2] - bbox_m[0]
draw.text(((W - mw) // 2, 516), meta_text, font=font_meta, fill=(148, 163, 184, 255))

out1 = os.path.join(base_dir, "man", "figures", "social_preview.png")
out2 = os.path.join(base_dir, ".github", "social_preview.png")

canvas.save(out1, "PNG", optimize=True)
canvas.save(out2, "PNG", optimize=True)
print("Saved minimalist social preview to:", out1, "and", out2)
