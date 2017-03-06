#!/usr/bin/env python2

from PIL import Image
from PIL import ImageFont
from PIL import ImageDraw
import time,os

print os.getcwd()
configFile = file("./config.file","r")
signature = configFile.readlines()[-1][:-28]
configFile.close()

time.sleep(4)

img = Image.open("../assets/state_default.jpg")
draw = ImageDraw.Draw(img)
font = ImageFont.truetype("/usr/share/fonts/gnu-free/FreeMono.ttf", 16)
draw.text((0, 0),"Signature: %s"%signature,(0,0,0),font=font)
img.save('../output/'+signature+'.jpg')
