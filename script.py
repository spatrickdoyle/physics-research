#!/usr/bin/env python2

import time

configFile = file("image.cfg","r")
signature = configFile.readlines()[-1][:-28]
configFile.close()

time.sleep(4)

imageFile = file('images/'+signature+'.jpg','w+')
imageFile.write(str(signature))
imageFile.close()
