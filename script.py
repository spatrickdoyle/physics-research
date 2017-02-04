#!/usr/bin/env python2

import cPickle as pickle
import os,time

#file("lockfile","w+")

configFile = file("image.cfg","r")
data = pickle.load(configFile)
configFile.close()

time.sleep(4)

imageFile = file('graph.png','w+')
imageFile.write(str(data))
imageFile.close()

#os.remove("lockfile")
