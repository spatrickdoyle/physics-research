#!/usr/bin/env python2

#import os
import cgi
import cgitb
cgitb.enable()

#os.chdir("../mathscript_v16/bin")

print "Content-type:text/html\r\n"

#The HTML bit
#Source file head
print "<html>"
print "<body>"
print "something"
a = file("../../test.txt","w")
a.write("SOMETHING ELSE")
a.close()
print "</body>"
print "</html>"
