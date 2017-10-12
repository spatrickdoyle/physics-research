#!/usr/bin/env python2

#CGI script to create an HTML page for displaying graphs generated by Bo Ting's Mathematica script
#Written by Sean Doyle in 2017 for the Southern Methodist University theoretical physics research lab

#TODO:
#Update all parameters
#Create seperate page for output
#Make the form update dynamically while parameters are being entered
#Make graph scalable (user can change axis bounds)
#Make history box
#Test with AWS
#Remove CGI debugging stuff

# Import module for CGI handling 
import cgi

#Object wrappers for Google Material Design components
import mdl

#For debugging - remove or comment when complete
import cgitb
cgitb.enable()

#Other things
import glob,os,time

#Mathscript interface
from mathscript import *


os.chdir(AROOT)


#Create all the input elements that will be on the page

#Get data from submitted form
form = cgi.FieldStorage()


#Print packet header
print "Content-type:text/html\r\n"

#The HTML bit
#Source file head
print "<html>"

print TAB(1)+"<head>"
print TAB(2)+"<title>Physics test page</title>\n"

#Google Material Design stylesheets
print TAB(2)+"<link rel='stylesheet' href='%sct66.css'>"%ASSETS
print TAB(2)+"<link rel='stylesheet' href='%smdl/material.min.css'>"%ASSETS
print TAB(2)+"<script src='%smdl/material.min.js'></script>"%ASSETS
print TAB(2)+"<link rel='stylesheet' href='%smdl/icon.css'>"%ASSETS

#Other resources
print TAB(2)+"<link rel='stylesheet' href='%sindex.css'>"%ASSETS
print TAB(2)+"<script src='%sindex.js'></script>"%ASSETS

print TAB(1)+"</head>"

print TAB(1)+"<body>"
print TAB(2)+"<h1>LHC Particle Distributions</h1>"
print TAB(2)+"<h2>Southern Methodist University Physics Department</h2>\n"

#Update input elements based on previous form submission
if len(form) != 0:
    for element in all_elements:
        element.checkState(form)

#Generate the actual HTML form and draw all the input elements

print TAB(2)+'<form action="index.cgi" method="post" style="display:inline" id="theForm">'

print TAB(3)+'<table>'
print TAB(4)+'<tr>'
print TAB(5)+'<td>'

#PDFset
print TAB(6)+'Choose PDF set:<br/>'
pdfset.draw(6)
print TAB(6)+'<br/>\n'

print TAB(5)+'</td>\n'

#Experiment IDs
print TAB(5)+'<td>'
print TAB(6)+'Experiments to include:<br/>'

print TAB(6)+'<table>'

print TAB(7)+'<tr>'
print TAB(8)+'<td style="width:20%;border:none">'
exp_boxes[0].draw(9) #'All' box
print TAB(8)+'</td>'
print TAB(7)+'</tr>\n'

print TAB(7)+'<tr>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in exp_boxes[1:int(len(exp_boxes)/4)]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in exp_boxes[int(len(exp_boxes)/4):int(len(exp_boxes)/2)]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in exp_boxes[int(len(exp_boxes)/2):int(3*len(exp_boxes)/4)]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in exp_boxes[int(3*len(exp_boxes)/4):]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(7)+'</tr>'

print TAB(6)+'</table>'

print TAB(5)+'</td>'
print TAB(4)+'</tr>'
print TAB(3)+'</table>\n'

print TAB(3)+'<table>'
print TAB(4)+'<tr>'
print TAB(5)+'<td>'

print TAB(6)+'Figures to plot:<br/><br/>\n'
print TAB(6)+'<table>'

print TAB(7)+'<tr>'
for box in fig_boxes:
    print TAB(8)+'<td style="width: 17%">'
    box.draw(9)
    print TAB(8)+'</td>'
print TAB(7)+'</tr>\n'

print TAB(7)+'<tr>'
print TAB(8)+'<td style="visibility:hidden"></td>'
for s in range(5):
    if fig_boxes[1].getState() != s:
        if (len(form) == 0)and(s == 0):
            print TAB(8)+'<td class="highlight%d fadeIn"><span>'%(s)
        else:
            print TAB(8)+'<td class="highlight%d fadeOut"><span>'%(s)

    else:
        print TAB(8)+'<td class="highlight%d fadeIn"><span>'%(s)
    print TAB(9)+'Highlight mode:<br/>'
    selects[s].draw(9)
    print TAB(8)+'</span></td>'
print TAB(7)+'</tr>\n'

print TAB(7)+'<tr>'
print TAB(8)+'<td style="visibility:hidden"></td>'
for t in range(5):
    if ((fig_boxes[1].getState() != t) and not ((len(form) == 0)and(t == 0))) or (selects[t].getState() != 0):
        print TAB(8)+'<td class="highlight%d mode0 fadeOut"><span>'%(t)
    else:
        print TAB(8)+'<td class="highlight%d mode0 fadeIn"><span>'%(t)
    print TAB(6)+'Size of data points:<br/>'
    sizes[t].draw(6)

    print TAB(9)+'<br/>'
    print TAB(8)+'</span></td>'
print TAB(7)+'</tr>'


print TAB(7)+'<tr>'
print TAB(8)+'<td style="visibility:hidden"></td>'
for t in range(5):
    if ((fig_boxes[1].getState() != t) and not ((len(form) == 0)and(t == 0))) or (selects[t].getState() != 1):
        print TAB(8)+'<td class="highlight%d mode1 fadeOut"><span>'%(t)
    else:
        print TAB(8)+'<td class="highlight%d mode1 fadeIn"><span>'%(t)
    print TAB(9)+'Input range of values:<br/>'
    vmins[t].draw(9)
    print TAB(9)+'<br/>'
    vmaxs[t].draw(9)

    print TAB(9)+'<br/>'
    print TAB(8)+'</span></td>'
print TAB(7)+'</tr>'

print TAB(7)+'<tr>'
print TAB(8)+'<td style="visibility:hidden"></td>'
for t in range(5):
    if ((fig_boxes[1].getState() != t) and not ((len(form) == 0)and(t == 0))) or (selects[t].getState() != 2):
        print TAB(8)+'<td class="highlight%d mode2 fadeOut"><span>'%(t)
    else:
        print TAB(8)+'<td class="highlight%d mode2 fadeIn"><span>'%(t)
    print TAB(9)+'Input range of percentages:<br/>'
    pmins[t].draw(9)
    print TAB(9)+'<br/>'
    pmaxs[t].draw(9)
    print TAB(8)+'</span></td>'
print TAB(7)+'</tr>'

print TAB(6)+'</table>'
print TAB(5)+'</td>'
print TAB(4)+'</tr>'
print TAB(3)+'</table>\n'

print TAB(3)+'<table>'

print TAB(4)+'<tr>'
print TAB(5)+'<td>'
print TAB(6)+'Functions to use in correlations:<br/><br/>\n'
print TAB(6)+'<table>'

print TAB(7)+'<tr>'
print TAB(8)+'<td>'
func_boxes[1].draw(9)
print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(7)+'</tr>'

if not func_boxes[1].getState():
    print TAB(7)+'<tr class="user fadeIn">'
else:
    print TAB(7)+'<tr class="user fadeOut">'
print TAB(8)+'<td>'
func_boxes[0].draw(9)
print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(7)+'</tr>'

print TAB(6)+'</table>\n'

if not func_boxes[1].getState():
    print TAB(6)+'<table class="user fadeOut">'
else:
    print TAB(6)+'<table class="user fadeIn">'
print TAB(7)+'<tr>'
print TAB(8)+'<td>'
user_func[0].draw(9)
print TAB(9)+'<br/><br/>'
user_func[1].draw(9)
print TAB(8)+'</td>'
print TAB(7)+'</tr>'
print TAB(6)+'</table>\n'

print TAB(5)+'</td>'
print TAB(4)+'</tr>'
print TAB(3)+'</table>\n'

print TAB(3)+'<table>'
print TAB(4)+'<tr>'


print TAB(5)+'<td>'
print "Data plot X range:<br/>"
auto_boxes[0].draw(6)
print TAB(6)+'<br/><br/>'
if auto_boxes[0].getState():
    print "<span class='auto0 fadeOut'>"
else:
    print "<span class='auto0 fadeIn'>"
bounds[0].draw(6)
print TAB(6)+'<br/><br/>'
bounds[1].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</span></td>'

print TAB(5)+'<td>'
print "Data plot &#x03bc; range:<br/>"
auto_boxes[1].draw(6)
print TAB(6)+'<br/><br/>'
if auto_boxes[1].getState():
    print "<span class='auto1 fadeOut'>"
else:
    print "<span class='auto1 fadeIn'>"
bounds[2].draw(6)
print TAB(6)+'<br/><br/>'
bounds[3].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</span></td>'

print TAB(5)+'<td>'
print "Histogram data range:<br/>"
auto_boxes[2].draw(6)
print TAB(6)+'<br/><br/>'
if auto_boxes[2].getState():
    print "<span class='auto2 fadeOut'>"
else:
    print "<span class='auto2 fadeIn'>"
bounds[4].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</span></td>'

print TAB(5)+'<td>'
print "Histogram X range:<br/>"
auto_boxes[3].draw(6)
print TAB(6)+'<br/><br/>'
if auto_boxes[3].getState():
    print "<span class='auto3 fadeOut'>"
else:
    print "<span class='auto3 fadeIn'>"
bounds[-2].draw(6)
print TAB(6)+'<br/><br/>'
bounds[-1].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</span></td>'


print TAB(4)+'</tr>'
print TAB(3)+'</table>'

#Reset and submit buttons
print "<br/>"
buttons[0].draw(2)
print "&nbsp;"
print TAB(2)+'</form>'
buttons[1].draw(2)
print TAB(2)+'<br/><br/><br/>\n'

#The complicated part
#If a form has been submitted - this not is the first time the page is being loaded
if len(form) != 0:

    #Write the Mathematica configuration file
    jobID = getJobID(exp_boxes,fig_boxes,func_boxes,auto_boxes,pdfset,sizes,vmins,vmaxs,pmins,pmaxs,user_func,bounds,selects,expids)

    #Check if the graph being requested has already been generated and stored

    #Check if the plot is there
    path = ROUTPUT+jobID
    images = sorted(glob.glob(path[1:]+"/*.png"))

    log("Writing configuration file")
    makeConfig(exp_boxes,fig_boxes,func_boxes,auto_boxes,pdfset,sizes,vmins,vmaxs,pmins,pmaxs,user_func,bounds,selects,expids)

    if len(images) != 0:
        #If it is, display and nicely format the generated images
        log("Displaying generated graphs\n")
        print TAB(2)+"<img src='%s'/><br/>"%(ROUTPUT+jobID+"/exptname_table.png")
        print "<a href='%s'>Download configuration file</a>"%(RMATH+CONFIG)

        print "<table style='width:100%'><tr>"
        for image in images:
            if "_xQ" in image:
                print "<td style='text-align:center;width:3000px;border:none'>"
                print TAB(2)+"<img style='width:100%;max-width:800px' src='."+image+"'/><br/>\n"
                print "<a href='."+image+"'>View full image</a></td>"
        print "</tr><br/><tr>"
        print "</tr></table>"

        print "<table style='width:30%'><tr>"

        for image in images:
            if "_legend" in image:
                print "<td style='border:none'>"
                print TAB(2)+"<img style='width:100%' src='."+image+"'/><br/>\n"
                print "<a href='."+image+"'>View full image</a></td>"
        print "</tr><br/><tr>"

        for image in images:
            if "_hist1" in image:
                print "<td style='border:none'>"
                print TAB(2)+"<img style='width:100%' src='."+image+"'/><br/>\n"
                print "<a href='."+image+"'>View full image</a></td>"
        print "</tr><br/><tr>"

        for image in images:
            if "_hist2" in image:
                print "<td style='border:none'>"
                print TAB(2)+"<img style='width:100%' src='."+image+"'/><br/>\n"
                print "<a href='."+image+"'>View full image</a></td>"
        print "<br/>"

        print "</tr></table>"
    else:
        #Check for the presence of the lockfile
        log("Checking if lockfile exists")
        present = glob.glob(AMATH+"lock")
        if len(present) == 0:
            #If it isn't there, run the program
            makeGraph(jobID)
        else:
            #Otherwise, read the job ID of the lockfile
            log("Attempting to open"+AMATH+"lock for reading")

            try:
                lockfile = file(AMATH+'lock','r')
                prevID = lockfile.readline()[:-1]
                lockfile.close()
            except:
                log("Failed with"+sys.exec_info()[0].__name__)
                raise

            log("Successfully read lock file")

            #See if an output with the corresponding ID already exists, and if it does, delete the lockfile and generate the new graph
            prevPath = ROUTPUT+prevID+IMAGE
            if len(glob.glob(prevPath[1:])) != 0:
                os.system('rm '+AMATH+'lock')
                makeGraph(jobID)
            else:
                #If not, assume another process is running and wait until an output with the lockfile ID has been generated, then reload the page
                log("Waiting for previous job to finish")
                print TAB(2)+"<h3 id='graph'>Waiting for another request to finish...</h3><br/>\n"
                print TAB(2)+'<script>var loop = setInterval(function() { if (UrlExists("%s")) { clearInterval(loop); location.reload();} }, 3000);</script>'%(prevPath)

else:
    #If the page IS being loaded for the first time...
    #TODO: Create a 'history' box of previous requests
    print TAB(2)+"<img id='graph' src='%sstate_default.jpg'/><br/>\n"%ASSETS
    #print TAB(2)+"Plots will be displayed here<br/><br/>\n"

print TAB(1)+'</body>'
print '</html>'
