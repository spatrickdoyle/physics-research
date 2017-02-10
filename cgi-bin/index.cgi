#!/bin/env python2

#TODO:
#Update all parameters
#Make the form update dynamically while parameters are being entered
#Make button for resetting parameters
#Make back button
#Change stylesheet links to fetch Google MDL resources locally
#Make script generate better images
#Test with AWS
#Remove CGI debugging stuff
#Make form POST, not GET

# Import module for CGI handling 
import cgi

#Object wrappers for Google Material Design components
import mdl

#For debugging - remove or comment when complete
import cgitb
cgitb.enable()

#Other things
import glob,os,time


VERSION = '0.0'

#Function to make a lockfile, invoke the Mathematica program, and wait for the graph to be generated
def makeGraph():
    #Create lockfile containing signature of the image being generated
    lockfile = file('lockfile','w+')
    lockfile.write("%s\n%s %s\n%s"%(newSignature,time.strftime("%X %x"),os.environ["REMOTE_ADDR"],os.environ['HTTP_USER_AGENT']))#Write signature, time and date of creation, invoking host ip, and user agent 
    lockfile.close()

    #Invoke the program
    os.system("./script.py&")

    #Check every 2 seconds to see if the graph is done being generated, and display it when it is
    print "        <img id='graph' src='../loading.png'/>"
    print '''        <script>var loop = setInterval(function() { if (UrlExists("../images/%s.jpg")) { clearInterval(loop); document.getElementById('graph').src = "../images/%s.jpg"; }; }, 2000);</script>'''%(newSignature,newSignature)


#Create all the input elements that will be on the page
boxes = [mdl.CheckBox('box0','Make more things appear',False,"toggleBlock2()")]
radios = [mdl.RadioBox('PDFsetmethod',['Hessian','MC'],0),mdl.RadioBox('plottype',['single','multi','ALL','ProtonNutron'],0)]
texts = []
buttons = [mdl.Button('button1','SUBMIT')]

#Get data from submitted form
form = cgi.FieldStorage()

#Print packet header
print "Content-type:text/html\r\n"

#The HTML bit
print "<html>"

print "    <head>"
print "        <title>Physics test page</title>"

#Links to Google Material Design stylesheets - DOWNLOAD THESE INSTEAD OF FETCHING THEM
print '''        <link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
        <link rel="stylesheet" href="https://code.getmdl.io/1.3.0/material.indigo-pink.min.css">
        <script defer src="https://code.getmdl.io/1.3.0/material.min.js"></script>
        <link rel="stylesheet" href="../index.css">
        <script src="../index.js"></script>'''
print "    </head>"

print "    <body>"
print "        <h1>Enter parameters for the particle reaction</h1>"

#If A form has been submitted - this not is the first time the page is being loaded
if len(form) != 0:
    #Write the Mathematica configuration file
    configFile = open('image.cfg','w+')
    configFile.write("#Version %s\n"%VERSION)
    configFile.write(time.strftime("#%X %x\n"))

    #Also update input elements based on previous form submission and generate signature for this request
    info = ""
    for box in boxes:
        box.checkState(form)
        #configFile.write("%s = %s ! %s\n"%(box.getState(),box.name,box.label))
        #info += str(box.getState())
    for rad in radios:
        rad.checkState(form)
        configFile.write("%s = %s ! %s\n"%(rad.labels[rad.getState()],rad.name,rad.name))
        info += str(rad.getState())
    for t in texts:
        t.checkState(form)
        configFile.write("%s = %s ! %s\n"%(t.getState(),t.name,t.label))
        info += str(t.getState())
    newSignature = ''.join([str(ord(i)) for i in info])
    configFile.write("%s = exptid ! experimental ID\n"%newSignature)

    configFile.close()

    #Check if the graph being requested has already been generated and stored

    #Check if the image is there
    if len(glob.glob("images/"+newSignature+".jpg")) != 0:
        #If it is, display it
        print "<img id='graph' src='../images/"+newSignature+".jpg'/>"

    else:
        #Check for the presence of the lockfile. If it is not there...
        present = glob.glob("lockfile")
        if len(present) == 0:
            #Run the program
            makeGraph()
        else:
            #Read the signature of the lockfile
            lockfile = file('lockfile','r')
            signature = lockfile.readline()[:-1]
            lockfile.close()

            #See if an image with a corresponding signature already exists, and if it does, delete the lockfile and generate the new graph
            if len(glob.glob("images/"+signature+".jpg")) != 0:
                os.system('rm lockfile')
                makeGraph()
            else:
                #If not, wait until the image with the same signature has been generated, then reload the page
                print "        <img id='graph' src='../busy.png'/>"
                print '''        <script>var loop = setInterval(function() { if (UrlExists("../images/%s.jpg")) { clearInterval(loop); location.reload();} }, 3000);</script>'''%(signature)
else:
    print "        <img id='graph' src='../first.png'/>"

#Generate the actual HTML form and draw all the input elements
print '        <form action="index.cgi" method="get">'

for b in buttons:
    b.draw()

print '            <table>'
print '                <tr>'
print '                    <td>'
print 'Method used to generate PDFset:<br/>'
radios[0].draw()
print '                    </td>'
print '                    <td>'
boxes[0].draw()
print '                    </td>'
print '                </tr>'
print '            </table>'

if not boxes[0].getState():
    print '            <table id="level2" class="fadeOut">'
else:
    print '            <table id="level2" class="fadeIn">'
print '                <tr>'
print '                    <td>'
print 'The second beam consists of:<br/>'
radios[1].draw()
print '                    </td>'
print '                    <td>'
print "something else"
print '                    </td>'
print '                </tr>'
print '            </table>'

print '        </form>'

print '    </body>'
print '</html>'
