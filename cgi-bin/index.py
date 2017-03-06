#!/bin/env python2

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

os.chdir("/home/sean/Programs/git-repos/physics-research/mathscript_v9/bin")

VERSION = '9'
ASSETS = '../../assets/'
MATH = './'
OUTPUT = '../../output/'

CONFIG = 'config1.txt'

#Function to make a lockfile, invoke the Mathematica program, and wait for the graph to be generated
def makeGraph():
    #Create lockfile containing signature of the image being generated
    lockfile = file(MATH+'lock','w+')
    lockfile.write("%s\n%s %s\n%s"%(newSignature,time.strftime("%X %x"),os.environ["REMOTE_ADDR"],os.environ['HTTP_USER_AGENT']))#Write signature, time and date of creation, invoking host ip, and user agent 
    lockfile.close()

    #Invoke the program
    os.system("math -script "+MATH+"correlation_plot_project_v"+VERSION+"_script.m > log.txt&")

    #Check every 2 seconds to see if the graph is done being generated, and display it when it is
    path = OUTPUT+"CT14NNLO_multi/"+newSignature+"/corrdr_hist1_f-4_SMmode1.eps"
    print "        <img id='graph' src='"+ASSETS+"state_loading.jpg'/>"
    print '''        <script>var loop = setInterval(function() { if (UrlExists("%s")) { clearInterval(loop); document.getElementById('graph').src = "%s"; }; }, 2000);</script>'''%(path,path)


expids = [701, 702, 703, 159, 101, 102, 103, 104, 106, 108, 109, 110, 111, 124, 125, 126, 127, 147, 201, 203, 204, 225, 227, 231, 234, 260, 261, 504, 514, 145, 169, 267, 268, 535, 240, 241, 281, 265, 266, 538]
#Create all the input elements that will be on the page
boxes = [mdl.CheckBox('type1','Experimental data points',False),mdl.CheckBox('type2','Experimental errors',False),mdl.CheckBox('type3','Residuals',False),mdl.CheckBox('type4','PDF errors on residuals',False),mdl.CheckBox('type5','type 5',False),mdl.CheckBox('type6','type 6',False),mdl.CheckBox('type7','type 7',False)]+[mdl.CheckBox('%d'%i,'Exp %d'%i,False) for i in expids]+[mdl.CheckBox('func1','b<span class="bar">&#x203e;</span>',False),mdl.CheckBox('func2','c<span class="bar">&#x203e;</span>',False),mdl.CheckBox('func3','s<span class="bar">&#x203e;</span>',False),mdl.CheckBox('func4','d<span class="bar">&#x203e;</span>',False),mdl.CheckBox('func5','u<span class="bar">&#x203e;</span>',False),mdl.CheckBox('func6','g',False),mdl.CheckBox('func7','u',False),mdl.CheckBox('func8','d',False),mdl.CheckBox('func9','s',False),mdl.CheckBox('func10','c',False),mdl.CheckBox('func11','b',False)]
radios = []
texts = [mdl.Text('xmin','X-min',1,0.00001),mdl.Text('xmax','X-max',1,1),mdl.Text('mumin','&#x03bc;-min',1,1.0),mdl.Text('mumax','&#x03bc;-max',1,2000),mdl.Text('xxmin','X-min',1,-3),mdl.Text('xxmax','X-max',1,3),mdl.Text('ymin','Y-min',1,0),mdl.Text('ymax','Y-max',1,10)]
buttons = [mdl.Button('button1','SUBMIT'),mdl.Button('button2','RESET','resetbutton')]

#Get data from submitted form
form = cgi.FieldStorage()

#Print packet header
print "Content-type:text/html\r\n"

#The HTML bit
print "<html>"

print "    <head>"
print "        <title>Physics test page</title>"

#Google Material Design stylesheets
print '''        <link rel="stylesheet" href="'''+ASSETS+'''ct66.css">
        <link rel="stylesheet" href="'''+ASSETS+'''mdl/material.min.css">
        <script src="'''+ASSETS+'''mdl/material.min.js"></script>
        <link rel="stylesheet" href="'''+ASSETS+'''mdl/icon.css">'''

#Other resources
print '''        <link rel="stylesheet" href="'''+ASSETS+'''index.css">
        <script src="'''+ASSETS+'''index.js"></script>'''


print "    </head>"

print "    <body>"
print "        <h1>LHC Particle Distributions</h1>"
print "        <h2>Southern Methodist University Physics Department</h2>"

#If A form has been submitted - this not is the first time the page is being loaded
if len(form) != 0:
    #Write the Mathematica configuration file
    configFile = open(MATH+CONFIG,'w+')
    configFile.write("#Version %s\n"%VERSION)
    configFile.write(time.strftime("#%X %x\n\n"))

    configFile.write("PDF set: CT14NNLO\n")

    #Also update input elements based on previous form submission and generate signature for this request
    info = ""

    typestr = ""
    flagstr = ""
    funcstr = ""
    for box in range(len(boxes)):
        boxes[box].checkState(form)
        if box < 7:
            typestr += "     %d"%boxes[box].getState()
        elif (box >= 7) and (box < 47):
            flagstr += "     %d"%boxes[box].getState()
        elif box >= 47:
            funcstr += "%d     "%boxes[box].getState()
    configFile.write("Type:  1     2     3     4     5     6     7\n")
    configFile.write("Flag:%s\n\n"%(typestr[3:]))
    configFile.write("Expt. ID:   701   702   703   159   101   102   103   104   106   108   109   110   111   124   125   126   127   147   201   203   204   225   227   231   234   260   261   504   514   145   169   267   268   535   240   241   281   265   266   538\n")
    configFile.write("Expt. Flag:%s\n\n"%(flagstr[3:]))

    configFile.write("Type:  bb   cb   sb     db    ub     g     u     d     s     c     b   user\n")
    configFile.write("Flag:  %s0\n\n"%funcstr)

    configFile.write("""Name: sigma_Higgs (pb)
Values: 0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   \n\n""")

    bounds = []
    for t in texts:
        t.checkState(form)
        bounds.append(float(t.getState()))

    configFile.write("xmin,   xmax:  %f   %f\n"%(bounds[0],bounds[1]))
    configFile.write("mumin, mumax:      %f %f\n\n"%(bounds[2],bounds[3]))

    configFile.write("Number of bins: auto\n")
    configFile.write("xmin, xmax: %f  %f\n"%(bounds[4],bounds[5]))
    configFile.write("ymin, ymax:  %f %f\n"%(bounds[6],bounds[7]))

    '''for rad in radios:
        rad.checkState(form)
        configFile.write("%s = %s ! %s\n"%(rad.labels[rad.getState()],rad.name,rad.name))
        info += str(rad.getState())'''

    '''configFile.write(OUTPUT+" = figureDirtag ! the directory used to save figure files\n")
    configFile.write("no = PDFsetDir !  PDFset directory\n")
    configFile.write("no = PDFsetmethod !  method used to generate PDFset\n")
    configFile.write("no = PDFDataDir !  directory storing f(x,Q) data\n")
    configFile.write(MATH+"dat16lisformathematica = datalist !  datalist with experimental information\n")

    for t in texts:
        t.checkState(form)
        configFile.write("%s = %s ! %s\n"%(t.getState(),t.name,t.label))
        info += str(t.getState())
    newSignature = ''.join([str(ord(i)) for i in info])
    configFile.write("CT14NNLO = PDFname !  PDFname of file storing f(x,Q) value\n")
    configFile.write("%s = exptid ! experimental ID\n"%newSignature)'''
    newSignature = ""

    configFile.close()

    #Check if the graph being requested has already been generated and stored

    #Check if the image is there
    path = OUTPUT+"CT14NNLO_multi/"+newSignature+"/corrdr_hist1_f-4_SMmode1.eps"
    if len(glob.glob(path)) != 0:
        #If it is, display it
        print "<img id='graph' src='"+path+"'/>"

    else:
        #Check for the presence of the lockfile. If it is not there...
        present = glob.glob(MATH+"lock")
        if len(present) == 0:
            #Run the program
            makeGraph()
        else:
            #Read the signature of the lockfile
            lockfile = file(MATH+'lock','r')
            signature = lockfile.readline()[:-1]
            mode = lockfile.readline()[:-1]
            lockfile.close()

            #See if an image with a corresponding signature already exists, and if it does, delete the lockfile and generate the new graph
            path_prev = OUTPUT+"CT14NNLO_multi/"+signature+"/corrdr_hist1_f-4_SMmode1.eps"
            if len(glob.glob(path_prev)) != 0:
                os.system('rm '+MATH+'lock')
                makeGraph()
            else:
                #If not, wait until the image with the same signature has been generated, then reload the page
                print "        <img id='graph' src='"+ASSETS+"state_busy.jpg'/>"
                print '''        <script>var loop = setInterval(function() { if (UrlExists("%s")) { clearInterval(loop); location.reload();} }, 3000);</script>'''%(path_prev)
else:
    print "        <img id='graph' src='"+ASSETS+"state_default.jpg'/>"

#Generate the actual HTML form and draw all the input elements
print "<br/>"
buttons[1].draw()
print '        <form action="index.py" method="post" style="display:inline">'
buttons[0].draw()
print '<br/>'

print 'x-Q Figure Parameters: '
for t in texts[:4]:
    t.draw()
    print '&nbsp;'
print '<br/>'
print 'Hisotogram Figure Parameters: '
for t in texts[4:]:
    t.draw()
    print '&nbsp;'

print '            <table>'

print '                <tr>'

print '                    <td>'

print 'Figures to plot:<br/><br/>'

for box in boxes[:7]:
    box.draw()
    print "<br/>"

print '                    </td>'

print '                    <td>'

print 'Experiments to include:<br/>'

print '                        <table>'
print '                            <tr>'
print '                                <td style="width:20%;border:none">'
for box in boxes[7:17]:
    box.draw()
    print '<br/>'
print '                                </td>'
print '                                <td style="width:20%;border:none">'
for box in boxes[17:27]:
    box.draw()
    print '<br/>'
print '                                </td>'
print '                                <td style="width:20%;border:none">'
for box in boxes[27:37]:
    box.draw()
    print '<br/>'
print '                                </td>'
print '                                <td style="width:20%;border:none">'
for box in boxes[37:47]:
    box.draw()
    print '<br/>'
print '                                </td>'
print '                            </tr>'
print '                        </table>'

print '                    </td>'

print '                </tr>'

print '                <tr>'

print '                    <td>'

print 'Functions to use in correlations:<br/><br/>'

for box in boxes[47:58]:
    box.draw()
    print '<br/>'

print '                    </td>'

print '<td>'

print '''sigma_Higgs (pb)<br/>
0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012<br/>
0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012<br/>
0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012<br/>
0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012   0.012'''

print '</td>'

print '</tr>'

print '            </table>'

'''if not boxes[0].getState():
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
print '            </table>' '''

print '        </form>'

print '    </body>'
print '</html>'
