#!/bin/env python2

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

VERSION = '15' #Version of the Mathematica script
ASSETS = '../../assets/' #Path to HTML page assets
MATH = './' #Path to Mathematica script bin
OUTPUT = '../plots/Jobs/' #Path to output directory - the script will create a separate  folder here for each unique job ID

CONFIG = 'config1.txt' #Name of config file to be generated in the Mathematica script bin directory
IMAGE = 'dr_xQ.jpg'

#Set current working directory
os.chdir("/home/sean/Programs/git-repos/physics-research/mathscript_v%s/bin"%VERSION)


def TAB(num):
    '''TAB(int num)
    num: number of indentations
    return: a string of whitespace of length num*4
    Generates a string of num tabs, for formatting the generated HTML nicely'''

    return ('    '*num)

def makeConfig(boxes,radios,texts,selects):
    '''makeConfig(List boxes, List radios, List texts)
    boxes: list of mdl CheckBox elements
    radios: list of mdl RadioBox elements
    texts: list of mdl Text elements
    return: the job ID associated with the submission
    Generates and writes the configuration file for the given set of parameters'''

    #Open the file itself
    configFile = open(MATH+CONFIG,'w+')
    #Write the version of the Mathematica script
    configFile.write("#Version %s\n"%VERSION)
    #Write the timestamp
    configFile.write(time.strftime("#%X %x\n\n"))

    #Generate the job ID
    boxVals = ''.join([str(int(box.getState())) for box in boxes])
    boxStr = ''.join([str(int(boxVals[4*i:4*i+4],2)) for i in range(len(boxVals)/4)])

    radStr = ''.join([str(rad.getState()) for rad in radios])

    textVals = ''.join([t.getState() for t in texts])
    textStr = ''.join([str(ord(i)-45) for i in textVals])

    jobID = boxStr+radStr+textStr

    #Job ID
    configFile.write("Job ID (copy from the counter file): %s\n"%jobID)

    #PDF set
    configFile.write("PDF set: CT14NNLO\n\n")

    #Generate the sections for figure to plot, experiments to include, and functions to use
    typestr = ""
    flagstr = ""
    funcstr = ""
    for box in boxes[111:117]:
        typestr += "     %d"%box.getState()
    for box in boxes[0:111]:
        flagstr += "     %d"%box.getState()
    for box in boxes[117:132]:
        funcstr += "%d     "%box.getState()

    #Type
    configFile.write("Type:  1     2     3     4     5     6     7\n")
    #Flag
    configFile.write("Flag:%s0\n\n"%(typestr[3:]))

    #Expt. ID
    configFile.write("Expt. ID:   701   702   703   159   101   102   103   104   106   108   109   110   111   124   125   126   127   147   201   203   204   225   227   231   234   260   261   504   514   145   169   267   268   535   240   241   281   265   266   538\n")
    #Expt. Flag
    configFile.write("Expt. Flag:%s\n\n"%(flagstr[3:]))

    #Type
    configFile.write("Type:  bb   cb   sb     db    ub     g     u     d     s     c     b   user\n")
    #Flag
    configFile.write("Flag:  %s\n\n"%funcstr)

    #Name
    configFile.write("Name: %s"%texts[20])
    #Values
    configFile.write("Values: %s\n\n"%texts[21])

    bounds = []
    for t in texts[22:29]:
        print t.getState()
        bounds.append(float(t.getState()))

    for i in range(133,137):
        if boxes[i].getState():
            bounds[i-133] = 'auto'

    configFile.write("xmin,   xmax:  %f   %f\n"%(bounds[0],bounds[1]))
    configFile.write("mumin, mumax:      %f %f\n\n"%(bounds[2],bounds[3]))

    configFile.write("Number of bins: %f\n")%bounds[4]
    configFile.write("xmin, xmax: %f  %f\n"%(bounds[5],bounds[6]))
    configFile.write("ymin, ymax:  0 auto\n")

    configFile.write("Color by data percentage: 50 70 85")

    configFile.write("Size: %s"%(radios[1].getState()))

    configFile.write("Type:  1     2     3     4     5     6     7")
    configFile.write("Mode:  0     %d     %d     %d     %d     %d     0"%(selects[0].getState(),selects[1].getState(),selects[2].getState(),selects[3].getState(),selects[4].getState()))

    vals = [float(texts[0].getState()),float(texts[1].getState()),float(texts[4].getState()),float(texts[5].getState()),float(texts[8].getState()),float(texts[9].getState()),float(texts[12].getState()),float(texts[13].getState()),float(texts[16].getState()),float(texts[17].getState())]
    percs = [float(texts[2].getState()),float(texts[3].getState()),float(texts[6].getState()),float(texts[7].getState()),float(texts[10].getState()),float(texts[11].getState()),float(texts[14].getState()),float(texts[15].getState()),float(texts[18].getState()),float(texts[19].getState())]
    configFile.write("Mode 1 range: 0.0  0.0 %f  %f %f  %f %f  %f %f  %f %f  %f 0.0  0.0"%(vals[0],vals[1],vals[2],vals[3],vals[4],vals[5],vals[6],vals[7],vals[8],vals[9]))
    configFile.write("Mode 2 range: 0.0  0.0 %f  %f %f  %f %f  %f %f  %f %f  %f 0.0  0.0"%(percs[0],percs[1],percs[2],percs[3],percs[4],percs[5],percs[6],percs[7],percs[8],percs[9]))


    configFile.close()

    return jobID

def makeGraph(jobID):
    '''makeGraph(string jobID)
    jobID: the ID of the job that is invoking the Mathematica script
    Make a lockfile, invoke the Mathematica script, wait until the graph has been generated, and display it'''

    #Create lockfile containing job ID of the image being generated
    lockfile = file(MATH+'lock','w+')
    lockfile.write("%s\n%s\n%s\n%s\n"%(jobID,time.strftime("%X %x"),os.environ["REMOTE_ADDR"],os.environ['HTTP_USER_AGENT']))#Write job ID, time and date of creation, invoking host ip, and user agent 
    lockfile.close()

    #Invoke the program
    os.system("math -script "+MATH+"correlation_plot_project_v"+VERSION+"_script.m > log.txt&")

    #Check every 2 seconds to see if the graph is done being generated, and display it when it is
    path = OUTPUT+jobID+IMAGE
    print TAB(2)+"<img id='graph' src='"+ASSETS+"state_loading.jpg'/><br/>\n"
    print TAB(2)+'''<script>var loop = setInterval(function() { if (UrlExists("%s")) { clearInterval(loop); document.getElementById('graph').src = "%s"; }; }, 2000);</script>'''%(path,path)


#Create all the input elements that will be on the page

#Generate list of experiment IDs and their associated string names (currently 111 elements long)
idFile = file(MATH+'exptidname.txt','r')
expids = [i.split() for i in idFile.readlines()[3:] if i[:3] != '000']
expids = [i for i in expids if len(i) != 0]

#Checkboxes
boxes = [
    #Experiments to include [0:111]
    mdl.CheckBox('allexps','All',False,'expid','checkAllExps();') #'Select all' box
 ]+[mdl.CheckBox(i[0],i[1],False,'expid') for i in expids]+[

    #Figures to plot [111:117]
    mdl.CheckBox('type1','Experimental data points',False),
    mdl.CheckBox('type2','Experimental errors',False),
    mdl.CheckBox('type3','Residuals',False),
    mdl.CheckBox('type4','PDF errors on residuals',False),
    mdl.CheckBox('type5','Sensitivity factor',False),
    mdl.CheckBox('type6','Correlation',False),

    #Functions to use [117:132]
    mdl.CheckBox('func1','b<span class="bar">&#x203e;</span>',False),
    mdl.CheckBox('func2','c<span class="bar">&#x203e;</span>',False),
    mdl.CheckBox('func3','s<span class="bar">&#x203e;</span>',False),
    mdl.CheckBox('func4','d<span class="bar">&#x203e;</span>',False),
    mdl.CheckBox('func5','u<span class="bar">&#x203e;</span>',False),
    mdl.CheckBox('func6','g',False),
    mdl.CheckBox('func7','u',False),
    mdl.CheckBox('func8','d',False),
    mdl.CheckBox('func9','s',False),
    mdl.CheckBox('func10','c',False),
    mdl.CheckBox('func11','b',False),
    mdl.CheckBox('func12','q6',False),
    mdl.CheckBox('func13','q7',False),
    mdl.CheckBox('func14','q8',False),
    mdl.CheckBox('func15','user',False),

    #Figure range 'auto' boxes [132:136]
    mdl.CheckBox('xauto','Auto',True),
    mdl.CheckBox('muauto','Auto',True),
    mdl.CheckBox('hxauto','Auto',True),
    mdl.CheckBox('yauto','Auto',True)
]

#Radio buttons
radios = [
    #PDF set [0]
    mdl.RadioBox('pdfset',['CT14NNLO']),

    #Point size [1]
    mdl.RadioBox('pointsize',['Tiny','Small','Medium','Large'])
]

#Text inputs
texts = [
    #Value and percentage ranges for highlight mode functions 2-6 [0:20]
    mdl.Text('vmin2','Min',0),
    mdl.Text('vmax2','Max',0),
    mdl.Text('pmin2','Min',0),
    mdl.Text('pmax2','Max',0),
    mdl.Text('vmin3','Min',0),
    mdl.Text('vmax3','Max',0),
    mdl.Text('pmin3','Min',0),
    mdl.Text('pmax3','Max',0),
    mdl.Text('vmin4','Min',0),
    mdl.Text('vmax4','Max',0),
    mdl.Text('pmin4','Min',0),
    mdl.Text('pmax4','Max',0),
    mdl.Text('vmin5','Min',0),
    mdl.Text('vmax5','Max',0),
    mdl.Text('pmin5','Min',0),
    mdl.Text('pmax5','Max',0),
    mdl.Text('vmin6','Min',0),
    mdl.Text('vmax6','Max',0),
    mdl.Text('pmin6','Min',0),
    mdl.Text('pmax6','Max',0),

    #User function parameter [20:22]
    mdl.Text('userparamname','Name',0),
    mdl.Text('57values','Enter 57 values',0),

    #Window bounds for graph [22:31]
    mdl.Text('xmin','X-min',1,0.00001),
    mdl.Text('xmax','X-max',1,1),
    mdl.Text('mumin','&#x03bc;-min',1,1.0),
    mdl.Text('mumax','&#x03bc;-max',1,2000),
    mdl.Text('nbin','Nbin',1),
    mdl.Text('hxmin','X-min',1,-3),
    mdl.Text('hxmax','X-max',1,3),
    mdl.Text('ymin','Y-min',1,0),
    mdl.Text('ymax','Y-max',1,10)
]

#Select elements
selects = [
    #Dropdowns for highlight mode (figures 2-6) [0:5]
    #mdl.Select('wtype1',['No highlighting','Value range','Percentage range']),
    mdl.Select('wtype2',['No highlighting','Value range','Percentage range']),
    mdl.Select('wtype3',['No highlighting','Value range','Percentage range']),
    mdl.Select('wtype4',['No highlighting','Value range','Percentage range']),
    mdl.Select('wtype5',['No highlighting','Value range','Percentage range']),
    mdl.Select('wtype6',['No highlighting','Value range','Percentage range'])
]

#Buttons
buttons = [
    mdl.Button('button1','SUBMIT'), #[0]
    mdl.Button('button2','RESET','resetbutton') #[1]
]


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


#The complicated part
#If a form has been submitted - this not is the first time the page is being loaded
if len(form) != 0:
    #Update input elements based on previous form submission
    for box in boxes:
        box.checkState(form)
    for rad in radios:
        rad.checkState(form)
    for s in selects:
        s.checkState(form)
    for t in texts:
        t.checkState(form)

    #Write the Mathematica configuration file
    jobID = makeConfig(boxes,radios,texts,selects)

    #Check if the graph being requested has already been generated and stored

    #Check if the image is there
    path = OUTPUT+jobID+IMAGE
    if len(glob.glob(path)) != 0:
        #If it is, display it
        print TAB(2)+"<img id='graph' src='%s'/><br/>\n"%path
    else:
        #Check for the presence of the lockfile
        present = glob.glob(MATH+"lock")
        if len(present) == 0:
            #If it isn't there, run the program
            makeGraph()
        else:
            #Otherwise, read the job ID of the lockfile
            lockfile = file(MATH+'lock','r')
            prevID = lockfile.readline()[:-1]
            lockfile.close()

            #See if an output with the corresponding ID already exists, and if it does, delete the lockfile and generate the new graph
            prevPath = OUTPUT+prevID+IMAGE
            if len(glob.glob(prevPath)) != 0:
                os.system('rm '+MATH+'lock')
                makeGraph()
            else:
                #If not, assume another process is running and wait until an output with the lockfile ID has been generated, then reload the page
                print TAB(2)+"<img id='graph' src='%sstate_busy.jpg'/><br/>\n"%ASSETS
                print TAB(2)+'<script>var loop = setInterval(function() { if (UrlExists("%s")) { clearInterval(loop); location.reload();} }, 3000);</script>'%prevPath

else:
    #If the page IS being loaded for the first time, display the default image
    print TAB(2)+"<img id='graph' src='%sstate_default.jpg'/><br/>\n"%ASSETS


#Generate the actual HTML form and draw all the input elements
#Reset and submit buttons
buttons[1].draw(2)
print TAB(2)+'<form action="index.py" method="post" style="display:inline">'
buttons[0].draw(2)
print TAB(2)+'<br/>\n'

'''print 'x-Q Figure Parameters: '
for t in texts[:4]:
    t.draw()
    print '&nbsp;'
print '<br/>'
print 'Hisotogram Figure Parameters: '
for t in texts[4:]:
    t.draw()
    print '&nbsp;'
'''

print TAB(3)+'<table>'
print TAB(4)+'<tr>'
print TAB(5)+'<td>'

#PDFset and size of points
print TAB(6)+'Choose PDF set:<br/>'
radios[0].draw(6)
print TAB(6)+'<br/>\n'
print TAB(6)+'Size of data points:<br/>'
radios[1].draw(6)

print TAB(5)+'</td>\n'

#Experiment IDs
print TAB(5)+'<td>'
print TAB(6)+'Experiments to include:<br/>'

print TAB(6)+'<table>'

print TAB(7)+'<tr>'
print TAB(8)+'<td style="width:20%;border:none">'
boxes[0].draw(9)
print TAB(8)+'</td>'
print TAB(7)+'</tr>\n'

print TAB(7)+'<tr>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in boxes[1:20]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in boxes[20:39]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in boxes[39:58]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in boxes[58:77]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in boxes[77:96]:
    box.draw(9)
    print TAB(9)+'<br/>'
print TAB(8)+'</td>'
print TAB(8)+'<td style="width:20%;border:none">'
for box in boxes[96:112]:
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
for box in boxes[112:118]:
    print TAB(8)+'<td style="width: 30%">'
    box.draw(9)
    print TAB(8)+'</td>'
print TAB(7)+'</tr>\n'

print TAB(7)+'<tr>'
print TAB(8)+'<td></td>'
for s in selects[0:5]:
    print TAB(8)+'<td>'
    print TAB(9)+'Highlight mode:<br/>'
    s.draw(9)
    print TAB(8)+'</td>'
print TAB(7)+'</tr>\n'

print TAB(7)+'<tr>'
print TAB(8)+'<td></td>'
for t in range(5):
    print TAB(8)+'<td>'
    print TAB(9)+'Input range of values:<br/>'
    texts[4*t].draw(9)
    print TAB(9)+'<br/>'
    texts[4*t +1].draw(9)

    print TAB(9)+'<br/><br/>'

    print TAB(9)+'Input range of percentages:<br/>'
    texts[4*t +2].draw(9)
    print TAB(9)+'<br/>'
    texts[4*t +3].draw(9)
    print TAB(8)+'</td>'
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

for box in boxes[118:133]:
    print TAB(8)+'<td>'
    box.draw(9)
    print TAB(9)+'<br/>'
    print TAB(8)+'</td>'

print TAB(7)+'</tr>'
print TAB(6)+'</table>\n'

print TAB(6)+'<table>'
print TAB(7)+'<tr>'
print TAB(8)+'<td>'
texts[20].draw(9)
print TAB(9)+'<br/><br/>'
texts[21].draw(9)
print TAB(8)+'</td>'
print TAB(7)+'</tr>'
print TAB(6)+'</table>\n'

print TAB(5)+'</td>'
print TAB(4)+'</tr>'
print TAB(3)+'</table>\n'

print TAB(3)+'<table>'
print TAB(4)+'<tr>'


print TAB(5)+'<td>'
boxes[133].draw(6)
print TAB(6)+'<br/><br/>'
texts[22].draw(6)
print TAB(6)+'<br/><br/>'
texts[23].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</td>'

print TAB(5)+'<td>'
boxes[134].draw(6)
print TAB(6)+'<br/><br/>'
texts[24].draw(6)
print TAB(6)+'<br/><br/>'
texts[25].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</td>'

print TAB(5)+'<td>'
boxes[135].draw(6)
print TAB(6)+'<br/><br/>'
texts[26].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</td>'

print TAB(5)+'<td>'
boxes[136].draw(6)
print TAB(6)+'<br/><br/>'
texts[27].draw(6)
print TAB(6)+'<br/><br/>'
texts[28].draw(6)
print TAB(6)+'<br/>'
print TAB(5)+'</td>'


print TAB(4)+'</tr>'
print TAB(3)+'</table>'

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

print TAB(2)+'</form>'

print TAB(1)+'</body>'
print '</html>'
