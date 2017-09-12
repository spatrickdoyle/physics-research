import os,time,glob
from elements import *

def TAB(num):
    #TAB(int num)
    #num: number of indentations
    #return: a string of whitespace of length num*4
    #Generates a string of num tabs, for formatting the generated HTML nicely

    return ('    '*num)

def makeConfig(exp_boxes,fig_boxes,func_boxes,auto_boxes,pdfset,sizes,vmins,vmaxs,pmins,pmaxs,user_func,bounds,selects,expids):
    #makeConfig(List boxes, List radios, List texts)
    #expids: list of experiment ids to include in the config file
    #return: the job ID associated with the submission
    #Generates and writes the configuration file for the given set of parameters

    #print os.getcwd()+"<br/>"
    testFile = file(MATH+CONFIG,'r')
    #for line in testFile.readlines():
    #    print line+"<br/>"
    testFile.close()
    testFile = file(MATH+CONFIG,'w')
    testFile.write("THIS IS A TEST")
    testFile.close()


    #Open the file itself
    configFile = open(MATH+CONFIG,'w+')
    #Write the version of the Mathematica script
    configFile.write("#Version %s\n"%VERSION)
    #Write the timestamp
    configFile.write(time.strftime("#%X %x\n\n"))

    #Generate the job ID
    all_elements = exp_boxes+fig_boxes+func_boxes+auto_boxes+[pdfset]+sizes+vmins+vmaxs+pmins+pmaxs+user_func+bounds+selects

    jobStr = ''.join([str(t.getState()) for t in all_elements])
    jobID = str(int(sum([(ord(jobStr[i])-45)*i for i in range(len(jobStr))])))

    #Job ID
    configFile.write("Job ID (copy from the counter file): %s\n"%jobID)

    #PDF set
    configFile.write("PDF set: CT14NNLO\n\n")

    #Generate the sections for figure to plot, experiments to include, and functions to use
    typestr = [0 for i in range(6)]
    flagstr = ""
    funcstr = [0 for i in range(15)]
    figures = []

    #Should the data points be displayed?
    if fig_boxes[0].getState():
        typestr[0] = 1
    #typestr[int(boxes[exp_boxes+1].getState())+1] = 1
    typestr[int(fig_boxes[1].getState())+1] = 1
    typestr = "     "+("     ".join([str(i) for i in typestr]))
    #for box in boxes[exp_boxes:exp_boxes+fig_boxes]:
    #    typestr += "     %d"%box.getState()
    #    figures.append(box.getState())
    for box in exp_boxes[1:]:
        flagstr += "     %d"%box.getState()
    #for box in boxes[exp_boxes+fig_boxes:exp_boxes+fig_boxes+func_boxes]:
    #    funcstr += "%d     "%box.getState()
    if func_boxes[-1].getState():
        funcstr[-1] = 1
    #funcstr[int(boxes[exp_boxes+fig_boxes].getState())] = 1
    funcstr[int(func_boxes[0].getState())] = 1
    funcstr = ("     ".join([str(i) for i in funcstr]))

    #Type
    configFile.write("Type:  1     2     3     4     5     6     7\n")
    #Flag
    configFile.write("Flag:%s     0\n\n"%(typestr[3:]))

    #Expt. ID
    configFile.write("Expt. ID:   "+''.join([i[0]+"   " for i in expids])+"\n")
    #Expt. Flag
    configFile.write("Expt. Flag:%s\n\n"%(flagstr[3:]))

    #Type
    configFile.write("Type:  bb   cb   sb     db    ub     g     u     d     s     c     b    q6   q7    q8   user\n")
    #Flag
    configFile.write("Flag:  %s\n\n"%funcstr)

    #Name
    configFile.write("Name: %s\n"%user_func[0].getState())
    #Values
    configFile.write("Values: %s\n\n"%user_func[1].getState())

    bounds_ = []
    for t in bounds[:5]+bounds[6:]:
        bounds_.append(t.getState())

    for i in range(len(auto_boxes)/2):
        if auto_boxes[i].getState():
            bounds_[2*i] = 'auto'
            bounds_[2*i + 1] = 'auto'

    configFile.write("xmin,   xmax:  %s   %s\n"%(bounds_[0],bounds_[1]))
    configFile.write("mumin, mumax:      %s %s\n\n"%(bounds_[2],bounds_[3]))

    configFile.write("Number of bins: %s\n"%(bounds_[4]))
    configFile.write("xmin, xmax: %s  %s\n"%(bounds_[6],bounds_[7]))
    configFile.write("ymin, ymax:  0 auto\n\n")

    configFile.write("Color by data percentage: 50 70 85\n\n")

    configFile.write("Type:  1     2     3     4     5     6     7\n")
    configFile.write("Mode:  0     %d     %d     %d     %d     %d     0\n"%(selects[0].getState(),selects[1].getState(),selects[2].getState(),selects[3].getState(),selects[4].getState()))

    configFile.write("Mode 1 range: 0.0  0.0 %s  %s %s  %s %s  %s %s  %s %s  %s 0.0  0.0\n"%(vmins[0].getState(),vmaxs[0].getState(),vmins[1].getState(),vmaxs[1].getState(),vmins[2].getState(),vmaxs[2].getState(),vmins[3].getState(),vmaxs[3].getState(),vmins[4].getState(),vmaxs[4].getState()))
    configFile.write("Mode 2 range: 0.0  0.0 %s  %s %s  %s %s  %s %s  %s %s  %s 0.0  0.0\n\n"%(pmins[0].getState(),pmaxs[0].getState(),pmins[1].getState(),pmaxs[1].getState(),pmins[2].getState(),pmaxs[2].getState(),pmins[3].getState(),pmaxs[3].getState(),pmins[4].getState(),pmaxs[4].getState()))

    configFile.write("Size: %s\n\n"%(sizes[0].labels[int(sizes[int(fig_boxes[1].getState())].getState())].lower()))


    configFile.close()

    #IMAGE = []
    #obsname = ["xQbyexpt","expt_error_ratio","residue","dr","corrdr","corr"]
    #for i in range(6):
    #    if figures[i]:
    #        IMAGE.append(obsname[i]+"_xQ.png")
    #return (jobID,IMAGE)
    return jobID

def makeGraph(jobID):
    #makeGraph(string jobID)
    #jobID: the ID of the job that is invoking the Mathematica script
    #Make a lockfile, invoke the Mathematica script, wait until the graph has been generated, and display it

    #Create lockfile containing job ID of the image being generated
    lockfile = file(MATH+'lock','w+')
    lockfile.write("%s\n%s\n%s\n%s\n"%(jobID,time.strftime("%X %x"),os.environ["REMOTE_ADDR"],os.environ['HTTP_USER_AGENT']))#Write job ID, time and date of creation, invoking host ip, and user agent 
    lockfile.close()

    #Invoke the program
    os.system("math -script "+MATH+"correlation_plot_project_v"+VERSION+"_script.m > log.txt&")

    #Check every 2 seconds to see if the graph is done being generated, and display it when it is
    path = JS_PREFIX+OUTPUT+jobID+IMAGE
    #print TAB(2)+"<img id='graph' src='"+ASSETS+"state_loading.jpg'/><br/>\n"
    print TAB(2)+"<h3 id='graph'>Loading...<h3/><br/>\n"
    #print TAB(2)+"""<script>var loop = setInterval(function() { if (UrlExists("%s")) { clearInterval(loop); document.getElementById('graph').src = "%s"; }; }, 2000);</script>"""%(path,path)
    print TAB(2)+"""<script>var loop = setInterval(function() { if (UrlExists("%s")) { clearInterval(loop); document.getElementById('graph').innerHTML = "<a onclick='window.location.reload()'>Click to view plots</a>"; }; }, 2000);</script>"""%(path)
