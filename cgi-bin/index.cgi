#!/bin/env python2

# Import modules for CGI handling 
import cgi,glob,os
import cPickle as pickle
import cgitb
cgitb.enable()


class CheckBox:
    def __init__(self,name,label,default=False):
        self.name = name
        self.label = label
        if default:
            self.state = " checked"
        else:
            self.state = ""

    def draw(self):
        print '<label class="mdl-checkbox mdl-js-checkbox" for="'+self.name+'">'
        print '<input type="checkbox" id="'+self.name+'" name="'+self.name+'" class="mdl-checkbox__input"'+self.state+'>'
        print '<span class="mdl-checkbox__label">'+self.label+'</span>'
        print '</label>'

    def checkState(self,form):
        if form.getvalue(self.name) == 'on':
            self.changeState(True)
        else:
            self.changeState(False)

    def changeState(self,state):
        if state:
            self.state = " checked"
        else:
            self.state = "";

    def getState(self):
        if self.state == "":
            return False
        else:
            return True

class RadioBox:
    def __init__(self,name,labels,default=0):
        self.name = name
        self.labels = labels
        self.states = ["" for i in range(len(labels))]
        self.states[default] = " checked"

    def draw(self):
        for i in range(len(self.labels)):
            print '<label class="mdl-radio mdl-js-radio" for="'+self.name+'-'+str(i)+'">'
            print '<input type="radio" id="'+self.name+'-'+str(i)+'" class="mdl-radio__button" name="'+self.name+'" value="'+str(i)+'"'+self.states[i]+'>'
            print '<span class="mdl-radio__label">'+self.labels[i]+'</span>'
            print '</label><br/>'

    def checkState(self,form):
        self.changeState(int(form.getvalue(self.name)))

    def changeState(self,state):
        self.states = ["" for i in range(len(self.labels))]
        self.states[state] = " checked"

    def getState(self):
        return self.states.index(" checked")

class Text:
    def __init__(self,name,label,default=None):
        self.name = name
        self.label = label
        if default == None:
            self.state = ""
        else:
            self.state = ' value="'+default+'"'

    def draw(self):
        print '<div class="mdl-textfield mdl-js-textfield mdl-textfield--floating-label">'
        print '<input class="mdl-textfield__input" type="text" pattern="-?[0-9]*(\.[0-9]+)?" name="'+self.name+'" id="'+self.name+'"'+self.state+'>'
        print '<label class="mdl-textfield__label" for="'+self.name+'">'+self.label+'</label>'
        print '<span class="mdl-textfield__error">Enter a number</span>'
        print '</div><br/>'

    def checkState(self,form):
        self.changeState(form.getvalue(self.name))

    def changeState(self,state):
        if state == None:
            self.state = ""
        else:
            self.state = ' value="'+state+'"'

    def getState(self):
        return self.state[8:-1]

class Button:
    def __init__(self,name,label):
        self.name = name
        self.label = label

    def draw(self):
        print '<button class="mdl-button mdl-js-button mdl-button--raised">'
        print self.label
        print '</button>'


# Create instance of FieldStorage 
form = cgi.FieldStorage()

boxes = [CheckBox('box0','Check Box 0'),CheckBox('box1','Check Box 1',True),CheckBox('box2','Check Box 2')]
radios = [RadioBox('rad1',['Option 1','Option 2','Option 3'],1)]
texts = [Text('text1','Text box 1'),Text('text2','Text box 2','1234.643')]
buttons = [Button('button1','SUBMIT')]

print "Content-type:text/html\r\n\r\n"

print "<html>"

print "<head>"
print "<title>Physics test page</title>"
print '''<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
<link rel="stylesheet" href="https://code.getmdl.io/1.3.0/material.indigo-pink.min.css">
<script defer src="https://code.getmdl.io/1.3.0/material.min.js"></script>
<link rel="stylesheet" href="../index.css">'''
print "</head>"

print "<body>"

if len(form) != 0:
    for box in boxes:
        box.checkState(form)
    for rad in radios:
        rad.checkState(form)
    for t in texts:
        t.checkState(form)

    configDump = {}
    configDump['checkboxes'] = [box.getState() for box in boxes]
    configDump['radiobuttons'] = [rad.getState() for rad in radios]
    configDump['textinputs'] = [t.getState() for t in texts]

    configFile = open('image.cfg','w+')
    pickle.dump(configDump,configFile)
    configFile.close()

    #Check for the presence of the lockfile
    #if glob.glob("lockfile") == []:
    os.system("./script.py&")
    print "<img id='graph' src='../loading.png'/>"
    print '''<script>function UrlExists(url)
    {
    var http = new XMLHttpRequest();
    http.open('HEAD', url, false);
    http.send();
    return http.status!=404;
    }
    var loop = setInterval(function(){ if (UrlExists('../graph.png')) {clearInterval(loop);document.getElementById('graph').src = '../graph.png';}; }, 1000);</script>'''
else:
    print "<img id='graph' src='../first.png'/>"

print '<form action="index.cgi" method="get">'
print '<table border=1px><tr>'
print '<td>'
for box in boxes:
    box.draw()
print '</td><td>'
for rad in radios:
    rad.draw()
print '</td><td>'
for t in texts:
    t.draw()
print '</td></tr></table>'
for b in buttons:
    b.draw()

print '</form>'

print "</body>"
print "</html>"
