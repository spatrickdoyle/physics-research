class CheckBox:
    def __init__(self,name,label,default=False,onUse=None):
        self.name = name
        self.label = label
        self.default = default
        if default:
            self.state = " checked"
        else:
            self.state = ""
        if onUse != None:
            self.onUse = ' onchange="'+onUse+'"'
        else:
            self.onUse = ""

    def draw(self):
        print '<label style="display:inline" class="mdl-checkbox mdl-js-checkbox" for="'+self.name+'">'
        print '<input type="checkbox" id="'+self.name+'" name="'+self.name+'" class="mdl-checkbox__input"'+self.state+self.onUse+'>'
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
        self.default = default
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
    def __init__(self,name,label,numbers,default=None):
        self.name = name
        self.label = label
        if numbers:
            self.numbers = 'pattern="-?[0-9]*(\.[0-9]+)?" '
        else:
            self.numbers = ''
        self.default = default
        if default == None:
            self.state = ""
        else:
            if numbers:
                self.state = ' value="%f"'%default
            else:
                self.state = ' value="'+default+'"'

    def draw(self):
        print '<div class="mdl-textfield mdl-js-textfield mdl-textfield--floating-label">'
        print '<input class="mdl-textfield__input" type="text" '+self.numbers+'name="'+self.name+'" id="'+self.name+'"'+self.state+'>'
        print '<label class="mdl-textfield__label" for="'+self.name+'">'+self.label+'</label>'
        print '<span class="mdl-textfield__error">Enter a number</span>'
        print '</div>'

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
    def __init__(self,name,label,idd=None):
        self.name = name
        self.label = label
        if idd == None:
            self.idd = ""
        else:
            self.idd = idd

    def draw(self):
        print '<button class="mdl-button mdl-js-button mdl-button--raised"'+' id="'+self.idd+'">'
        print self.label
        print '</button>'
