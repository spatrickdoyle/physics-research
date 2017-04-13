#Object wrappers for Google Material Design components, to be used in conjunction with the Python CGI interface
#Written by Sean Doyle in 2017 for the Southern Methodist University theoretical physics research lab


class CheckBox:
    '''Wrapper class for displaying and storing the values of checkboxes, styled with the Google MDL toolkit'''

    def __init__(self,name,label,default=False,clas=None,onUse=None):
        '''CheckBox(string name, string label, bool default=False, string clas=None, string onUse=None)
        name: name of this element, used for internal reference
        label: label which will be displayed next to the box on the HTML page
        default: the default value of this element, before it has been used
        clas: the HTML class name to give the element
        onUse: javascript function to call upon clicking the box
        Initializes class properties according to arguments'''

        self.name = name
        self.label = label
        self.default = default
        if default:
            self.state = " checked"
        else:
            self.state = ""
        if clas != None:
            self.clas = " class='"+clas+"'"
        else:
            self.clas = ""
        if onUse != None:
            self.onUse = ' onchange="'+onUse+'"'
        else:
            self.onUse = ""

    def draw(self,tabs):
        '''draw(int tabs)
        tabs: number of indentations to make before each line
        Prints HTML for displaying the checkbox'''

        print tabs*'    ' + '<label style="display:inline" class="mdl-checkbox mdl-js-checkbox" for="'+self.name+'">'
        print tabs*'    ' + '<input type="checkbox" id="'+self.name+'" name="'+self.name+'" class="mdl-checkbox__input"'+self.clas+self.state+self.onUse+'>'
        print tabs*'    ' + '<span class="mdl-checkbox__label">'+self.label+'</span>'
        print tabs*'    ' + '</label>'

    def checkState(self,form):
        '''checkState(Form form)
        form: the Python CGI form object the checkbox is being used within
        Checks the state of the element and updates it'''

        if form.getvalue(self.name) == 'on':
            self.changeState(True)
        else:
            self.changeState(False)

    def changeState(self,state):
        '''changeState(bool state)
        state: value to update box to
        Update the class property storing the value of the box'''

        if state:
            self.state = " checked"
        else:
            self.state = "";

    def getState(self):
        '''getState()
        return: boolean value representing whether the box is checked
        Checks and returns the value of the checkbox'''

        if self.state == "":
            return False
        else:
            return True


class RadioBox:
    '''Wrapper class for displaying and storing the values of sets of radio buttons, styled with the Google MDL toolkit'''

    def __init__(self,name,labels,default=0,onuse=None):
        '''RadioBox(string name, List label, int default=0)
        name: name of this element, used for internal reference
        label: labels which will be displayed next to the boxes on the HTML page
        default: the default value of this element, before it has been used
        onuse: list of strings, javascript calls to make, one for each radio button
        Initializes class properties according to arguments'''

        self.name = name
        self.labels = labels
        self.default = default
        self.states = ["" for i in labels]
        self.states[default] = " checked"
        if onuse:
            self.onuse = [" onclick='%s'"%i for i in onuse]
        else:
            self.onuse = ["" for i in labels]

    def draw(self,tabs):
        '''draw(int tabs)
        tabs: number of indentations to make before each line
        Prints HTML for displaying the radio buttons'''

        for i in range(len(self.labels)):
            print tabs*'    ' + '<label class="mdl-radio mdl-js-radio" for="'+self.name+'-'+str(i)+'">'
            print tabs*'    ' + '<input type="radio" id="'+self.name+'-'+str(i)+'" class="mdl-radio__button" name="'+self.name+'" value="'+str(i)+'"'+self.states[i]+self.onuse[i]+'>'
            print tabs*'    ' + '<span class="mdl-radio__label">'+self.labels[i]+'</span>'
            print tabs*'    ' + '</label><br/>'

    def checkState(self,form):
        '''checkState(Form form)
        form: the Python CGI form object the radios are being used within
        Checks the state of the element and updates it'''

        self.changeState(int(form.getvalue(self.name)))

    def changeState(self,state):
        '''changeState(int state)
        state: value to update radios to
        Update the class property storing the value of the checked box'''

        self.states = ["" for i in range(len(self.labels))]
        self.states[state] = " checked"

    def getState(self):
        '''getState()
        return: integer representing which radio box is checked
        Checks and returns the value of the checked radio box'''

        return self.states.index(" checked")


class Text:
    '''Wrapper class for displaying and storing the values of text fields, styled with the Google MDL toolkit'''

    def __init__(self,name,label,numbers,default=None):
        '''Text(string name, string label, bool numbers, string default)
        name: name of this element, used for internal reference
        label: label which will be displayed next to the field on the HTML page
        numbers: whether this field should only accept numbers
        default: the default value of this element, before it has been used
        Initializes class properties according to arguments'''

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
                self.state = ' value="%.5f"'%default
            else:
                self.state = ' value="'+default+'"'

    def draw(self,tabs):
        '''draw(int tabs)
        tabs: number of indentations to make before each line
        Prints HTML for displaying the text field'''

        """print tabs*'    ' + '<div class="mdl-textfield mdl-js-textfield mdl-textfield--floating-label">'
        print tabs*'    ' + '<input class="mdl-textfield__input" type="text" '+self.numbers+'name="'+self.name+'" id="'+self.name+'"'+self.state+'>'
        print tabs*'    ' + '<label class="mdl-textfield__label" for="'+self.name+'">'+self.label+'</label>'
        print tabs*'    ' + '<span class="mdl-textfield__error">Enter a number</span>'
        print tabs*'    ' + '</div>'"""

        print tabs*'    ' + self.label + ': <input type="text" name="'+self.name+'" id="'+self.name+'"'+self.state+'>'

    def checkState(self,form):
        '''checkState(Form form)
        form: the Python CGI form object the text field is being used within
        Checks the state of the element and updates it'''

        self.changeState(form.getvalue(self.name))

    def changeState(self,state):
        '''changeState(string state)
        state: string to update the contents of the field to
        Update the class property storing the string in the field'''

        if state == None:
            self.state = ""
        else:
            self.state = ' value="'+state+'"'

    def getState(self):
        '''getState()
        return: the string in th text field
        Checks and returns the value of the field'''

        return self.state[8:-1]


class Button:
    '''Wrapper class for displaying and storing the values of buttons, styled with the Google MDL toolkit'''

    def __init__(self,name,label,idd=None):
        '''Button(string name, string label, string idd)
        name: name of this element, used for internal reference
        label: label which will be displayed next to the field on the HTML page
        idd: HTML id to give the element
        Initializes class properties according to arguments'''

        self.name = name
        self.label = label
        if idd == None:
            self.idd = ""
        else:
            self.idd = idd

    def draw(self,tabs):
        '''draw(int tabs)
        tabs: number of indentations to make before each line
        Prints HTML for displaying the button'''

        print tabs*'    ' + '<button class="mdl-button mdl-js-button mdl-button--raised"'+' id="%s">%s</button>'%(self.idd,self.label)

class Select:
    '''Wrapper class for a styled select element'''

    def __init__(self,name,labels,onchange="",idd=""):
        '''Select(string name, List label, int default=0)
        name: name of this element, used for internal reference
        label: labels which will be displayed in the menu on the HTML page
        Initializes class properties according to arguments'''

        self.name = name
        self.labels = labels
        self.state = 0
        self.onchange = onchange
        self.idd = idd

    def draw(self,tabs):
        '''draw(int tabs)
        tabs: number of indentations to make before each line
        Prints HTML for displaying the select element'''

        print tabs*'    ' + '<select id="%s" name="%s" onchange="%s">'%(self.idd,self.name,self.onchange)
        for i in range(len(self.labels)):
            if i != self.state:
                print tabs*'    ' + '<option value="%s">%s</option>'%(self.labels[i],self.labels[i])
            else:
                print tabs*'    ' + '<option value="%s" selected>%s</option>'%(self.labels[i],self.labels[i])

    def checkState(self,form):
        '''checkState(Form form)
        form: the Python CGI form object the select menu is being used within
        Checks the state of the element and updates it'''

        self.changeState(int(self.labels.index(form.getvalue(self.name))))

    def changeState(self,state):
        '''changeState(int state)
        state: value to update selection to
        Update the class property storing the value of the selected item'''

        self.state = state

    def getState(self):
        '''getState()
        return: integer representing which option is selected
        Checks and returns the value of the checked radio box'''

        return self.state
