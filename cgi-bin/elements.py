import mdl,os

VERSION = '17' #Version of the Mathematica script
RROOT = '../' #Relative root, use for links
AROOT = '/home/sean/Programs/git-repos/physics-research/' #'/htdocs/seand/' #Absolute root, use for file accessing
AMATH = AROOT+'mathscript_v%s/bin/'%VERSION #Path to Mathematica script bin
RMATH = RROOT+'mathscript_v%s/bin/'%VERSION #Relative path to Mathematica script bin
ASSETS = '../assets/' #Path to HTML page assets
AOUTPUT = AROOT+'mathscript_v%s/plots/Jobs/'%VERSION #Path to output directory - the script will create a separate  folder here for each unique job ID
ROUTPUT = RROOT+'mathscript_v%s/plots/Jobs/'%VERSION

EXPIDS = 'exptidname_inconfig.txt'
CONFIG = 'config1.txt' #Name of config file to be generated in the Mathematica script bin directory
IMAGE = '/exptname_table.png'

#Generate list of experiment IDs and their associated string names (currently 111 elements long)
idFile = file(AMATH+EXPIDS,'r')
expids = [i.split() for i in idFile.readlines()]
expids = [i for i in expids if len(i) != 0]
idFile.close()


#Experiments to include
exp_boxes = [
    mdl.CheckBox('allexps','All',False,'expid','checkAllExps();') #'Select all' box
 ]+[mdl.CheckBox(i[0],i[1],False,'expid') for i in expids]

#Figures to plot
fig_boxes = [
    mdl.CheckBox('type1','Experimental data points',False), #x-mu plot
    mdl.RadioBox('figtype',[
        'Experimental errors',
        'Residuals',
        'PDF errors on residuals',
        'Sensitivity factor'
        ,'Correlation'
    ],0,[
        'changeState(["highlight"],[0],[5])',
        'changeState(["highlight"],[1],[5])',
        'changeState(["highlight"],[2],[5])',
        'changeState(["highlight"],[3],[5])',
        'changeState(["highlight"],[4],[5])'
    ])
]

#Functions to use
func_boxes = [
    mdl.RadioBox('function',[
        'b<span class="bar">&#x203e;</span>',
        'c<span class="bar">&#x203e;</span>',
        's<span class="bar">&#x203e;</span>',
        'd<span class="bar">&#x203e;</span>',
        'u<span class="bar">&#x203e;</span>',
        'g',
        'u',
        'd',
        's',
        'c',
        'b',
        'q6',
        'q7',
        'q8'
    ]),
    mdl.CheckBox('func15','user',False,None,"toggleBlock2('user')") #User created option
]

#Figure range 'auto' boxes
auto_boxes = [
    mdl.CheckBox('xauto','Auto',True,None,"toggleState(['auto'],[0],[4])"),
    mdl.CheckBox('muauto','Auto',True,None,"toggleState(['auto'],[1],[4])"),
    mdl.CheckBox('hxauto','Auto',True,None,"toggleState(['auto'],[2],[4])"),
    mdl.CheckBox('yauto','Auto',True,None,"toggleState(['auto'],[3],[4])")
]

#PDFset selection - only one option right now
pdfset = mdl.RadioBox('pdfset',['CT14NNLO'])

#Size selection for points when nothing is highlighted
sizes = [
    mdl.RadioBox('pointsize%d'%i,[
        'Tiny',
        'Small',
        'Medium',
        'Large'
    ])
    for i in range(1,6)
]

#Value and percentage ranges for highlight mode functions 2-6 [0:20]
vmins = [mdl.Text('vmin%d'%i,'Min',1,0.0) for i in range(2,7)]
vmaxs = [mdl.Text('vmax%d'%i,'Max',1,0.0) for i in range(2,7)]
pmins = [mdl.Text('pmin%d'%i,'Min',1,0.0) for i in range(2,7)]
pmaxs = [mdl.Text('pmax%d'%i,'Max',1,0.0) for i in range(2,7)]

#User function parameter
user_func = [
    mdl.Text('userparamname','Name',0),
    mdl.Text('57values','Enter 57 values',0)
]

#Window bounds for graph
bounds = [
    mdl.Text('xmin','X-min',1,0.00001),
    mdl.Text('xmax','X-max',1,1),
    mdl.Text('mumin','&#x03bc;-min',1,1.0),
    mdl.Text('mumax','&#x03bc;-max',1,2000),
    mdl.Text('nbin','Nbin',1,20),
    mdl.Text('hxmin','X-min',1,-3),
    mdl.Text('hxmax','X-max',1,3),
    mdl.Text('ymin','Y-min',1,0),
    mdl.Text('ymax','Y-max',1,10)
]

#Highlight options
selects = [
    #Dropdowns for highlight mode (figures 2-6)
    #mdl.Select('wtype1',['No highlighting','Value range','Percentage range']),
    mdl.RadioBox('wtype%d'%i,[
        'No highlighting',
        'Value range',
        'Percentage range'
    ],0,[
        'changeState(["mode"],[0],[3])',
        'changeState(["mode"],[1],[3])',
        'changeState(["mode"],[2],[3])'
    ]) for i in range(5)
    #mdl.Select('wtype3',['No highlighting','Value range','Percentage range']),
    #mdl.Select('wtype4',['No highlighting','Value range','Percentage range']),
    #mdl.Select('wtype5',['No highlighting','Value range','Percentage range']),
    #mdl.Select('wtype6',['No highlighting','Value range','Percentage range'])
]

#Buttons
buttons = [
    mdl.Button('button1','SUBMIT'), #[0]
    mdl.Button('button2','RESET','resetbutton') #[1]
]

all_elements = exp_boxes+fig_boxes+func_boxes+auto_boxes+[pdfset]+sizes+vmins+vmaxs+pmins+pmaxs+user_func+bounds+selects
