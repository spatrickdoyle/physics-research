(* ::Package:: *)

(* ::Text:: *)
(*Investigation of correlated systematic errors in jet production*)
(*Pavel Nadolsky, December 2008*)


(* ::Section:: *)
(*Common setup*)


BeginPackage["dtaread2016boting`"]; 

ReadLisFile::usage="";
ExptIDtoName::usage="";
ExptIDEcm::usage="";
ExptIDprocess::usage="";
ReadDta::usage="";
ReadExptTable::usage="";
EZIDecomposition::usage="";
selectdatacolume::usage="";
ExptIDinfo::usage="";
selectExptxQ::usage="";
selectExptxQv2::usage="";
selectExptresidue::usage="";
obsxQ::usage="";
obsxQresidue::usage="";
selectExptobservable::usage="";
selectExptxQall::usage="";
extractdata2::usage="";
extractobsfromdata::usage="";
makexQdataset::usage="";
makeobsdataset::usage="";
PDFloglogplot::usage="";
colorset::usage="";


Begin["`Private`"]; 


(* ::Text:: *)
(*Mathematica version-specific set up*)


(* ::Input::Initialization:: *)
Off[General::spell]
Off[General::spell1]


(* ::Text:: *)
(*Set Interpolation Order*)
(**)


(* ::Input::Initialization:: *)
SetOptions[Interpolation,InterpolationOrder->2]


(* ::Input::Initialization:: *)
{InterpolationOrder->2,Method->Automatic,PeriodicInterpolation->False}


(* ::Input::Initialization:: *)
{InterpolationOrder->2,Method->Automatic,PeriodicInterpolation->False}


(* ::Input::Initialization:: *)
SetOptions[{Plot,ListPlot,Graphics,ParametricPlot},BaseStyle->{FontFamily->"Helvetica",16}];


(* ::Subsection:: *)
(*Subroutines to generate the table of the experiment names from the "...lis" file*)


(* ::Text:: *)
(*Given the "...lis" file lisFileName (e.g., M01lis), generate a table of correspondence between the three-digit experiment ID and its name (lisTable)*)


(* ::Input::Initialization:: *)
(*2016.11.20 test error*)
lisTable={};

(* old version
ReadLisFile=Function[lisFileName,
Module[{lisFile,lisTable},
lisFile=OpenRead[lisFileName];
If[lisFile==$Failed,Abort[]];

(* Dump the first two lines *)
Read[lisFile,{Record,Record,Record}];

lisTable=ReadList[lisFile,{Number,Number,Word,Record}];
Close[lisFile];

(* Generate the correspondence table *)
lisTable=Select[lisTable,Round[#[[1]]] !=0&];
Table[{lisTable[[i]][[1]]*100+lisTable[[i]][[2]],lisTable[[i]][[3]]},{i,Dimensions[lisTable][[1]]}]
](*Module\[Rule]*)
](*Function\[Rule]*)
*)

(*2016.11.20 test error*)
(*new version: listable is global, so it could be used by ExptIDtoName*)
ReadLisFile=Function[lisFileName,
Module[{lisFile,lisTabletmp},
lisFile=OpenRead[lisFileName];
If[lisFile==$Failed,Abort[]];

(* Dump the first two lines *)
Read[lisFile,{Record,Record,Record}];

lisTabletmp=ReadList[lisFile,{Number,Number,Word,Record}];
Close[lisFile];

(* Generate the correspondence table *)
lisTabletmp=Select[lisTabletmp,Round[#[[1]]] !=0&];

lisTable=
Table[{lisTabletmp[[i]][[1]]*100+lisTabletmp[[i]][[2]],lisTabletmp[[i]][[3]]},{i,Dimensions[lisTabletmp][[1]]}];
lisTable
](*Module\[Rule]*)
](*Function\[Rule]*)


(* ::Text:: *)
(*Given the three-digit experiment ID, the function ExptIDtoName  returns its true name*)


(* ::Input::Initialization:: *)
(*2016.11.20 test error*)
(*
ExptIDtoName=
Function[ExptID,IDPosition=Position[lisTable,ExptID][[1]][[1]];lisTable[[IDPosition]][[2]]];
*)
(*20170214: seems make problem in parallel mode, modify it: variables become local*)
ExptIDtoName[ExptIDin_]:=
Module[{ExptID=ExptIDin,IDPosition,lisTabletmp},
lisTabletmp=lisTable;
IDPosition=Position[lisTabletmp,ExptID][[1]][[1]];
lisTabletmp[[IDPosition]][[2]]

]


(*
ExptIDtoName[ExptIDin_]:=
Module[{ExptID=ExptIDin},
(*
IDPosition=Position[lisTable,ExptID][[1]][[1]];
lisTable[[IDPosition]][[2]]
*)
lisTable[[3]]
]
*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
(*2016.11.04 botingw*)
(*table of center of energy (Ecm, sqrt(S)) of experiments *)
(*collect expid with its Ecm (sqrt(S)) value, 201, 203, 204 have two version of Ecm*)
(*input: ExptID, return Ecm of corresponding ExptID *)
ExptIDEcm[ExptIDin_]:=
Module[{ExptID=ExptIDin,output,expidsqrtS,Ecm,IDPosition},
(*
the table of Ecm of exptid looks like this: 
{{exptid1, Ecm1},{exptid2, Ecm2},...}
*)
expidsqrtS=
{
{201,38.8},
{203,38.76},
{204,38.75},
{225,1800},
{227,1960},
{234,1960},
{240,7000},
{241,7000},
{260,1960},
{261,1960},
{266,7000},
{267,7000},
{268,7000},
{281,1960},
{504,1960},
{514,1960},
{535,7000},
{538,7000}
};

(*find position of exptID*)
Ecm=-1;
IDPosition=Position[expidsqrtS,ExptID];
(*
if find one match exptid on the list, return Ecm(sqrt(S)) of that exptid,
if #match of input exptID to expidS is not 1, return error massage
*)
If[
Length[IDPosition]==1,
IDPosition=IDPosition[[1]][[1]];
Ecm=expidsqrtS[[IDPosition]][[2]]
,
Print["error (ExptIDEcm), there is no ID match or more than 1 ID match to energy table, ID = ",ExptID]
];
(*
IDPosition=Position[expidsqrtS,ExptID][[1]][[1]];
Ecm=expidsqrtS[[IDPosition]][[2]];
*)
Ecm

]


ExptIDprocess[ExptIDin_]:=
Module[{ExptID=ExptIDin,output,expidsqrtS,Ecm,IDPosition},
(*
the table of Ecm of exptid looks like this: 
{{exptid1, {exp name1, process1, observable1, Ecm1}},{exptid2, {exp name2, process2, observable2, Ecm2}},...}
*)
expidsqrtS=
{
{101, {"BCDMS", "p\[Mu] \[Rule] \[Mu]X", "",""}},(*mu+ or mu-*)
{102, {"BCDMS", "d\[Mu] \[Rule] \[Mu]X", "",""}},(*mu+ or mu-*)
{104, {"NMC-NA37", "p(d)\[Mu] \[Rule] \[Mu]X", "",""}},(*mu+ or mu-*)
{106, {"NMC-NA37", "p\[Mu] \[Rule] \[Mu]X", "",""}},(*mu+ or mu-*)
{108, {"CDHSW", "\[Nu](\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\))Fe \[Rule] \[Mu]X", "F2",""}},
{109, {"CDHSW", "\[Nu](\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\))Fe \[Rule] \[Mu]X", "xF3",""}},
{110, {"CCFR", "\[Nu](\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\))Fe \[Rule] \[Mu]X", "F2",""}},
{111, {"CCFR", "\[Nu](\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\))Fe \[Rule] \[Mu]X", "xF3",""}},
{124, {"NuTeV", "\[Nu]N \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)X", "",""}},
{125, {"NuTeV", "\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\)N \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)X", "",""}},
{126, {"CCFR", "\[Nu](\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\))Fe \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)X", "F2",""}},
{127, {"CCFR", "\[Nu](\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\))Fe \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)X", "xF3",""}},
{145, {"H1", "\[ScriptE]\[ScriptP] -> \[ScriptE]\[ScriptB]\!\(\*OverscriptBox[\(\[ScriptB]\), \(_\)]\)X", "reduced Xsec(\[ScriptB]\!\(\*OverscriptBox[\(\[ScriptB]\), \(_\)]\))",""}},(*e+ or e-*)
{147, {"H1&ZEUS", "\[ScriptE]\[ScriptP] -> \[ScriptE]\[ScriptC]\!\(\*OverscriptBox[\(\[ScriptC]\), \(_\)]\)X", "reduced Xsec(\[ScriptC]\!\(\*OverscriptBox[\(\[ScriptC]\), \(_\)]\))",""}},(*e+ or e-*)
{159, {"H1&ZEUS", "\!\(\*SuperscriptBox[\(\[ScriptE]\), \(+\)]\)(\!\(\*SuperscriptBox[\(\[ScriptE]\), \(-\)]\))\[ScriptP] -> X", "",""}},
{160, {"H1&ZEUS", "\!\(\*SuperscriptBox[\(\[ScriptE]\), \(+\)]\)(\!\(\*SuperscriptBox[\(\[ScriptE]\), \(-\)]\))\[ScriptP] -> \!\(\*SuperscriptBox[\(\[ScriptE]\), \(+\)]\)(\!\(\*SuperscriptBox[\(\[ScriptE]\), \(-\)]\))X or \[Nu](\!\(\*OverscriptBox[\(\[Nu]\), \(_\)]\))X", "",""}},
{169, {"H1", "\!\(\*SuperscriptBox[\(\[ScriptE]\), \(+\)]\)(\!\(\*SuperscriptBox[\(\[ScriptE]\), \(-\)]\))\[ScriptP] -> X (NC)", "",""}},
{201,{"E605", "\[ScriptP]\[ScriptCapitalC]\[ScriptU] \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)X", "", 38.8}},
{203, {"FNAL E866/NuSea", "\[ScriptP]\[ScriptP](d) \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)X", "",38.76}},
{204, {"E866/NuSea", "\[ScriptP]\[ScriptP] \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(-\)]\)X", "", 38.75}},
{225, {"CDF run 1", "\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] ??", "", 1800}},(*charge asym*)
{227, {"CDF run 2", "\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] WX, W \[Rule] \[ScriptE]\[Nu]", "", 1960}},(*charge asym*)
{234, {"D0 Run IIa & Run IIb", "", "", 1960}},(*CP folded asym*)
{240, {"LHCb", "\[ScriptP]\[ScriptP] \[Rule] ZX, Z \[Rule] \!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(\(-\)\(\\\ \)\)]\) or \[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] WX, W \[Rule] \[Mu]\[Nu]", "", 7000}},(*forward VB production*)
{241, {"LHCb", "\[ScriptP]\[ScriptP] \[Rule] WX, W \[Rule] \[Mu]\[Nu]", "", 7000}},(*W+ or W-*)
{260, {"D0 Run 2", "\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] Z", "", 1960}},(*Z decay ??*)
{261, {"CDF Run 2", "\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] Z", "", 1960}},
{266, {"CMS", "\[ScriptP]\[ScriptP] \[Rule] WX, W \[Rule] \[Mu]\[Nu]", "", 7000}},
{267, {"CMS", "\[ScriptP]\[ScriptP] \[Rule] WX, W \[Rule] \[ScriptE]\[Nu]", "", 7000}},
{268, {"ATLAS", "\[ScriptP]\[ScriptP] \[Rule] \[ScriptL]\[Nu]X/\!\(\*SuperscriptBox[\(\[Mu]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[Mu]\), \(\(-\)\(\\\ \)\)]\)X", "", 7000}},
{281, {"D0 Run 2", "\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] WX, W \[Rule] \[ScriptE]\[Nu]", "", 1960}},
{504, {"CDF run 2", "\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] \[ScriptJ]X", "", 1960}},
{514, {"D0 Run 2", "\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] \[ScriptJ]X", "", 1960}},
{535, {"ATLAS", "\[ScriptP]\[ScriptP] \[Rule] \[ScriptJ]X", "", 7000}},
{538, {"CMS", "\[ScriptP]\[ScriptP] \[Rule] \[ScriptJ]X", "", 7000}}
};



(*find position of exptID*)
Ecm=-1;
IDPosition=Position[expidsqrtS,ExptID];
(*
if find one match exptid on the list, return Ecm(sqrt(S)) of that exptid,
if #match of input exptID to expidS is not 1, return error massage
*)
If[
Length[IDPosition]==1,
IDPosition=IDPosition[[1]][[1]];
Ecm=expidsqrtS[[IDPosition]]
,
Print["error (ExptIDprocess), there is no ID match or more than 1 ID match to process table, ID = ",ExptID]
];
(*
IDPosition=Position[expidsqrtS,ExptID][[1]][[1]];
Ecm=expidsqrtS[[IDPosition]][[2]];
*)
Ecm

]




(* ::Subsection:: *)
(*Subroutines to read in the .dta files*)


(* ::Text:: *)
(*ReadDta splits the .dta file into the list ExperimentRecords of the records for each individual experiment*)


(* ::Input::Initialization:: *)
ReadDta=Function[DtaFileName,
Module[{DtaFile,ExperimentRecords},
DtaFile=OpenRead[DtaFileName];
If[DtaFile==$Failed,Abort[]];
(* The first line is the name of the file; dump it *)
Read[DtaFile,Record,RecordSeparators->"DATA SET"];
(*
ExperimentRecords=ReadList[DtaFile,Record,RecordSeparators\[Rule]"DATA SET:"];
*)
(*2016.11.01 botingw*)
ExperimentRecords=ReadList[DtaFile,Record,RecordSeparators->"DATA SET"];

Print[Dimensions[ExperimentRecords][[1]]," experiment record(s) read from ",DtaFileName];
Close[DtaFile];
ExperimentRecords
]
];


(* ::Text:: *)
(*Read all information from the .dta file into the table of the following format:*)
(*{experiment ID, experiment name, normalization factor, number of points, description of data columns, the data}*)


(* ::Input::Initialization:: *)
(*PrintExpt=True;*)PrintExpt=False;


(* ::Input::Initialization:: *)
ReadExptTable=Function[{DtaFileName,FileFormat},
Module[
{NExperiments,MyStream,ExptID,R2String,NormFac,NPoints,DataDescription,DataString,NDataColumns,DataList,tmpTable,chi2,S},(*20161103 botingw: S& chi2 become local variable*)

(* Read the experiment strings from the .dta file *)
ExperimentRecords=ReadDta[DtaFileName];

NExperiments=Dimensions[ExperimentRecords][[1]];

(* Process the input string for each experiment and write into a temporary table*)
If[PrintExpt,Print["#  ExptID ExptName  NormFac  #points  #data entries"]];

tmpTable=Table[
{
Experiment=ExperimentRecords[[iExperiment]];

MyStream=StringToStream[Experiment];

Switch[FileFormat,
(*2016.11.01 botingw*)
"ct2016",
{ExptID,dummy,dummy,dummy,dummy,NormFac,dummy,dummy,dummy,dummy,dummy,NPoints,dummy,dummy,dummy,chi2,dummy,S}=Read[MyStream,{Number,Word,Word,Word,Word,Number,Word,Word,Word,Word,Word,Number,Word,Word,Word,Number,Word,Number}],
"ct66",
{ExptID,dummy,dummy,dummy,dummy,NormFac,dummy,dummy,dummy,dummy,dummy,NPoints,dummy,dummy,dummy,chi2}=Read[MyStream,{Number,Word,Word,Word,Word,Number,Word,Word,Word,Word,Word,Number,Word,Word,Word,Number}],
"ct60",
{ExptID,dummy,dummy,dummy,dummy,NormFac,dummy,dummy,dummy,dummy,dummy,NPoints}=Read[MyStream,{Number,Word,Word,Word,Word,Number,Word,Word,Word,Word,Word,Number}],
_,
Print["Wrong FileFormat value=",FileFormat]
];

(* Read one or two line of the data description *)
DataDescription=Read[MyStream,Record];
(*Read correlated systematic error parameters *)
R2String="";If[StringMatchQ[DataDescription,"*r(k)*"],
R2String=Read[MyStream,Record]];(* Correlated systematic errors \[Rule] *)

(* Read in the data string and get rid of the intermediary comments specifying the value of x, yj, etc.*)
DataString=Select[ReadList[MyStream,Record],
!StringMatchQ[#,"* X *"] && !StringMatchQ[#,"*Tau*"]&& !StringMatchQ[#,"*Y*"]&];

(* Determine the number of data columns by sampling the first element of DataString *)
NDataColumns=Dimensions[ReadList[StringToStream[DataString[[1]]],Number]][[1]];

(*Append a blank character to the end of each line in DataString to separate the first number in each line from the last number in the previous line in the concatenated string *)
DataString=(StringJoin[#," "]&/@DataString);

(* Finally, read the data list *)
DataList=ReadList[StringToStream[StringJoin[DataString]],Table[Number,{NDataColumns}]/.List->LF];

If[PrintExpt,Print[iExperiment,"  ",ExptID,"    ",ExptIDtoName[ExptID],"    ",NormFac,"     ",NPoints,"     ",Dimensions[DataList]]];
Close[MyStream];
ExptID,ExptIDtoName[ExptID],R2String,NormFac,NPoints,DataDescription,DataList,S
},
{iExperiment,NExperiments}];(* tmpTable \[Rule] *)
tmpTable
](* Module \[Rule]*)
];(* Function ->*)


(* ::Input::Initialization:: *)
EZIDecomposition=Function[{InputMatrix,rank,eps1,PrintMode},
Module[{\[CapitalLambda],U,Npt,\[Rho],\[Rho]s,\[CapitalSigma],\[Rho]hat,as,bs},
Npt=Length[InputMatrix];
\[CapitalSigma]=Table[InputMatrix[[i,i]]-1,{i,Npt}];
\[Rho]s=Table[If[i==j,1,InputMatrix[[i,j]]],{i,Npt},{j,Npt}];
as=eps1*1000;
While[as>eps1,
{\[CapitalLambda],U}=Eigensystem[\[Rho]s];
\[Rho]=Transpose[U].DiagonalMatrix[Table[If[i<=rank,\[CapitalLambda][[i]],0],{i,Npt}]].U;
as=Sum[(\[Rho]s[[i,j]]-\[Rho][[i,j]])^2,{i,Npt},{j,Npt}];
s=DiagonalMatrix[Table[Sqrt[\[Rho][[i,i]]],{i,Npt}]];
\[Rho]hat=Inverse[s].\[Rho].Inverse[s]; 
\[Rho]s=Table[If[i==j,1,\[Rho][[i,j]]],{i,Npt},{j,Npt}];
bs=Sum[(InputMatrix[[i,j]]-\[Rho]hat[[i,j]])^2,{i,Npt},{j,Npt}];
If[PrintMode==1,Print["as, bs = ",as,"   ",bs]];
](*While\[Rule]*);{bs,\[Rho]hat}
](*Module\[Rule]*)
](*Function\[Rule]*)


(* ::Input::Initialization:: *)
(*2016.11.01 botingw*)
(*read specific member of a colume*)
selectdatacolume[datain_,collistin_]:=
Module[{data=datain,collist=collistin,datatmp,output,i},
(*
data=data/.List\[RuleDelayed]LF;
data=data/.LF[x__]\[RuleDelayed]{x}[[Xindex]];
data
*)
(*
output=Table[
datatmp=data[[i]]/.List\[RuleDelayed]LF,
{i,1,Length[data]}
];
*)
output=data/.LF[a__]:>Table[{a}[[collist[[i]] ]],{i,1,Length[collist]}]/.List:>LF;

output=Table[
datatmp=output[[i]]/.List:>LF,
{i,1,Length[output]}
];

output 
];

ExptIDinfo[ExptIDin_]:=
Module[{ExptID=ExptIDin,output,expItype,VBPtype1,VBPtype2,VBPtype3},
(*
1:DIS;
2:VBP (VectorBosonProd.;real/virtual photon;W,Z,...)
3:Direct Photon
4:Heavy Quark production
5:Jet production
*)

(*
VBPtype
format:
1: Q, tau
2: Q, y, sqrt(S)
3: Asym(yl)
*)

(*seems not totally right, need to check later*)
VBPtype1={};
VBPtype2={201,203,204,260,261,268,240};
VBPtype3={225,227,234,267,281,241,266};

expItype=
Which[
ExptID>99 && ExptID<200,
"DIS",
 ExptID>199 && ExptID<300 && SubsetQ[VBPtype1,{ExptID}],
"VBP1",
 ExptID>199 && ExptID<300 && SubsetQ[VBPtype2,{ExptID}],(*Z \[Rule] l+l-, dXsec/dy(l)*)
"VBP2",
 ExptID>199 && ExptID<300 && SubsetQ[VBPtype3,{ExptID}],(*W \[Rule] lv, asym*)
"VBP3",
 ExptID>199 && ExptID<300,
"VBPothers",
 ExptID>299 && ExptID<400,
"DP",
 ExptID>399 && ExptID<500,
"HQP",
 ExptID>499 && ExptID<600,
"JP",
 ExptID>599 && ExptID<700,
"undefine",
 ExptID>699 && ExptID<800,
"undefine",
_,
Print["Wrong ExptID value=",ExptID];
];

(*
type1a\[Equal] lp scattering, DIS, NC
type1b\[Equal] lp scattering, DIS, CC
type2a\[Equal] lp \[Rule] cX, DIS, NC
type2b\[Equal] lp \[Rule] bX, DIS, NC
type3\[Equal] lN \[Rule] X, DIS
type4a\[Equal] nuN \[Rule] X, DIS
type4b\[Equal] nuN \[Rule] cX, DIS
type5\[Equal] pN \[Rule] l+l-X, VBP
type6\[Equal] ppbar \[Rule] l+l-X, VBP
type7\[Equal] ppbar \[Rule] l nu X, VBP
type8\[Equal] ppbar \[Rule] jet X, JP
type9a\[Equal] ld scattering, DIS, NC
type9b\[Equal] ld scattering, DIS, CC
type10a\[Equal] nuFe \[Rule] X, DIS
type10b\[Equal] nuFe \[Rule] cX, DIS
type11\[Equal] p copper scattering, VBP
type12==pd \[Rule] l+l-X, VBP

type9\[Equal] combination of pp and pN scattering (pd, p copper, nu Fe)
*)

(*
type1a={101,106,169};
type1b={145,147};
type2a={};
type2b={};
type3={};
type4a={};
type4b={124,125};
type5={};
type6={260,261};
type7={225,227,234,241,266,267,268,281};
type8={514,535,538};
type9a={102,104};
type9b={};
type10a={108,109,110,111};
type10b={126,127};
type11={201};
type12={203,204};
*)

output =expItype
];

(*
input data ((output of ReadExptTable[[iExperiment, 7]]))& ExptID
output the (x,Q) list of it
*)
selectExptxQ[ExptIDin_,datain_,Sin_]:=
Module[{ExptID=ExptIDin,data=datain,S=Sin,sqrtS,expItype,datatmp,output,i},
(*
data=data/.List\[RuleDelayed]LF;
data=data/.LF[x__]\[RuleDelayed]{x}[[Xindex]];
data
*)
(*
output=Table[
datatmp=data[[i]]/.List\[RuleDelayed]LF,
{i,1,Length[data]}
];
*)

expItype=ExptIDinfo[ExptID];
sqrtS=ExptIDEcm[ExptID];
S=sqrtS^2;

output=
Switch[expItype,
"DIS",
data/.LF[a__]:>{{a}[[2]],{a}[[1]]},
"VBP1",
data/.LF[a__]:>{{a}[[2]]/Sqrt[S],{a}[[2]]},
"VBP2",
Join[data/.LF[a__]:>{({a}[[2]]/Sqrt[S])*







\!\(\*SuperscriptBox[\(\[ExponentialE]\), \({a}[\([1]\)]\)]\),{a}[[2]]},data/.LF[a__]:>{({a}[[2]]/Sqrt[S])*E^-{a}[[1]],{a}[[2]]}],(* x1 = (Q/sqrt(S))*exp(+-y) *)
"VBP3",
Join[data/.LF[a__]:>{({a}[[2]]/Sqrt[S])*







\!\(\*SuperscriptBox[\(\[ExponentialE]\), \({a}[\([1]\)]\)]\),{a}[[2]]},data/.LF[a__]:>{({a}[[2]]/Sqrt[S])*E^-{a}[[1]],{a}[[2]]}],(* formula not decided yet *)
"JP",
(* this form is for q1q2 \[Rule] j1j2, estimate x1, x2 of jet as peak of y(j1), y(j2)*)

Join[data/.LF[a__]:>{((2*{a}[[1]])/(Sqrt[S]))*E^(({a}[[3]]-{a}[[2]])/2.0),{a}[[1]]},data/.LF[a__]:>{((2*{a}[[1]])/(Sqrt[S]))*E^(({a}[[2]]-{a}[[3]])/2.0),{a}[[1]]}],(*x1=(Subscript[p, Tj]/(2Sqrt[S]))*e^Subscript[y, j]*)

(*this form is for q1q2 \[Rule] W, Z or something, so rapidity is yjj, estimating x as peak of yjj*)
(*
data/.LF[a__]\[RuleDelayed]{((2\[Times]{a}[[1]])/(Sqrt[S])),2\[Times]{a}[[1]]},
*)
_,
Print["Wrong expItype value, ",expItype,", ",ExptID] (*data/.LF[a__]\[RuleDelayed]{{a}[[1]],{a}[[2]]}*)
];

(*
output=Table[
datatmp=output[[i]]/.List\[RuleDelayed]LF,
{i,1,Length[output]}
];
*)

output 
];

(*2017.01.15, like selectExptxQ, but keep other data*)
selectExptxQv2[ExptIDin_,datain_,Sin_]:=
Module[{ExptID=ExptIDin,data=datain,S=Sin,sqrtS,expItype,datatmp,output,i},
(*
data=data/.List\[RuleDelayed]LF;
data=data/.LF[x__]\[RuleDelayed]{x}[[Xindex]];
data
*)
(*
output=Table[
datatmp=data[[i]]/.List\[RuleDelayed]LF,
{i,1,Length[data]}
];
*)

expItype=ExptIDinfo[ExptID];
sqrtS=ExptIDEcm[ExptID];
S=sqrtS^2;

output=
Switch[expItype,
"DIS",
data/.LF[a__]:>LF@@{Sequence@@{a},{a}[[2]],{a}[[1]]},
"VBP1",
data/.LF[a__]:>LF@@{Sequence@@{a},{a}[[2]]/Sqrt[S],{a}[[2]]},
"VBP2",
Join[data/.LF[a__]:>LF@@{Sequence@@{a},({a}[[2]]/Sqrt[S])*







\!\(\*SuperscriptBox[\(\[ExponentialE]\), \({a}[\([1]\)]\)]\),{a}[[2]]},data/.LF[a__]:>LF@@{Sequence@@{a},({a}[[2]]/Sqrt[S])*E^-{a}[[1]],{a}[[2]]}],(* x1 = (Q/sqrt(S))*exp(+-y) *)
"VBP3",
Join[data/.LF[a__]:>LF@@{Sequence@@{a},({a}[[2]]/Sqrt[S])*







\!\(\*SuperscriptBox[\(\[ExponentialE]\), \({a}[\([1]\)]\)]\),{a}[[2]]},data/.LF[a__]:>LF@@{Sequence@@{a},({a}[[2]]/Sqrt[S])*E^-{a}[[1]],{a}[[2]]}],(* formula not decided yet *)
"JP",
(* this form is for q1q2 \[Rule] j1j2, estimate x1, x2 of jet as peak of y(j1), y(j2)*)

Join[data/.LF[a__]:>LF@@{Sequence@@{a},((2*{a}[[1]])/(Sqrt[S]))*E^(({a}[[3]]-{a}[[2]])/2.0),{a}[[1]]},data/.LF[a__]:>LF@@{Sequence@@{a},((2*{a}[[1]])/(Sqrt[S]))*E^(({a}[[2]]-{a}[[3]])/2.0),{a}[[1]]}],(*x1=(Subscript[p, Tj]/(2Sqrt[S]))*e^Subscript[y, j]*)

(*this form is for q1q2 \[Rule] W, Z or something, so rapidity is yjj, estimating x as peak of yjj*)
(*
data/.LF[a__]\[RuleDelayed]{((2\[Times]{a}[[1]])/(Sqrt[S])),2\[Times]{a}[[1]]},
*)
_,
Print["Wrong expItype value, ",expItype,", ",ExptID] (*data/.LF[a__]\[RuleDelayed]{{a}[[1]],{a}[[2]]}*)
];

(*
output=Table[
datatmp=output[[i]]/.List\[RuleDelayed]LF,
{i,1,Length[output]}
];
*)

output 
];

selectExptresidue[ExptIDin_,datain_,Sin_]:=
Module[{ExptID=ExptIDin,data=datain,S=Sin,sqrtS,expItype,datatmp,output,i},
expItype=ExptIDinfo[ExptID];
output=data/.LF[a__]:>{a}[[13]];
output 
];

obsxQ[datain_]:=
Module[{data=datain,sqrtS,expItype,datatmp,output,i,ExptID,S},
ExptID=data[[1]];
S=dummy;
output=selectExptxQ[ExptID,data[[7]],S];
output 
];

obsxQresidue[datain_]:=
Module[{data=datain,sqrtS,expItype,datatmp,output,i,ExptID,S,xQ,residue,j,NormFac},
ExptID=data[[1]];
expItype=ExptIDinfo[ExptID];

S=dummy;
(*get {x,Q} points*)
xQ=selectExptxQ[ExptID,data[[7]],S];

(*get residue method 1*)
(*
residue=data[[7]]/.LF[a__]\[RuleDelayed]{a}[[13]];
residue=Map[If[#>0,Sqrt[#],-Sqrt[-#]]&,residue,{1}];
*)

(*get residue method 2*)
(*result of method1&2 mostly diff in 5%, method 2 should be accurate*)
(*2017.01.18: NormFac should be in formula or not?*)
NormFac=data[[4]];
residue=data[[7]]/.LF[a__]:>({a}[[5]]*NormFac-{a}[[11]])/{a}[[12]];



(*get (x,Q,residue(i))*)
(*for DIS, JP, every data make 2 points, so the residue value of the same data point should map to two points*)
output=
Table[
(*residue: 1,2,3,4...N-1,0,1,2,3,...N-1,0, then for residue[i]=0, it should be N*)
j=Mod[i,Length[residue] ]+If[Mod[i,Length[residue]]==0,Length[residue],0];
Append[xQ[[i]],residue[[j ]] ],
{i,1,Length[xQ]}
];


output
];

selectExptobservable[datain_,obsin_]:=
Module[{data=datain,obs=obsin,sqrtS,expItype,datatmp,output,i},
(*info of data*)
(*ExptID,ExptIDtoName[ExptID],R2String,NormFac,NPoints,DataDescription,DataList,S*)

output=obs[data];
output 
];


(*replace all datalist of experiment (output of ReadExptTable) by xQ list *)
selectExptxQall[datain_,Sin_]:=
Module[{data=datain,S=Sin,ExptID,datatmp,output,i},
output=
Table[
(*
ExptID=data[[iExperiment,1]];
datatmp=data[[iExperiment,7]];
*)
(* 
selectExptxQ[ExptList1[[24,1]],ExptList1[[24,7]]] 
data[iExperiment,7]=selectExptxQ[ExptID,datatmp];
*)
data[[iExperiment,7]]=selectExptxQ[data[[iExperiment,1]],data[[iExperiment,7]]] ;
data[[iExperiment]]
,
{iExperiment,Length[data]}
];
output

]


(*
input data (output of ReadExptTable) & ExptID,
then find whether data of ExptID included in it,
if data of ExptID is included, then output (x,Q) of it
if it is not included, then return a massage "can not find ExptID"

output the (exp index, flavour) of a data from datain_
output formate:
{ {expN, expid, f}, {LF[x1, Q1], LF[x2, Q2],...} }
*)
extractdata2[datain_,Nexpin_,fin_]:=
Module[{data=datain,Nexp=Nexpin,f=fin,mystr,expN,expid,dataout,dataflavor,nodataflavor,WorkDir,DataFile,myfile,tmpexpdata},
expN=0;
expid=Nexpin;

dataflavor={};nodataflavor;

Do[
If[
data[[iExperiment,1]]==expid,
dataflavor=selectExptxQ[data[[iExperiment,1]],data[[iExperiment,7]] ,data[[iExperiment,8]] ];
expN=iExperiment;
 ];,
{iExperiment,1,Length[data]}
];

If[
expN==0,
Print["the experiment does not exist in the data, ",expid];
];
dataflavor={{expN,expid,f},dataflavor};

dataflavor
]

(*
2016.11.18 botingw
input data (output of ReadExptTable) & ExptID,
then find whether data of ExptID included in it,
if data of ExptID is included, then output obsevable we want of it
if it is not included, then return a massage "can not find ExptID"

output the (exp index, flavour) of a data from datain_
output formate:
{ {expN, expid, f}, {LF[obs1, obs2,...], LF[obs1, obs2,...],...} }
*)
extractobsfromdata[datain_,Nexpin_,fin_,obsin_]:=
Module[{data=datain,Nexp=Nexpin,f=fin,obs=obsin,mystr,expN,expid,dataout,dataflavor,nodataflavor,WorkDir,DataFile,myfile,tmpexpdata},
expN=0;
expid=Nexpin;

dataflavor={};nodataflavor;

Do[
If[
data[[iExperiment,1]]==expid,
(*
dataflavor=selectExptxQ[data[[iExperiment,1]],data[[iExperiment,7]] ,data[[iExperiment,8]] ];
*)
dataflavor=selectExptobservable[data[[iExperiment]],obs ];
expN=iExperiment;
 ];,
{iExperiment,1,Length[data]}
];

If[
expN==0,
Print["the experiment does not exist in the data, ",expid];
];
dataflavor={{expN,expid,f},dataflavor};

dataflavor
]



(*use data*)
makexQdataset[datain_,Nexpin_,fin_]:=
Module[{data=datain,Nexp=Nexpin,f=fin,dataplot,allplot2,lgd,nodatalist,outputdata},
dataplot={};nodatalist={};
(* no function, delete it.
dataplot=Append[dataplot,extractdata2[data,Nexp[[1]],-5] ];
dataplot=Append[dataplot,extractdata2[data,Nexp[[2]],3] ];
*)
dataplot=Table[extractdata2[data,Nexp[[i]],f[[i]]] ,{i,1,Length[Nexp]}];
(*make list of (expid, f) sets which have no data in it*)
Do[If[Length[dataplot[[i,2]] ]==0,nodatalist=Append[nodatalist,i]],{i,1,Length[Nexp]}];
(*delete (expid, f) with no data by list we made*)
Do[dataplot=Drop[dataplot,{nodatalist[[i]]+1-i}];Nexp=Drop[Nexp,{nodatalist[[i]]+1-i}];f=Drop[f,{nodatalist[[i]]+1-i}],{i,1,Length[nodatalist]}];
(*define output data: delete subset of histogram data, means we don't want to know which subset a data point belong to*)
outputdata=Flatten[Table[dataplot[[i,2]]/.LF->List,{i,1,Length[Nexp]}],1];

outputdata
 ];


(*use data*)
makeobsdataset[datain_,Nexpin_,fin_,obsin_]:=
Module[{data=datain,Nexp=Nexpin,f=fin,obs=obsin,dataplot,allplot2,lgd,nodatalist,outputdata},
dataplot={};nodatalist={};
(* no function, delete it.
dataplot=Append[dataplot,extractdata2[data,Nexp[[1]],-5] ];
dataplot=Append[dataplot,extractdata2[data,Nexp[[2]],3] ];
*)

(*f is dummy, give it some value*)
f=Table[1,{i,1,Length[Nexp]}];

dataplot=Table[extractobsfromdata[data,Nexp[[i]],f[[i]],obs] ,{i,1,Length[Nexp]}];
(*make list of (expid, f) sets which have no data in it*)
Do[If[Length[dataplot[[i,2]] ]==0,nodatalist=Append[nodatalist,i]],{i,1,Length[Nexp]}];
(*delete (expid, f) with no data by list we made*)
Do[dataplot=Drop[dataplot,{nodatalist[[i]]+1-i}];Nexp=Drop[Nexp,{nodatalist[[i]]+1-i}];f=Drop[f,{nodatalist[[i]]+1-i}],{i,1,Length[nodatalist]}];
(*define output data: delete subset of histogram data, means we don't want to know which subset a data point belong to*)
outputdata=Flatten[Table[dataplot[[i,2]]/.LF->List,{i,1,Length[Nexp]}],1];

outputdata
 ];

(*2016.11.04 botingw*)
(*plot pdf function with log log scale*)
(*if plotrangein\[Equal]"None", then plotrange is by default*)
(*example input: *)
(*
myMarker
myplotstyle
title="(x, Q) points of experiments based on CT14LN fitting\n (Heavy Nucleons included)";
xtitle="x";
ytitle="Q [GeV]";
lgd={"pl \[Rule] lX, NC","pl \[Rule] clX, NC","\[Nu]N \[Rule] X, CC","\[Nu]N \[Rule] cX, CC","pOverscript[p, _] \[Rule] l^+l^-X, \[ScriptD]\[Sigma]/\[ScriptD]y","pOverscript[p, _] \[Rule] l\[Nu]X, A(Subscript[y, l])"};
lgdpos={0.15,0.6};
PDFloglogplot[tmpDISdataf1,myMarker,myplotstyle,title,xtitle,ytitle,{0.001,5,1,500},lgd,lgdpos]

myMarker={{"\[FilledCircle]",6.272`},{"\[FilledSquare]",6.272`},{"\[FilledDiamond]",7.616`},{"\[FilledDiamond]",7.616`},{"\[EmptyCircle]",7.167999999999999`},{"\[EmptySquare]",7.167999999999999`}};
myplotstyle={,,,,,};

*)
PDFloglogplot[datain_,plotmarkerin_,plotstylein_,titlein_,xtitlein_,ytitlein_,plotrangein_,lgdin_,lgdposin_]:=
Module[{data=datain,plotmarker=plotmarkerin,plotstyle=plotstylein,title=titlein,xtitle=xtitlein,ytitle=ytitlein,plotrange=plotrangein,lgd=lgdin,lgdpos=lgdposin,plotxQout,minx,maxx,miny,maxy,imgsize,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,plotrangetmp},

(*default*)
minx=0.00001;
maxx=1;
miny=1;
maxy=1100;
imgsize={{600},{600}};
titlesize=24;
xtitlesize=18;
ytitlesize=18;
lgdlabelsize=12;
ticklablesize=18;

(*if want to customize the plot range*)
plotrangetmp=ToString[plotrange];
If[
plotrangetmp!="None",
minx=plotrange[[1]];
maxx=plotrange[[2]];
miny=plotrange[[3]];
maxy=plotrange[[4]];
];


plotxQout=ListLogLogPlot[
data,

Frame->True,
(*
PlotMarkers\[Rule]{Automatic,Markersize},
*)
PlotMarkers->plotmarker,
PlotStyle->plotstyle,
BaseStyle->{FontSize->18(*,FontFamily\[Rule]"Helvetica"  test close it*)},
ImageSize->imgsize,(*size of image, including title, xy title, it is not just size of image frame *)
FrameLabel->{Style[xtitle,xtitlesize],Style[ytitle,ytitlesize]},
PlotLabel->Style[title,titlesize],
FrameTicksStyle->Directive[Black,ticklablesize],
PlotRange->{{minx,maxx},{miny,maxy}},
PlotLegends->Placed[PointLegend[lgd,LabelStyle->Directive[Black,lgdlabelsize,FontFamily->"Latin Modern Roman Caps"],LegendMarkerSize->15,LegendFunction->Framed ],lgdpos],
GridLines->{{0.00001,0.0001,0.001,0.01,0.1},{10,50,100,500,1000}},
GridLinesStyle->Directive[Dashed],
AspectRatio->1(*size ratio of x, y frame *)


];

plotxQout
 ]


(*input: RGB="R" or "G" or "B" or "Brown" or "Gray"*)
(*output: red (red, deep red, deep orange...), green, blue, blown, gray color lists*)
colorset[RGBin_]:=
Module[{RGB=RGBin,colorset,Rcolor,Gcolor,Bcolor,Graycolor,Browncolor,output},
colorset=ColorData["WebSafe","ColorList"];
Rcolor={colorset[[12]],colorset[[14]],colorset[[32]],colorset[[18]],colorset[[23]],colorset[[29]],colorset[[36]],colorset[[22]],Red,colorset[[43*1+29]]};
Gcolor={colorset[[43*4+12]],colorset[[43*4+14]],colorset[[43*4+20]],colorset[[43*4+26]],colorset[[43*4+32]],colorset[[43*2+40]],colorset[[41]],Green};
Bcolor={colorset[[43*4+3]],colorset[[43*4+9]],colorset[[43*4+15]],colorset[[43*4+22]],colorset[[43]],colorset[[43*2+7]],colorset[[43*2+13]],colorset[[43*1+30]],Blue};
Graycolor={colorset[[43*1+1]],colorset[[43*2+1]],colorset[[43*3+1]],colorset[[43*4+1]],Gray};
Browncolor={colorset[[43*2+9]],colorset[[43*1+9]],colorset[[43*3+9]],colorset[[43*2+16]],colorset[[43*2+22]],Brown};

output=
Switch[
RGB,
"R",Rcolor,
"G",Gcolor,
"B",Bcolor,
"Gray",Graycolor,
"Brown",Browncolor,
_,
Print["error, input should be \"R\" or \"G\" or \"B\" or \"Brown\" or \"Gray\""]
];

output

]









End[];  (* End Private Context *)

EndPackage[]; (* End Package Context *)
