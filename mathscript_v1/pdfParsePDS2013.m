(* ::Package:: *)

(* ::Text:: *)
(*Mathematica package to parse CTEQXX .pds files*)
(*History:*)
(*Nov 15, 2012 -- v. 0.1, Ben Clark, Fred Olness*)
(*April 15, 2013 -- v. 0.2, Pavel Nadolsky, implemented the new CT10 NNLO format, additional error control features, a flag for verbose printing in pdfParseCTEQ*)
(*April 2015 -- Pavel Nadolsky, copied and revised functions from Ben for computations of Monte-Carlo uncertainties and correlations *)


(* ::Section:: *)
(*Setup Package*)


BeginPackage["parsePDS`"]; 
pdfFlavor::usage="pdfFlavor[iflavor]: return a string with the name of the PDF flavor iflavor. Example:
pdfFlavor[0], pdfFlavor[1], pdfFlavor[2] return \"g\", \"u\", \"d\" for the gluon, up quark, and down quark PDFs.";
pdfFamilyParseCTEQ::usage="pdfFamilyParseCTEQ[MyPath,[ifamily]]: read multiple PDS in the path MyPath and stores them 
into a set called a \"PDF family\". Without optional parameters, pdfFamilyParseCTEQ reads a new family containing all 
.pds files specified in MyPath and prints out the numerical ID of this family. If an optional parameter ifamily is specified, 
the new .pds files will be added to the already existing family # ifamily. 
Examples:
  pdfFamilyParseCTEQ[\"MyGrids/ct10*pds\"] reads all .pds files in the subdirectory MyGrids/ beginning with \"ct10\" into a new family.
  pdfFamilyParseCTEQ[\"MyGrids/ct10*pds\",2]: If 2 or more families have been already read, pdfFamilyParseCTEQ will add the .new pds files
  into family 2, will print out an error message if family 2 does not exist.";
pdfSetActiveFamily::usage="pdfSetActiveFamily[ifamily] activates family # ifamily in the memory for plotting, etc.
Example: pdfSetActiveFamily[3] will activate access to PDF grids in family 3";
pdfGetActiveFamily::usage="pdfGetActiveFamily returns the numerical ID of the currently active PDF family.";
pdfParseCTEQ::usage="pdfParseCTEQ[FileName,[Verbose]]: read a .pds file FileName into the active PDF family; set the optional parameter Verbose=False to suppress the printout";
pdfCTEQ::usage="pdfCTEQ[x,q,iflavor,iset]: return the value of the pdf for flavor iflavor from a PDF set iset in the currently active family, at the given momentum fraction x and scale Q";
pdfLOWCTEQ::usage="pdfLOWCTEQ[x, q, iflavor, iset,power]: return the value of the pdf as in pdfCTEQ, but with a interpolation below the minimum x value that goes as 1/x^power (default power=1)";
(* PN April 26, 2013 ->*)
pdfCTEQFamily::usage="pdfCTEQFamily[x, q, iflavor]: return a table of PDF values for flavor iflavor for all sets in the currently active family, at the given momentum fraction x and scale Q";
pdfHessianSymError::usage="pdfHessianSymError[x, q, ipart]: returns the symmetric PDF uncertainty for Hessian PDF error sets, assuming that the family contains one central PDF set 
and an even number of PDF eigenvector sets.
pdfHessianSymError[f]: compute the symmetric PDF uncertainty for the function f[iset] or list f[[iset]] obtained with Hessian PDF error sets.";
pdfHessianPlusError::usage="pdfHessianPlusError[x, q, ipart]: returns the positive asymmetric PDF uncertainty for Hessian PDF error sets, assuming that the family contains one central PDF set and an even number of PDF eigenvector sets.
pdfHessianPlusError[f]: compute the positive asymmetric PDF uncertainty for the function f[iset] or list f[[iset]] obtained with Hessian PDF error sets.";
pdfHessianMinusError::usage="pdfHessianMinusError[x, q, ipart]: returns the negative asymmetric PDF uncertainty for Hessian PDF error sets, assuming that the family contains one central PDF set and an even number of PDF eigenvector sets.
pdfHessianMinusError[f]: computes the negative asymmetric PDF uncertainty for the function f[iset] or list f[[iset]] obtained with Hessian PDF error sets.";
pdfLuminosity::usage="pdfLuminosity[sqrts,MX,ipdf1,ipdf2,iset,MyPrecisionGoal]: returns the parton luminosity for collider energy \!\(\*SuperscriptBox[\(s\), \(1/2\)]\)=sqrts, 
particle mass MX, PDF flavors ipdf1 and ipdf2, PDF set iset (default 1), computing the numerical integral 
with the precision goal MyPrecisionGoal (default: 3 figures). The parton luminosity is defined according to Eq. ??? in Campbell, Huston, Stirling, arXiv:...";
pdfLuminosityHessianSymError::usage="pdfLuminosityHessianSymError[sqrts,MX,ipdf1,ipdf2,MyPrecisionGoal]: returns the symmetric Hessian PDF uncertainrty on the 
parton luminosity for collider energy \!\(\*SuperscriptBox[\(s\), \(1/2\)]\)=sqrts, particle mass MX, PDF flavors ipdf1 and ipdf2, computing the numerical integral 
with the precision goal MyPrecisionGoal (default: 3 figures).";
pdfLuminosityHessianPlusError::usage="pdfLuminosityHessianPlusError[sqrts,MX,ipdf1,ipdf2,MyPrecisionGoal]: returns the positive asymmetric Hessian PDF uncertainrty on the 
parton luminosity for collider energy \!\(\*SuperscriptBox[\(s\), \(1/2\)]\)=sqrts, particle mass MX, PDF flavors ipdf1 and ipdf2, computing the numerical integral 
with the precision goal MyPrecisionGoal (default: 3 figures).";
pdfLuminosityHessianMinusError::usage="pdfLuminosityHessianMinusError[sqrts,MX,ipdf1,ipdf2,MyPrecisionGoal]: returns the negative asymmetric Hessian PDF uncertainrty on the 
parton luminosity for collider energy \!\(\*SuperscriptBox[\(s\), \(1/2\)]\)=sqrts, particle mass MX, PDF flavors ipdf1 and ipdf2, computing the numerical integral 
with the precision goal MyPrecisionGoal (default: 3 figures).";
(* PN April 7, 2015 <-*)
pdfMCMean::usage="pdfMCMean[x,q,ipart]: returns the mean value for Monte Carlo PDF replica sets.
pdfMCMean[f]: returns the mean value for the function f[iset] or the list f[[iset]] obtained with Monte Carlo PDF replica sets."; 
pdfMCStdDeviation::usage="pdfMCStdDeviation[x,q,ipart]: returns the standard deviation for Monte Carlo PDF error sets.  
pdfMCStdDeviation[f]: returns the standard deviation for the function f[iset] or list f[[iset]] obtained with Monte Carlo PDF replica sets.";
pdfMCCorrelation::usage="pdfMCCorrelation[list1, list2]: returns the correlation between two lists of observables calculated with Monte Carlo PDF replica sets.";
pdfMCCentralIntervals::usage="pdfMCCentralIntervals[x,q,ipart]: returns {fc, flow, fup}, where fc is the central value of the PDF f(x,q,ipart) on a sample of Monte Carlo PDF replica sets, corresponding to the cumulative probability of 1/2; flow and fup are the lower and upper limits of the central 68.2% probability interval, corresponding to the cumulative probabilities of (1-0.682)/2 and (1+0.682)/2. No assumptions about the statistical distribution of the MC replicas are made.
pdfMCCentralIntervals[x,q,ipart,{p1, p2,...}]: returns {fc, flow1, fup1, flow2, fup2, ...}, where fc is the central value, as above; flow1, fup1, flow2, fup2, ... are the upper and lower limits of the central intervals containing probabilities p1, p2, ... ( 0 <= p <= 1). 
pdfMCCentralIntervals[f] and pdfMCCentralIntervals[f, {p1, p2,...}]: same as above, where either a function f[iset] or a list f[[iset]] provides the values of the PDFs for each Monte Carlo replica."; 
pdfMCEnvelope::usage="pdfMCEnvelope[x,q,ipart]: returns  {fc, flow, fup}, where fc is the central value of the PDF f(x,q,ipart) on a sample of Monte Carlo PDF replica sets, corresponding to the cumulative probability of 1/2; flow and fup are the lower and upper limits of the envelope interval, corresponding to the cumulative probabilities of 0 and 1. Same as pdfMCCentralIntervals[x,q,ipart,{0,1}].
pdfMCEnvelope[f]: the envelope for either a function f[iset] or a list f[[iset]] providing the PDFs for each Monte Carlo replica.";
(* PN April 7, 2015 -> *)
(* PN April 26, 2013 <-*)
pdfHessianCorrelation::usage="pdfHessianCorrelation[list1, list2]: returns the correlation between two lists of observables calculated with Hessian PDF error sets.";
pdfSetList::usage="pdfSetList: prints the list of the loaded .pds grids, their iset numbers, maximal number of quark flavors, and number of valence flavors in each set.
Examples: pdfSetList//TableForm prints .pds grids and their iset numbers in all families.
 pdfSetList[[4]]//TableForm prints .pds grids and their iset numbers in family4.";
pdfResetCTEQ::usage="pdfResetCTEQ: delete all .pds data files from Mathematica and reset all internal variables in the package";
pdfCheckXlist::usage="pdfCheckXlist[iset]: return the x values in the PDF grid # iset";
pdfCheckQlist::usage="pdfCheckQlist[iset]: return the Q values in the PDF grid # iset";
pdfXmin::usage="pdfXmin[iset]: return the minimum x value in the PDF grid # iset";
Begin["`Private`"]; 


(* ::Section:: *)
(*Loading and parsing nCTEQ PDFs*)


Nfamilies=0; ifamily=0;
nSetCount={}; 
pdfSetList={};
pdfTableData={};
(* The above are global variables *)

(*this function will parse cteq 6.6 and 10 .pds files*)
pdfParseCTEQ[filename_?StringQ,Verbose_:True]:=Module[{stream,order,ipk,nfl,ipdsformat,Qalpha,AlfaQ,lambda,m1,m2,m3,m4,m5,m6,ipd0,ihdn,iknl,nfmx,nfval,nx,nt,ng,dum,idum,qini,qmax,xmin,xcr,xlist,nblk,length,biglist,nx1,nt1,list1,xlist1,qlist1,pdstype},

stream=OpenRead[filename];
SetStreamPosition[stream,0];
tmpstring=Read[stream,String];
If[Verbose,Print[tmpstring]];
pdstype=StringTake[Read[stream,Record,RecordSeparators->","],-4];Read[stream,Record];
If[pdstype=="Ordr",
ipdsformat = 6;           (* CTEQ6 .6 .pds format; alpha_s  is not specified *)
{order,nfl,lambda,m1,m2,m3,m4,m5,m6}=Read[stream,Table[Number,{i,1,9}]];
Read[stream,String];
{ipd0,ihdn,iknl,nfmx,nfval,dum,dum}=Read[stream,Table[Number,{i,1,7}]],
 (* Post-CTEQ6 .6 formats*)
{ipk,order,Qalpha,AlfaQ,m1,m2,m3,m4,m5,m6}=Read[stream,Table[Number,{i,1,10}]];
pdstype2=StringTake[Read[stream,Record,RecordSeparators->","],-5];Read[stream,Record];
If[pdstype2=="IMASS",
          ipdsformat = 11; (*CT12 .pds format*)
          {aimass, fswitch,idum,idum,idum, nfmx, nfval}=Read[stream,Table[Number,{i,1,7}]],
ipdsformat=10; (* Pre-CT10 NNLO format *)
          {idum,idum,idum, nfmx, nfval}=Read[stream,Table[Number,{i,1,5}]]
](*If[pdstype2->...*)
](*If[pdstype->...*)
Read[stream,String];
{nx,nt,idum,ng,idum}=Read[stream,Table[Number,{i,1,5}]];
 If[ng>0, Read[stream,Table[Record,{i,1,ng+1}]]];
t1=Read[stream,Record];
{qini,qmax}=Read[stream,{Number,Number}];
If[ipdsformat==11, 
(*Post-CT10 format *)
qlist=Read[stream,Table[LF[Number,Number,Number],{i,0,nt}]]/.LF[a__]:>{{a}[[1]],{a}[[2]]};
,
(*Pre-CT10 format *)
qlist=Read[stream,Table[{Number,Number},{i,0,nt}]]
];
Read[stream,String];
{xmin,xcr}=Read[stream,{Number,Number}];
xlist=Read[stream,Table[Number,{i,1,nx}]];
Read[stream,String];
nblk=(nx+1)*(nt+1);
length=nblk*(nfmx+1+nfval);
biglist=Read[stream,Table[Number,{i,1,length}]];
nx1=nx+1;
nt1=nt+1;
nflav=(nfmx+1+nfval);
list1=Partition[Partition[biglist,{nx1}],{nt1}];
x0=0; (* THIS JUST PADS THE X-LIST *)
xlist1=Join[{x0},xlist];
qlist1=qlist//Transpose//First;
Close[stream];

If[Length[$MessageList]==0,
If[Nfamilies==0,Nfamilies++;ifamily=Nfamilies; AppendTo[nSetCount,0]; AppendTo[pdfTableData,{}]; AppendTo[pdfSetList,{}]];

nSetCount[[ifamily]]=nSetCount[[ifamily]]+1;
AppendTo[ pdfTableData[[ifamily]], {xlist1,qlist1,list1} ];
AppendTo[ pdfSetList[[ifamily]],   {nSetCount[[ifamily]],filename,nfmx,nfval}  ];
Return[nSetCount[[ifamily]]],

Print[filename," was not initialized: ",Length[$MessageList]," error messages"]
](* If[Length[$MessageList]==0 *);
];


pdfResetCTEQ:=Module[{},
Nfamilies=0;
nSetCount={};
pdfSetList={};
pdfTableData={};
];


pdfSetActiveFamily[ifamilyin_?IntegerQ]:=Module[{},
If[(ifamilyin>0 && ifamilyin <= Nfamilies),
ifamily=ifamilyin,
Print["Error: requested family ",ifamilyin," is not in the initialized range 0 \[LessEqual] ifamily \[LessEqual] ",Nfamilies]
];
];


pdfGetActiveFamily:=Module[{},
Return[ifamily];
];


pdfFamilyParseCTEQ[path_?StringQ,ifamilyin_:0]:=
Module[{fileList,output,fleng,currDir,nSetCountOld},
fileList=FileNames[path];
fleng=Length[fileList];
If[(ifamilyin==0 || ifamilyin=="new"),
(*Initialize a new family *)
Nfamilies++; AppendTo[nSetCount,0]; AppendTo[pdfTableData,{}]; AppendTo[pdfSetList,{}];ifamily=Nfamilies;nSetCountOld=0,
(*Access an old family, if it exists *)
If[(ifamilyin > 0 && ifamilyin <= Nfamilies),
ifamily=ifamilyin;nSetCountOld=nSetCount[[ifamily]],
Print["Error: requested family ",ifamilyin," is not in the initialized range ifamily \[LessEqual] ",Nfamilies];Return[]
]
];
Do[pdfParseCTEQ[fileList[[i]],False],{i,1,fleng}];
Print["Included ",nSetCount[[ifamily]]-nSetCountOld," more files in the PDF family ",ifamily];
Return[fleng];
];


(* ::Section:: *)
(*Interpolation*)


(* ::Subsection:: *)
(*This is a 4 - point (cubic) interpolation funtion.If we redifine this a a vector interpolation, we can have an arbitrary number of dimensions.*)


interpol4[xIn_,data_]:=Module[
{x,xvec,qvec,x0,x1,x2,x3,q0,q1,q2,q3,c0,c1,c2,c3,output},
{xvec,qvec}=Transpose[data];
x=xIn;
{x0,x1,x2,x3}=xvec;
{q0,q1,q2,q3}=qvec;
c0=((x-x1)/(x0-x1)) ((x-x2)/(x0-x2)) ((x-x3)/(x0-x3));
c1=((x-x0)/(x1-x0)) ((x-x2)/(x1-x2)) ((x-x3)/(x1-x3));
c2=((x-x0)/(x2-x0)) ((x-x1)/(x2-x1)) ((x-x3)/(x2-x3));
c3=((x-x0)/(x3-x0)) ((x-x1)/(x3-x1)) ((x-x2)/(x3-x2));
output=q0 c0+q1 c1+q2 c2+q3 c3;
If[output<0,Return[0],Return[output]]
];


(* ::Subsection:: *)
(*Interpolation Subfunctions*)


bigger[xin_,xintp_]:=(xin>=xintp);
SetAttributes[bigger,Listable];

findXindex[x_,xlist_]:=Module[{pos,xmin,xmax,r},
xmin=xlist[[2]];
r=Length[xlist];
xmis=xlist[[r-1]];
If[x<=xmin,Return[4],If[x>=xmis,Return[(r-1)],Return[Position[bigger[xlist,x],True]//First//First]];];
];

findQindex[q_,qlist_]:=Module[{pos,qmin,qmax,r},
qmin=qlist[[2]];
r=Length[qlist];
qmis=qlist[[r-1]];
If[q<=qmin,Return[3],If[q>=qmis,Return[(r-1)],Return[Position[bigger[qlist,q],True]//First//First]];];
];

getXdata[xi_,qi_,flav_,xlist_,grid_]:=Transpose[{xlist[[xi-2;;xi+1]],grid[[flav,qi,xi-2;;xi+1]]}];
getQdata[xi_,qi_,flav_,qlist_,grid_]:=Transpose[{qlist[[qi-2;;qi+1]],grid[[flav,qi-2;;qi+1,xi]]}];

doQinterp[x_,xi_,qi_,flav_,xlist_,qlist_,grid_]:=
Module[{pvec,qvec,output},
pvec={
interpol4[x,getXdata[xi,qi-2,flav,xlist,grid]],
interpol4[x,getXdata[xi,qi-1,flav,xlist,grid]],
interpol4[x,getXdata[xi,qi,flav,xlist,grid]],
interpol4[x,getXdata[xi,qi+1,flav,xlist,grid]]};
qvec=qlist[[qi-2;;qi+1]];
output={qvec,pvec}//Transpose;
Return[output]
];

fullinterp[x_?NumericQ,q_?NumericQ,flav_,list_]:=
Module[{xi,qi,qData,output,xlist,qlist,grid},
xlist=list[[1]];
qlist=list[[2]];
grid=list[[3]];
xi=findXindex[x,xlist];
qi=findQindex[q,qlist];
qData=doQinterp[x,xi,qi,flav,xlist,qlist,grid];
output=interpol4[q,qData];
Return[output];
];


(* ::Section:: *)
(*DefinePDF (CTEQ)*)


(* ::Subsection:: *)
(*The pdf function accepts values of x, q, parton #, and pdf data set in the following syntax: pdf[x,q,ipart,{dataset}].*)


plist[x_?NumericQ,q_?NumericQ,flav_?IntegerQ,list_]:=fullinterp[x,q,flav,list];

pdfX[x_?NumericQ,q_?NumericQ,ipart_?IntegerQ,iset_]:=Module[{list,tpdf,nfmx,nfval},
list=pdfTableData[[ifamily,iset]];
{nfmx,nfval}={pdfSetList[[ifamily,iset,3]],pdfSetList[[ifamily,iset,4]]};
If[list==False,Return[Null]];  (*Check that table exists *)
If[Abs[ipart]>nfmx,Return[0]];
If[ipart >= -nfmx && ipart <= nfval,
Return[plist[x,q,nfmx+ipart+1,list]],
Return[plist[x,q,nfmx-ipart+1,list]]
];
];



pdfCTEQ[x_, q_, ipart_, iset_:1] := pdfX[x, q, ipart, iset];

pdfLOWCTEQ[x_, q_, ipart_, iset_:1,power_:1.0] := 
Module[{xmin,output,list},
If[pdfTableData[iset]==False,Return[Null]];  (*Check that table exists. 
Without this call function will not return null for a nonexistent list *)
xmin=pdfxmin[ifamily,iset];
output=If[x>xmin,pdfX[x,q,ipart,iset],pdfX[xmin,q,ipart,iset] (xmin/x)^power];
Return[output];
];

pdfCTEQFamily[x_, q_, ipart_]:=Table[pdfX[x, q, ipart, iset],{iset,nSetCount[[ifamily]]}];

(* PN April 26, 2013 *)
pdfHessianSymError[x_, q_, ipart_]:=If[OddQ[nSetCount[[ifamily]]],
1/2 Sqrt[Sum[(pdfX[x, q, ipart, 2*iset+1]-pdfX[x, q, ipart, 2*iset])^2,{iset,1,(nSetCount[[ifamily]]-1)/2}]],
Print["Error: pdfHessianSymError requires (2*i + 1) PDF sets in the family"]];

pdfHessianSymError[f_]:=Module[{Neigen},
If[ListQ[f],Neigen=Length[f],Neigen=nSetCount[[ifamily]]];
If[OddQ[Neigen],
1/2 Sqrt[Sum[
If[ListQ[f],(f[[2*iset+1]]-f[[2*iset]])^2,(f[2*iset+1]-f[2*iset])^2],
{iset,1,(nSetCount[[ifamily]]-1)/2}]],
Print["Error: pdfHessianSymError requires an odd number of entries, not ",Neigen]]
]; 

pdfHessianPlusError[x_, q_, ipart_]:=Module[{Central},
Central=pdfX[x, q, ipart, 1];
If[OddQ[nSetCount[[ifamily]]],Sqrt[Sum[Max[pdfX[x, q, ipart, 2*iset]-Central,pdfX[x, q, ipart, 2*iset+1]-Central,0]^2,{iset,1,(nSetCount[[ifamily]]-1)/2}]],
Print["Error: pdfHessianPlusError requires (2*i + 1) PDF sets in the family"]]];

pdfHessianMinusError[x_, q_, ipart_]:=Module[{Central},
Central=pdfX[x, q, ipart, 1];
 If[OddQ[nSetCount[[ifamily]]],
Sqrt[Sum[Max[Central-pdfX[x, q, ipart, 2*iset],Central-pdfX[x, q, ipart, 2*iset+1],0]^2,{iset,1,(nSetCount[[ifamily]]-1)/2}]],
Print["Error: pdfHessianMinusError requires (2*i + 1) PDF sets in the family"]]];

pdfHessianPlusError[f_]:=Module[{Central,Neigen},
{Central,Neigen}=If[ListQ[f], {f[[1]],Length[f]}, {f[1],nSetCount[[ifamily]]}];
If[OddQ[Neigen],Sqrt[Sum[If[ListQ[f],
Max[f[[2*iset]]-Central,f[[2*iset+1]]-Central,0]^2,Max[f[2*iset]-Central,f[2*iset+1]-Central,0]^2],{iset,1,(Neigen-1)/2}]],
Print["Error: pdfHessianPlusError requires an odd number of entries, not ",Neigen]]];

pdfHessianMinusError[f_]:=Module[{Central,Neigen},
{Central,Neigen}=If[ListQ[f], {f[[1]],Length[f]},  {f[1],nSetCount[[ifamily]]}];
If[OddQ[Neigen],Sqrt[Sum[If[ListQ[f],
			    Max[Central-f[[2*iset]],Central-f[[2*iset+1]],0]^2,Max[Central-f[2*iset],Central-f[2*iset+1],0]^2],{iset,1,(Neigen-1)/2}]],
Print["Error: pdfHessianMinusError requires an odd number of entries, not ",Neigen]]];

pdfLuminosity[sqrts_,MX_,ipdf1_,ipdf2_,iset_:1,MyPrecisionGoal_:3]:=Module[{s,\[Tau],\[Alpha]x},
\[Alpha]x=0.3;s=sqrts^2;\[Tau]=MX^2/s;1/s NIntegrate[1/x1 pdfCTEQ[x1,MX,ipdf1,iset]pdfCTEQ[\[Tau]/x1,MX,ipdf2,iset],{x1,\[Tau],1},PrecisionGoal->MyPrecisionGoal]
];

pdfLuminosityHessianSymError[sqrts_,MX_,ipdf1_,ipdf2_,MyPrecisionGoal_:3]:=If[OddQ[nSetCount[[ifamily]]],
1/2 Sqrt[Sum[(pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 2*iset+1,MyPrecisionGoal]-pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 2*iset,MyPrecisionGoal])^2,{iset,1,(nSetCount[[ifamily]]-1)/2}]],
Print["Error: pdfLuminosityHessianSymError requires (2*i + 1) PDF sets in the family"]];

pdfLuminosityHessianPlusError[sqrts_,MX_,ipdf1_,ipdf2_,MyPrecisionGoal_:3]:=Module[{Central},
If[OddQ[nSetCount[[ifamily]]],
Central=pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 1,MyPrecisionGoal];
Sqrt[
Sum[
Max[
pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 2*iset+1,MyPrecisionGoal]-Central,
pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 2*iset  ,MyPrecisionGoal]-Central,
0]^2,{iset,1,(nSetCount[[ifamily]]-1)/2}]],
Print["Error: pdfLuminosityHessianSymError requires (2*i + 1) PDF sets in the family"]]
];

pdfLuminosityHessianMinusError[sqrts_,MX_,ipdf1_,ipdf2_,MyPrecisionGoal_:3]:=Module[{Central},
Central=pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 1,MyPrecisionGoal];
If[OddQ[nSetCount[[ifamily]]],
Sqrt[
Sum[
Max[
Central-pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 2*iset+1,MyPrecisionGoal],
Central-pdfLuminosity[sqrts, MX, ipdf1,ipdf2, 2*iset  ,MyPrecisionGoal],
0]^2,{iset,1,(nSetCount[[ifamily]]-1)/2}]],
Print["Error: pdfLuminosityHessianSymError requires (2*i + 1) PDF sets in the family"]]
];

pdfHessianCorrelation[list1_, list2_] :=Module[{Neigen,Neigen1, Neigen2, PDFerror1, PDFerror2},
   If[ ! ListQ[list1] || ! ListQ[list2], 
    Print["pdfHessianCorrelation: both arguments must be lists"]; Return];
   If[Length[list1] != Length[list2],
    Print["Problem: length of the lists do not match, ", 
     Length[list1] , "!=", Length[list2]; Return]
    ];
   Neigen = Length[list1];
   If[EvenQ[Neigen], Print["Stop, an even number of eigenvectors: ", Neigen]; Return];
   {PDFerror1, PDFerror2} = Max[pdfHessianSymError[#], 10^-8] & /@ {list1, list2};
   1/(PDFerror1 PDFerror2) 1/4 Sum[(list1[[2*iset + 1]] - list1[[2*iset]])*(list2[[2*iset + 1]] - list2[[2*iset]]), {iset, 1, (Neigen - 1)/2}]
   ];

(* PN April 7, 2015 <-*)
pdfMCMean[x_,q_,ipart_]:=Module[{nreps,mean},
nreps=nSetCount[[ifamily]];
 If[nreps < 1,
   Print["Can't compute pdfMCMean: the number of MC replicas = ",nreps," < 1"];
   Return[0]
];

 mean=(1/nreps)*Sum[pdfX[x,q,ipart,iset],{iset,1,nreps}];
 Return[mean]
];

pdfMCMean[f_]:=Module[{nreps,mean},
nreps=If[ListQ[f], Length[f], nSetCount[[ifamily]] ];
 If[nreps < 1,
   Print["Can't compute pdfMCMean: the number of MC replicas = ",nreps," < 1"];
   Return[0]
];

If[ListQ[f],
   mean = (1/nreps)*Sum[f[[i]],{i,1,nreps}],
   mean = (1/nreps)*Sum[f[i],{i,1,nreps}]
  ];
   Return[mean]
];

pdfMCStdDeviation[x_, q_, ipart_]:=Module[{nreps,mean,output},
nreps=nSetCount[[ifamily]];
 If[nreps < 2,
   Print["Can't compute pdfMCStdDeviation: the number of MC replicas = ",nreps," < 2"];
   Return[0]
];

mean=pdfMCMean[x,q,ipart];
output=Sqrt[(1/(nreps-1))*Sum[(pdfX[x,q,ipart,iset] - mean)^2,{iset,1,nreps}]];
Return[output]
];

pdfMCStdDeviation[f_]:=Module[{nreps,mean,output},
If[ListQ[f],nreps=Length[f],nreps=nSetCount[[ifamily]]];

If[nreps < 2,
   Print["Can't compute pdfMCStdDeviation: the number of MC replicas = ",nreps," < 2"];
   Return[0]
];

mean=pdfMCMean[f];
If[ListQ[f],
output=Sqrt[(1/(nreps-1))*Sum[(f[[iset]] - mean)^2,{iset,1,nreps}]],
output=Sqrt[(1/(nreps-1))*Sum[(f[iset] - mean)^2,{iset,1,nreps}]]
];
Return[output]
];

pdfMCCorrelation[list1_, list2_] := 
  Module[{nreps, mean1, mean2, list3, mean3, PDFerror1, 
	  PDFerror2, output},
    If[ ! ListQ[list1] || ! ListQ[list2], 
	Print["pdfMCCorrelation: both arguments must be lists"]; Return[]];
    If[Length[list1] != Length[list2], 
       Print["Problem: length of the lists do not match, ", 
       Length[list1] , "!=", Length[list2]]; Return[]];
    nreps = Length[list1];

    If[nreps < 2,
    Print["Can't compute pdfMCorrelation: the number of MC replicas = ",
          nreps," < 2"];
    Return[0]
    ];

    PDFerror1 = pdfMCStdDeviation[list1];
    PDFerror2 = pdfMCStdDeviation[list2];
    mean1 = pdfMCMean[list1];
    mean2 = pdfMCMean[list2];

    list3 = list1*list2;
    mean3 = pdfMCMean[list3];
    output = 
     nreps*(mean3 - mean1 mean2)/(nreps - 1)/PDFerror1/PDFerror2;

    Return[output]
    (*cov=(1/nreps)*Sum[(list1[[iset]]-mean1)*(list2[[iset]]-
    mean2),{iset,2,nreps+1}];
    from http://mathworld.wolfram.com/Covariance.html
    *)
   ];

pdfMCCentralIntervals[x_, q_, ipart_, p_: {0.682}] := 
  Module[{nreps, values, output = {}}, 
   nreps = Length[pdfSetList[[ifamily]]];
   If[nreps < 2, 
    Print["Can't compute pdfMCCentralIntervals: the number of MC replicas = ", nreps, " < 2"];
    Return[0]];
   values = Sort[Table[pdfCTEQ[x, q, ipart, iset], {iset, nreps}]];
   (* First, find the central (median) value *);
   y = nreps/2; 
   output = 
    Append[output, 
     values[[Floor[y]]] + (values[[Ceiling[y]]] - values[[Floor[y]]])*
       FractionalPart[y]];
   output = Join[output,
     Flatten[
      Table[
       {xlow, xup} = {nreps*(1.0 - p[[ip]])/2, 
         nreps*(1.0 + p[[ip]])/2};
       values[[Floor[#]]] + (values[[Ceiling[#]]] - 
             values[[Floor[#]]])*FractionalPart[#] & /@ {xlow, xup},
       {ip, 1, Length[p]}]
      ]
     ];
   Return[output]
   ];

pdfMCCentralIntervals[f_, p_: {0.682}] := 
  Module[{nreps, values, output = {}},
   If[ListQ[f], nreps = Length[f], 
    nreps = Length[pdfSetList[[ifamily]]]];
   
   If[nreps < 2, 
    Print["Can't compute pdfMCCentralIntervals: the number of MC replicas = ", nreps, " < 2"];
    Return[0]];
   If[ListQ[f],
    values = Sort[Table[f[[iset]], {iset, nreps}]],
    values = Sort[Table[f[iset], {iset, nreps}]]
    ];
   
   (* First, find the central (median) value *);
   y = nreps/2; 
   output = 
    Append[output, 
     values[[Floor[y]]] + (values[[Ceiling[y]]] - values[[Floor[y]]])*
       FractionalPart[y]];
   output = Join[output,
     Flatten[
      Table[
       {xlow, xup} = {nreps*(1.0 - p[[ip]])/2, 
         nreps*(1.0 + p[[ip]])/2};
       values[[Floor[#]]] + (values[[Ceiling[#]]] - 
             values[[Floor[#]]])*FractionalPart[#] & /@ {xlow, xup},
       {ip, 1, Length[p]}]
      ]
     ];
   Return[output]
   ];

(* PN April 7, 2015 ->*)

pdfMCEnvelope[x_, q_, ipart_] := 
  Module[{nreps, values, output = {}}, 
   nreps = Length[pdfSetList[[ifamily]]];
   If[nreps < 2, 
    Print["Can't compute pdfMCEnvelope: the number of MC replicas = ", nreps, " < 2"];
    Return[0]];
   values = Table[pdfCTEQ[x, q, ipart, iset], {iset, nreps}];
   output = {Min[values],Max[values]};
   Return[output]
   ];

pdfMCEnvelope[f_] := 
  Module[{nreps, values, output = {}},
   If[ListQ[f], nreps = Length[f], 
    nreps = Length[pdfSetList[[ifamily]]]];
   
   If[nreps < 2, 
    Print["Can't compute pdfMCEnvelope: the number of MC replicas = ", nreps, " < 2"];
    Return[0]];
   If[ListQ[f],
    values = Sort[Table[f[[iset]], {iset, nreps}]],
    values = Sort[Table[f[iset], {iset, nreps}]]
    ];
   
    output = {Min[values],Max[values]};
    Return[output]
   ];

pdfFlavorTMP={
"tbar",
"bbar",
"cbar",
"sbar",
"dbar",
"ubar",
"gluon",
"up",
"down",
"strange",
"charm",
"bottom",
"top"
};

pdfFlavor[i_]:=pdfFlavorTMP[[i+7]]


(* ::Section:: *)
(*pdfCheck Functions:*)


pdfCheckXlist[iset_]:=pdfTableData[[ifamily,iset]][[1]] //Drop[#,1]&; (* The first x-point is for padding *)
pdfCheckQlist[iset_]:=pdfTableData[[ifamily,iset]][[2]];
pdfXmin[iset_] := pdfTableData[[ifamily,iset]][[1, 2]];


(* ::Section:: *)
(*End Package*)


End[];  (* End Private Context *)

EndPackage[]; (* End Package Context *)
