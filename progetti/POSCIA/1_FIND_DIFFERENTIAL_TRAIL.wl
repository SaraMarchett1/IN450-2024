(* ::Package:: *)

Import["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\SKINNY64.wl"]
(*S-BOX*)
sboxString = "c6901a2b385d4e7f";

cx=IntegerDigits[FromDigits[sboxString,16],16,16];
S[x_]:=cx[[x+1]];

(*TABELLE*)
DTT[delta_]:=(diffs=Table[(i2=BitXor[i,delta];BitXor[S[i],S[i2]]),{i,0,15}];
weights=Map[{#,Count[diffs,#]/16}&,Range[0,15]]);
DTT[]:=DTT[]=Map[Last,Map[DTT,Range[0,15]],{2}]
(*funzione in grado di selezione i differenziali per cui il valore della tabella e diverso da threshold*)
DTTSparse[delta_,threshold_]:=Module[{},weights=DTT[delta];
Map[{delta,#[[1]],#[[2]]}&,Select[weights,#[[2]]!=threshold&]]];

sboxInvString="3468ca1e92570bdf";
cxInv=IntegerDigits[FromDigits[sboxInvString,16],16,16];
SInv[x_]:=cxInv[[x+1]];

DTTInv[delta_]:=(diffs=Table[(i2=BitXor[i,delta];BitXor[SInv[i],SInv[i2]]),{i,0,15}];
weights=Map[{#,Count[diffs,#]/16}&,Range[0,15]]);
DTTInv[]:=DTTInv[]=Map[Last,Map[DTTInv,Range[0,15]],{2}]
DTTSparseInv[delta_,threshold_]:=Module[{},weights=DTTInv[delta];
Map[{delta,#[[1]],#[[2]]}&,Select[weights,#[[2]]!=threshold&]]];
(*TOOLS per il calcolo della probabilit\[AGrave] della chiusura del boomerang*)
DDT[a_, b_] := Count[Table[BitXor[S[BitXor[x, a]], S[x]], {x, 0, 15}], b];
DDTtab = Select[Flatten[Table[{i, j, DDT[i, j]}, {i, 1, 15}, {j, 1, 15}], 1],#[[3]]>=4&];

BCT[a_,b_]:=Count[Table[BitXor[SInv[BitXor[S[x],b]],SInv[BitXor[S[BitXor[x,a]],b]]],{x,0,15}],a];
BCTtab = Select[Flatten[Table[{i, j, BCT[i, j]}, {i, 1, 15}, {j, 1, 15}], 1],#[[3]]==16&];
UBCT[a_, c_, b_] := Count[
  Table[
    If[
      BitXor[S[x], S[BitXor[x, a]]] == c && 
      BitXor[SInv[BitXor[S[x], b]], SInv[BitXor[S[BitXor[x, a]], b]]] == a, 
      1, 0
    ],
    {x, 0, 15}
  ], 1
];

UBCTtab = Select[ Flatten[Table[{i, j, z, UBCT[i, j, z]}, {i, 1, 15}, {j, 1, 15}, {z, 1, 15}], 2],#[[4]]>=4&];

LBCT[a_, d_, b_] := Count[
  Table[
    If[
      BitXor[S[x], S[BitXor[x, d]]] == b && 
      BitXor[SInv[BitXor[S[x], b]], SInv[BitXor[S[BitXor[x, a]], b]]] == a, 
      1, 0
    ],
    {x, 0, 15}
  ], 1
];

LBCTtab = Select[ Flatten[Table[{i, j, z, LBCT[i, j, z]}, {i, 1, 15}, {j, 1, 15}, {z, 1, 15}], 2],#[[4]]>=4&];

EBCT[a_, c_, d_, b_] := Count[
  Table[
    If[
      BitXor[S[x], S[BitXor[x, a]]] == c &&
      BitXor[S[x], S[BitXor[x, d]]] == b &&
      BitXor[SInv[BitXor[S[x], b]], SInv[BitXor[S[BitXor[x, a]], b]]] == a,
      1,
      0
    ],
    {x, 0, 15}
  ], 1
];

EBCTtab = Select[Flatten[Table[{i, j, z, t, EBCT[i, j, z, t]}, {i, 1, 15}, {j, 1, 15}, {z, 1, 15}, {t, 1, 15}],3],#[[5]]>=4&];




ClearAll[DDT2,isTable,isActiveUp,isActiveL,isFreeXUp,isFreeXL,isFreeSBUp,isFreeSBL,z1,z2,z3,z4,t1,t2,t3,t4,t5,t6,t7,cons3,objective,sol,cons1,cons2]




Print["Il programma verte nella determinazione dei trail pi\[UGrave] vantaggiosi per l'upper e la lower part in grado di massimizzare la probabilit\[AGrave] della chiusura del boomerang"]

Print["STEP 1: riscrittura del problema in uno di tipo MILP e risoluzione"]

(*STEP 1 MILP MODEL:il problema viene riscritto sottoforma di problema milp e risolto tramite Liner Optimization*)
nroundup=2;
nroundm=4;
nroundl=5;
(*Definizioni delle variabili:vendono defite delle varaibili binarie che stanno ad indicare le celle attive, controlllate o libere e ad indicare quali tools devono essere utilizzati per calcolo della probabilit\[AGrave] del boomerang*)
vars=Flatten[Table[{isActiveUp[i,r],isActiveL[i,r],isFreeXUp[i,r],isFreeXL[i,r],isFreeSBUp[i,r],isFreeSBL[i,r],z1[i,r],z2[i,r],t1[i,r],t2[i,r],t3[i,r],t4[i,r],t5[i,r],DDT2[i,r],isTable[i,r]},{i,1,16},{r,0,nroundl}]];

(*Vincoli sull'upper trail, permettono di mantenere controllate le variabili nell'upper trail e tengono in consideraione della parte lineare del cifrato per la definizioni delle varibili delle celle attive nei vari round*)
cons1up=Flatten[Table[{
isFreeXUp[i,r]==0,isFreeSBUp[i,r]==0,isFreeXL[i,r]==1,isFreeSBL[i,r]==1,z1[i,r]==0,z2[i,r]==0,t1[i,r]==0,t2[i,r]==0,t3[i,r]==0,t4[i,r]==0,t5[i,r]==0,DDT2[i,r]==0,isTable[i,r]==isActiveUp[i,r]},{i,1,16},{r,0,nroundup}]];

cons2up=Flatten[Table[{
isActiveUp[1,r]>=isActiveUp[1,r-1],isActiveUp[1,r]>=isActiveUp[11,r-1],isActiveUp[1,r]>=isActiveUp[14,r-1],isActiveUp[1,r]<=isActiveUp[1,r-1]+isActiveUp[11,r-1]+isActiveUp[14,r-1],
isActiveUp[5,r]==isActiveUp[1,r-1],
isActiveUp[9,r]>=isActiveUp[8,r-1],isActiveUp[9,r]>=isActiveUp[11,r-1],isActiveUp[9,r]<=isActiveUp[8,r-1]+isActiveUp[11,r-1],
isActiveUp[13,r]>=isActiveUp[1,r-1],isActiveUp[13,r]>=isActiveUp[11,r-1],isActiveUp[13,r]<=isActiveUp[1,r-1]+isActiveUp[11,r-1],
isActiveUp[2,r]>=isActiveUp[2,r-1],isActiveUp[2,r]>=isActiveUp[12,r-1],isActiveUp[2,r]>=isActiveUp[15,r-1],isActiveUp[2,r]<=isActiveUp[2,r-1]+isActiveUp[12,r-1]+isActiveUp[15,r-1],
isActiveUp[6,r]==isActiveUp[2,r-1],
isActiveUp[10,r]>=isActiveUp[5,r-1],isActiveUp[10,r]>=isActiveUp[12,r-1],isActiveUp[10,r]<=isActiveUp[5,r-1]+isActiveUp[12,r-1],
isActiveUp[14,r]>=isActiveUp[2,r-1],isActiveUp[14,r]>=isActiveUp[12,r-1],isActiveUp[14,r]<=isActiveUp[2,r-1]+isActiveUp[12,r-1],
isActiveUp[3,r]>=isActiveUp[3,r-1],isActiveUp[3,r]>=isActiveUp[9,r-1],isActiveUp[3,r]>=isActiveUp[16,r-1],isActiveUp[3,r]<=isActiveUp[3,r-1]+isActiveUp[9,r-1]+isActiveUp[16,r-1],
isActiveUp[7,r]==isActiveUp[3,r-1],
isActiveUp[11,r]>=isActiveUp[6,r-1],isActiveUp[11,r]>=isActiveUp[9,r-1],isActiveUp[11,r]<=isActiveUp[6,r-1]+isActiveUp[9,r-1],
isActiveUp[15,r]>=isActiveUp[3,r-1],isActiveUp[15,r]>=isActiveUp[9,r-1],isActiveUp[15,r]<=isActiveUp[3,r-1]+isActiveUp[9,r-1],
isActiveUp[4,r]>=isActiveUp[4,r-1],isActiveUp[4,r]>=isActiveUp[10,r-1],isActiveUp[4,r]>=isActiveUp[13,r-1],isActiveUp[4,r]<=isActiveUp[4,r-1]+isActiveUp[10,r-1]+isActiveUp[13,r-1],
isActiveUp[8,r]==isActiveUp[4,r-1],
isActiveUp[12,r]>=isActiveUp[7,r-1],isActiveUp[12,r]>=isActiveUp[10,r-1],isActiveUp[12,r]<=isActiveUp[7,r-1]+isActiveUp[10,r-1],
isActiveUp[16,r]>=isActiveUp[4,r-1],isActiveUp[16,r]>=isActiveUp[10,r-1],isActiveUp[16,r]<=isActiveUp[4,r-1]+isActiveUp[10,r-1]
}
,{i,1,16},{r,1,nroundl}]];
(*vincolo che forza l'accensione di almeno un niddle sullo stato iniziale *)
cons3up=Total[Flatten[Table[isActiveUp[i,0],{i,1,16}]]]>=1;

(*Vinvoli sulla middle part: consendo una corretta definizione delle celle libere o controllate durante i round della midlle part, inoltre permettono l'utilizzo di tools corretti secondo specifiche condizionoi*)
cons1=Flatten[Table[{isFreeSBUp[i,r]>=isFreeXUp[i,r],isFreeXL[i,r]>=isFreeSBL[i,r],isActiveUp[i,r-1]>=isFreeSBUp[i,r], isActiveL[i,r-1]>=isFreeXL[i,r],isFreeSBUp[i,r]+isFreeSBL[i,r]<=1,isFreeXUp[i,r]+isFreeXL[i,r]<=1,
z1[i,r]<=isActiveUp[i,r-1],
z1[i,r]<=1-isFreeXUp[i,r],
z1[i,r]<=1-isFreeSBUp[i,r],
z1[i,r]<=isFreeSBL[i,r],
z1[i,r]<=isFreeXL[i,r],
z1[i,r]>=isActiveUp[i,r-1]-isFreeXUp[i,r]-isFreeSBUp[i,r]+isFreeSBL[i,r]+isFreeXL[i,r]-2,
z2[i,r]<=isActiveL[i,r-1],
z2[i,r]<=1-isFreeSBL[i,r],
z2[i,r]<=1-isFreeXL[i,r],
z2[i,r]<=isFreeSBUp[i,r],
z2[i,r]<=isFreeXUp[i,r],
z2[i,r]>=isActiveL[i,r-1]-isFreeSBL[i,r]-isFreeXL[i,r]+isFreeSBUp[i,r]+isFreeXUp[i,r]-2,
DDT2[i,r]==z1[i,r]+z2[i,r],
t1[i,r]<=isActiveUp[i,r-1],
t1[i,r]<=1-isFreeXUp[i,r],
t1[i,r]<=1-isFreeSBUp[i,r],
t1[i,r]<=1-isActiveL[i,r-1],
t1[i,r]>=isActiveUp[i,r-1]-isFreeXUp[i,r]-isFreeSBUp[i,r]-isActiveL[i,r-1],
t2[i,r]<=isActiveL[i,r-1],
t2[i,r]<=1-isFreeSBL[i,r],
t2[i,r]<=1-isFreeXL[i,r],
t2[i,r]<=1-isActiveUp[i,r-1],
t2[i,r]>=isActiveL[i,r-1]-isFreeSBL[i,r]-isFreeXL[i,r]-isActiveUp[i,r-1],
t3[i,r]<=isActiveUp[i,r-1],
t3[i,r]<=1-isFreeXUp[i,r],
t3[i,r]<=1-isFreeSBUp[i,r],
t3[i,r]<=1-isFreeSBL[i,r],
t3[i,r]<=isFreeXL[i,r],
t3[i,r]>=isActiveUp[i,r-1]-isFreeXUp[i,r]-isFreeSBUp[i,r]+isFreeXL[i,r]-isFreeSBL[i,r]-1,
t4[i,r]<=1-isFreeXUp[i,r],
t4[i,r]<=isFreeSBUp[i,r],
t4[i,r]<=1-isFreeXL[i,r],
t4[i,r]<=1-isFreeSBL[i,r],
t4[i,r]<=isActiveL[i,r-1],
t4[i,r]>=isActiveL[i,r-1]-isFreeXUp[i,r]+isFreeSBUp[i,r]-isFreeXL[i,r]-isFreeSBL[i,r]-1,
t5[i,r]<=isActiveUp[i,r-1],
t5[i,r]<=isActiveL[i,r-1],
t5[i,r]<=1-isFreeXUp[i,r],
t5[i,r]<=1-isFreeSBUp[i,r],
t5[i,r]<=1-isFreeXL[i,r],
t5[i,r]<=1-isFreeSBL[i,r],
t5[i,r]>=isActiveUp[i,r-1]+isActiveL[i,r-1]-isFreeXUp[i,r]-isFreeSBUp[i,r]-isFreeXL[i,r]-isFreeSBL[i,r]-1,
isTable[i,r]==t1[i,r]+t2[i,r]+t3[i,r]+t4[i,r]+t5[i,r]
},{i,1,16},{r,nroundup+1,nroundm}]];

(*Vincoli dovuti alla parte linere del cifrato*)
cons2=Flatten[Table[{
isFreeXUp[1,r]>=isFreeSBUp[1,r-1],isFreeXUp[1,r]>=isFreeSBUp[11,r-1],isFreeXUp[1,r]>=isFreeSBUp[14,r-1],isFreeXUp[1,r]<=isFreeSBUp[1,r-1]+isFreeSBUp[11,r-1]+isFreeSBUp[14,r-1],
isFreeXUp[5,r]==isFreeSBUp[1,r-1],
isFreeXUp[9,r]>=isFreeSBUp[8,r-1],isFreeXUp[9,r]>=isFreeSBUp[11,r-1],isFreeXUp[9,r]<=isFreeSBUp[8,r-1]+isFreeSBUp[11,r-1],isFreeXUp[13,r]>=isFreeSBUp[1,r-1],isFreeXUp[13,r]>=isFreeSBUp[11,r-1],isFreeXUp[13,r]<=isFreeSBUp[1,r-1]+isFreeSBUp[11,r-1],isFreeXUp[2,r]>=isFreeSBUp[2,r-1],isFreeXUp[2,r]>=isFreeSBUp[12,r-1],isFreeXUp[2,r]>=isFreeSBUp[15,r-1],isFreeXUp[2,r]<=isFreeSBUp[2,r-1]+isFreeSBUp[12,r-1]+isFreeSBUp[15,r-1],
isFreeXUp[6,r]==isFreeSBUp[2,r-1],
isFreeXUp[10,r]>=isFreeSBUp[5,r-1],isFreeXUp[10,r]>=isFreeSBUp[12,r-1],isFreeXUp[10,r]<=isFreeSBUp[5,r-1]+isFreeSBUp[12,r-1],isFreeXUp[14,r]>=isFreeSBUp[2,r-1],isFreeXUp[14,r]>=isFreeSBUp[12,r-1],isFreeXUp[14,r]<=isFreeSBUp[2,r-1]+isFreeSBUp[12,r-1],isFreeXUp[3,r]>=isFreeSBUp[3,r-1],isFreeXUp[3,r]>=isFreeSBUp[9,r-1],isFreeXUp[3,r]>=isFreeSBUp[16,r-1],isFreeXUp[3,r]<=isFreeSBUp[3,r-1]+isFreeSBUp[9,r-1]+isFreeSBUp[16,r-1],
isFreeXUp[7,r]==isFreeSBUp[3,r-1],
isFreeXUp[11,r]>=isFreeSBUp[6,r-1],isFreeXUp[11,r]>=isFreeSBUp[9,r-1],isFreeXUp[11,r]<=isFreeSBUp[6,r-1]+isFreeSBUp[9,r-1],isFreeXUp[15,r]>=isFreeSBUp[3,r-1],isFreeXUp[15,r]>=isFreeSBUp[9,r-1],isFreeXUp[15,r]<=isFreeSBUp[3,r-1]+isFreeSBUp[9,r-1],isFreeXUp[4,r]>=isFreeSBUp[4,r-1],isFreeXUp[4,r]>=isFreeSBUp[10,r-1],isFreeXUp[4,r]>=isFreeSBUp[13,r-1],isFreeXUp[4,r]<=isFreeSBUp[4,r-1]+isFreeSBUp[10,r-1]+isFreeSBUp[13,r-1],
isFreeXUp[8,r]==isFreeSBUp[4,r-1],
isFreeXUp[12,r]>=isFreeSBUp[7,r-1],isFreeXUp[12,r]>=isFreeSBUp[10,r-1],isFreeXUp[12,r]<=isFreeSBUp[7,r-1]+isFreeSBUp[10,r-1],isFreeXUp[16,r]>=isFreeSBUp[4,r-1],isFreeXUp[16,r]>=isFreeSBUp[10,r-1],isFreeXUp[16,r]<=isFreeSBUp[4,r-1]+isFreeSBUp[10,r-1]},
{r,nroundup+1,nroundm} ]];

cons3=Flatten[Table[{
isFreeSBL[1,r-1]==isFreeXL[5,r],
isFreeSBL[5,r-1]>=isFreeXL[6,r],isFreeSBL[5,r-1]>=isFreeXL[10,r],isFreeSBL[5,r-1]>=isFreeXL[14,r],isFreeSBL[5,r-1]<=isFreeXL[6,r]+isFreeXL[10,r]+isFreeXL[14,r],
isFreeSBL[9,r-1]>=isFreeXL[7,r],isFreeSBL[9,r-1]>=isFreeXL[15,r],isFreeSBL[9,r-1]<=isFreeXL[7,r]+isFreeXL[15,r],isFreeSBL[13,r-1]>=isFreeXL[4,r],isFreeSBL[13,r-1]>=isFreeXL[16,r],isFreeSBL[13,r-1]<=isFreeXL[4,r]+isFreeXL[16,r],isFreeSBL[2,r-1]==isFreeXL[6,r],
isFreeSBL[6,r-1]>=isFreeXL[7,r],isFreeSBL[6,r-1]>=isFreeXL[11,r],isFreeSBL[6,r-1]>=isFreeXL[15,r],isFreeSBL[6,r-1]<=isFreeXL[7,r]+isFreeXL[11,r]+isFreeXL[15,r],
isFreeSBL[10,r-1]>=isFreeXL[8,r],isFreeSBL[10,r-1]>=isFreeXL[16,r],isFreeSBL[10,r-1]<=isFreeXL[8,r]+isFreeXL[16,r],isFreeSBL[14,r-1]>=isFreeXL[1,r],isFreeSBL[14,r-1]>=isFreeXL[13,r],isFreeSBL[14,r-1]<=isFreeXL[1,r]+isFreeXL[13,r],isFreeSBL[3,r-1]==isFreeXL[7,r],
isFreeSBL[7,r-1]>=isFreeXL[8,r],isFreeSBL[7,r-1]>=isFreeXL[12,r],isFreeSBL[7,r-1]>=isFreeXL[16,r],isFreeSBL[7,r-1]<=isFreeXL[8,r]+isFreeXL[12,r]+isFreeXL[16,r],
isFreeSBL[11,r-1]>=isFreeXL[5,r],isFreeSBL[11,r-1]>=isFreeXL[13,r],isFreeSBL[11,r-1]<=isFreeXL[5,r]+isFreeXL[13,r],isFreeSBL[15,r-1]>=isFreeXL[2,r],isFreeSBL[15,r-1]>=isFreeXL[14,r],isFreeSBL[15,r-1]<=isFreeXL[2,r]+isFreeXL[14,r],isFreeSBL[4,r-1]==isFreeXL[8,r],
isFreeSBL[8,r-1]>=isFreeXL[5,r],isFreeSBL[8,r-1]>=isFreeXL[9,r],isFreeSBL[8,r-1]>=isFreeXL[13,r],isFreeSBL[8,r-1]<=isFreeXL[5,r]+isFreeXL[9,r]+isFreeXL[13,r],
isFreeSBL[12,r-1]>=isFreeXL[6,r],isFreeSBL[12,r-1]>=isFreeXL[14,r],isFreeSBL[12,r-1]<=isFreeXL[6,r]+isFreeXL[14,r],isFreeSBL[16,r-1]>=isFreeXL[3,r],isFreeSBL[16,r-1]>=isFreeXL[15,r],isFreeSBL[16,r-1]<=isFreeXL[3,r]+isFreeXL[15,r]},
{r,nroundup+2,nroundm+1} ]];

(*Vincoli sul lower trail. permetto di avre solo niddle controllati e tengono in considerazione della parte lineare del cifrato*)
cons1lo=Flatten[Table[{
isFreeXUp[i,r]==1,isFreeSBUp[i,r]==1,isFreeSBL[i,r]==0,isFreeXL[i,r]==0,z1[i,r]==0,z2[i,r]==0,t1[i,r]==0,t2[i,r]==0,t3[i,r]==0,t4[i,r]==0,t5[i,r]==0,DDT2[i,r]==0,isTable[i,r]==isActiveL[i,r]},{i,1,16},{r,nroundm+1,nroundl}]];

cons2lo=Flatten[Table[{
isActiveL[1,r-1]==isActiveL[5,r],isActiveL[5,r-1]>=isActiveL[6,r],isActiveL[5,r-1]>=isActiveL[10,r],
isActiveL[5,r-1]>=isActiveL[14,r],isActiveL[5,r-1]<=isActiveL[6,r]+isActiveL[10,r]+isActiveL[14,r],
isActiveL[9,r-1]>=isActiveL[7,r],isActiveL[9,r-1]>=isActiveL[15,r],isActiveL[9,r-1]<=isActiveL[7,r]+isActiveL[15,r],
isActiveL[13,r-1]>=isActiveL[4,r],isActiveL[13,r-1]>=isActiveL[16,r],isActiveL[13,r-1]<=isActiveL[4,r]+isActiveL[16,r],
isActiveL[2,r-1]==isActiveL[6,r],isActiveL[6,r-1]>=isActiveL[7,r],isActiveL[6,r-1]>=isActiveL[11,r],
isActiveL[6,r-1]>=isActiveL[15,r],isActiveL[6,r-1]<=isActiveL[7,r]+isActiveL[11,r]+isActiveL[15,r],
isActiveL[10,r-1]>=isActiveL[8,r],isActiveL[10,r-1]>=isActiveL[16,r],isActiveL[10,r-1]<=isActiveL[8,r]+isActiveL[16,r],
isActiveL[14,r-1]>=isActiveL[1,r],isActiveL[14,r-1]>=isActiveL[13,r],isActiveL[14,r-1]<=isActiveL[1,r]+isActiveL[13,r],
isActiveL[3,r-1]==isActiveL[7,r],isActiveL[7,r-1]>=isActiveL[8,r],isActiveL[7,r-1]>=isActiveL[12,r],
isActiveL[7,r-1]>=isActiveL[16,r],isActiveL[7,r-1]<=isActiveL[8,r]+isActiveL[12,r]+isActiveL[16,r],
isActiveL[11,r-1]>=isActiveL[5,r],isActiveL[11,r-1]>=isActiveL[13,r],isActiveL[11,r-1]<=isActiveL[5,r]+isActiveL[13,r],
isActiveL[15,r-1]>=isActiveL[2,r],isActiveL[15,r-1]>=isActiveL[14,r],isActiveL[15,r-1]<=isActiveL[2,r]+isActiveL[14,r],
isActiveL[4,r-1]==isActiveL[8,r],isActiveL[8,r-1]>=isActiveL[5,r],isActiveL[8,r-1]>=isActiveL[9,r],
isActiveL[8,r-1]>=isActiveL[13,r],isActiveL[8,r-1]<=isActiveL[5,r]+isActiveL[9,r]+isActiveL[13,r],
isActiveL[12,r-1]>=isActiveL[6,r],isActiveL[12,r-1]>=isActiveL[14,r],isActiveL[12,r-1]<=isActiveL[6,r]+isActiveL[14,r],
isActiveL[16,r-1]>=isActiveL[3,r],isActiveL[16,r-1]>=isActiveL[15,r],isActiveL[16,r-1]<=isActiveL[3,r]+isActiveL[15,r]
},{i,1,16},{r,1,nroundl}]];
(*forza l'accensione di almeno un niddle nello stato finale*)
cons3lo=Total[Flatten[Table[isActiveL[i,nroundl],{i,1,16}]]]>=1;

cons=Join[cons1up,cons2up,{cons3up},cons1,cons2,cons3,cons1lo,cons2lo,{cons3lo}];
(*funzione obbiettivo*)
objective=Total[Flatten[Table[4*DDT2[i,r]+2*isTable[i,r],{i,1,16},{r,0,nroundl}]]];
(*solutore*)
sol=LinearOptimization[objective,{cons,Thread[0<=vars<=1],vars\[Element]Integers   },vars];
(*Rislutati*)
Print["Soluzione del problema MILP"]
solList=Transpose[Partition[sol,15*(nroundl+1)]];
isActiveUp=Table[{Last/@solList[[i]]},{i,1,15*(nroundl+1),15}];
Print["Posizione attive per l'upper trail:",isActiveUp];
isActiveL=Table[{Last/@solList[[i]]},{i,2,15*(nroundl+1),15}];
Print["Posizione attive per il lower trail:",isActiveL];
isFreeXUp=Table[{Last/@solList[[i]]},{i,3,15*(nroundl+1),15}];
Print["Posizione libere sull'upper trail prima delle S-Box(1 per posizioni libere 0 per quelle controllate):",isFreeXUp];
isFreeXL=Table[{Last/@solList[[i]]},{i,4,15*(nroundl+1),15}];
Print["Posizione libere sul lower trail dopo la S-Box inversa(1 per posizioni libere 0 per quelle controllate):",isFreeXL];
isFreeSBUp=Table[{Last/@solList[[i]]},{i,5,15*(nroundl+1),15}];
Print["Posizione libere sull'upper trail dopo le S-Box(1 per posizioni libere 0 per quelle controllate):",isFreeSBUp];
isFreeSBL=Table[{Last/@solList[[i]]},{i,6,15*(nroundl+1),15}];
Print["Posizione libere sul lower trail prima  della S-Box inversa(1 per posizioni libere 0 per quelle controllate):",isFreeSBL];

z1=Table[{Last/@solList[[i]]},{i,7,15*(nroundl+1),15}];
z2=Table[{Last/@solList[[i]]},{i,8,15*(nroundl+1),15}];
t1=Table[{Last/@solList[[i]]},{i,9,15*(nroundl+1),15}];
t2=Table[{Last/@solList[[i]]},{i,10,15*(nroundl+1),15}];
t3=Table[{Last/@solList[[i]]},{i,11,15*(nroundl+1),15}];
t4=Table[{Last/@solList[[i]]},{i,12,15*(nroundl+1),15}];
t5=Table[{Last/@solList[[i]]},{i,13,15*(nroundl+1),15}];
DDT2=Table[{Last/@solList[[i]]},{i,14,15*(nroundl+1),15}];
Print["Posizioni in cui deve essere utulizzata il DDT^2 per il calcolo delle probabilit\[AGrave]",DDT2];
isTable=Table[{Last/@solList[[i]]},{i,15,15*(nroundl+1),15}];
Print["Posizione in cui vanno utilizzati tutti gli altri tools per il calcolo delle probabilit\[AGrave]",isTable];


(*STEP 2: determinazione dei trail pi\[UGrave] vantagggiosi per l'upper trail, viene construito un grafo di tutti i possibili cammini assegnando un peso secomdo tabella DDT, dopo di che viene trovato il cammino di peso minimo*)

Print["Step2: determinazione dei trail ottimali per l'upper part"]
Print["la strategia risolutiva consiste nell'andare a determinare i trail migliori per ogni differenziale di input possibile secondo quanto determinato nello Step 1"]
(*Funziona che costruisce tutti i possibili trail dato un differenziale inizle e aggiorna il grafo in input con tali risultati*)
GraphConstructor[delta_List,grafo_Graph]:=Module[{},
somma={};
G=grafo;
sboxattive=Flatten[Position[delta,x_/;x!="0"]];(*isolamento delle S-box attive*)
(*costruzione di tutti i possibili stati dopo la S-box*)
Do[
outsbox=DTTSparse[FromDigits[delta[[pos]],16],0];
matrix={};
Do[
Newdelta=  Table["0",16];
Newdelta[[pos]]=IntegerString[t,16];
prob=outsbox[[Position[outsbox[[All]],t][[1,1]],3]];
matrix = Append[matrix,{Newdelta,prob}];
,{t,Transpose[outsbox][[2]]}];
If[Length[somma]==0,somma=matrix,
risultati={};
Do[
Do [
A1= Transpose[matrix][[1]][[i]];
A2= Transpose[somma][[1]][[j]];
P= Times[Transpose[matrix][[2]][[i]],Transpose[somma][[2]][[j]]];
risultato =BitXor[Map[FromDigits[#,16]&,A1],Map[FromDigits[#,16]&,A2]]//IntegerString[#,16]&;
risultati=Append[risultati,{risultato,P}];
,{i,Length[matrix]}];
,{j,Length[somma]}];
somma = risultati];
,{pos,sboxattive}];
(*Aplicazione della parte lineare*)
SC=Map[ShiftRows[#]&,somma[[All,1]]];
MC=Map[MixColumns[#]&,SC];
archiEsistenti=EdgeList[G];
(*aggiornamento degli archi secondo i risultati ottenuti*)
pesiEsistenti=PropertyValue[{G,#},EdgeWeight]&/@archiEsistenti;
nuoviArchi=DirectedEdge[delta,#]&/@MC;
(*aggiornamento dei pesi secondo tabella DDT*)
nuoviPesi=Map[Function[x,-Log[2,x]],somma[[All,2]]];
(*GGiornamento del grafo*)
G=Graph[Join[archiEsistenti,nuoviArchi],EdgeWeight->Join[pesiEsistenti,nuoviPesi],DirectedEdges->True];
trail=MapThread[{delta,#1,#2}&,{MC,somma[[All,2]]}];
{G,trail}];

(*estrazione dei risultati dello step 1*)
pos=Flatten[Position[isActiveUp[[1]],1]][[2]];

trailmigliori={};
nround=2;
(*ciclo che crea tutti i possibili trail per l'upper part fissata la posizione attiva del differenziale di input e facendo varaiare questo niddle per tutti i valori possibili)*)
Do[
pesimigliori={};
delta=Table["0",16];
delta[[pos]]=IntegerString[i,16];
G=Graph[{},DirectedEdges->True];
{G1,TRAIL}=GraphConstructor[delta,G];
TrailCopia=TRAIL[[All,2]];
Do[T={};
Do[R=GraphConstructor[TrailCopia[[j]],G1];
T=Join[T,R[[2]]];
G1=R[[1]];,{j,Length[TrailCopia]}];
TRAIL=Append[TRAIL,T];
TrailCopia=T[[All,2]];,nround-1];
Besttrail=Select[DeleteDuplicates[FindShortestPath[G1,delta,#]&/@TrailCopia],Length[#]==nround+1&];
Do[
edges=Thread[DirectedEdge@@@Partition[Besttrail[[b]],2,1]];
pesi=PropertyValue[{G1,#},EdgeWeight]&/@edges;
p=Times@@Map[Function[x,2^(-x)],pesi];
pesimigliori=Append[pesimigliori,p];
,{b,1,Length[Besttrail]}];

posbest=Flatten[Position[pesimigliori,Max[pesimigliori]]];

Do[
trailmigliori=Append[trailmigliori,{{delta,Besttrail[[a]][[-1]]},Max[pesimigliori]}]
,{a,posbest}];

,{i,1,15}]

pmax=Max[Transpose[trailmigliori][[2]]];
trailmiglioriUP=Select[trailmigliori,#[[All]][[2]]==pmax&];

Print["Trail selezionati per l'upper part con relativa probabilit\[AGrave] teorica",trailmiglioriUP]


(*STEP 3: determinazione dei trail pi\[UGrave] vantagggiosi per il lowe trail, viene construito un grafo di tutti i possibili cammini assegnando un peso secomdo tabella DDT inversa, dopo di che viene trovato il cammino di peso minimo*)
(*Funziona che costruisce tutti i possibili trail dato un differenziale inizle e aggiorna il grafo in input con tali risultati*)

GraphConstructorInv[delta_List,grafo_Graph]:=Module[{},
somma={};
G=grafo;
MC=MixColumnsInv[delta];
SC=ShiftRowsInv[MC];
sboxattive=Flatten[Position[SC,x_/;x!="0"]];
Do[
outsbox=DTTSparseInv[FromDigits[SC[[pos]],16],0];
matrix={};
Do[
Newdelta=  Table["0",16];
Newdelta[[pos]]=IntegerString[t,16];
prob=outsbox[[Position[outsbox[[All]],t][[1,1]],3]];
matrix = Append[matrix,{Newdelta,prob}];
,{t,Transpose[outsbox][[2]]}];
If[Length[somma]==0,somma=matrix,
risultati={};
Do[
Do [
A1= Transpose[matrix][[1]][[i]];
A2= Transpose[somma][[1]][[j]];
P= Times[Transpose[matrix][[2]][[i]],Transpose[somma][[2]][[j]]];
risultato =BitXor[Map[FromDigits[#,16]&,A1],Map[FromDigits[#,16]&,A2]]//IntegerString[#,16]&;
risultati=Append[risultati,{risultato,P}];
,{i,Length[matrix]}];
,{j,Length[somma]}];
somma = risultati];
,{pos,sboxattive}];

archiEsistenti=EdgeList[G];
pesiEsistenti=PropertyValue[{G,#},EdgeWeight]&/@archiEsistenti;
nuoviArchi=DirectedEdge[delta,#]&/@somma[[All,1]];
nuoviPesi=Map[Function[x,-Log[2,x]],somma[[All,2]]];
G=Graph[Join[archiEsistenti,nuoviArchi],EdgeWeight->Join[pesiEsistenti,nuoviPesi],DirectedEdges->True];
trail=MapThread[{delta,#1,#2}&,{somma[[All,1]],somma[[All,2]]}];
{G,trail}];

trailmigliori={};
nround=1;

(*estrazione dei risultati allo Step1*)
pos=Flatten[Position[isActiveL[[-1]],1]][[2]];


(*ciclo per determinare i trail pi\[UGrave] vantaggiosi per il lower trail*)
Do[
pesimigliori={};
delta=Table["0",16];
delta[[pos]]=IntegerString[i,16];
G=Graph[{},DirectedEdges->True];
{G1,TRAIL}=GraphConstructorInv[delta,G];
TrailCopia=TRAIL[[All,2]];
Do[T={};
Do[R=GraphConstructor[TrailCopia[[j]],G1];
T=Join[T,R[[2]]];
G1=R[[1]];,{j,Length[TrailCopia]}];
TRAIL=Append[TRAIL,T];
TrailCopia=T[[All,2]];,nround-1];
Besttrail=Select[DeleteDuplicates[FindShortestPath[G1,delta,#]&/@TrailCopia],Length[#]==nround+1&];

Do[
edges=Thread[DirectedEdge@@@Partition[Besttrail[[b]],2,1]];
pesi=PropertyValue[{G1,#},EdgeWeight]&/@edges;
p=Times@@Map[Function[x,2^(-x)],pesi];
pesimigliori=Append[pesimigliori,p];
,{b,1,Length[Besttrail]}];
posbest=Flatten[Position[pesimigliori,Max[pesimigliori]]];

Do[
trailmigliori=Append[trailmigliori,{{delta,Besttrail[[a]][[-1]]},Max[pesimigliori]}]
,{a,posbest}];

,{i,1,15}];


pmax=Max[Transpose[trailmigliori][[2]]];
trailmiglioriL=Select[trailmigliori,#[[All]][[2]]==pmax&];
Print["Step3: determinazione dei trail ottimali per la lower part"]
Print["la strategia risolutiva consiste nell'andare a determinare i trail migliori per ogni differenziale di input possibile secondo quanto determinato nello Step 1"]
Print["Trail selezionati per la lower part con relativa probabilit\[AGrave] teorica",trailmiglioriL]


(*STEP3: i trail trovati vengono selezioni in moda da individuare quelli che generano una probabilit\[AGrave] di chiusura nella middle part massima*)
selectedTrailUp=DeleteDuplicates[trailmiglioriUP[[All,1,2]]];
(*questi due cicli vanno a selezionare i differenziali che secondo i tools generano prbabilit\[AGrave] massima*)
Do[
If[Flatten[t1[[nroundup+2]]][[i]]==1,
selectedTrail=Select[selectedTrailUp,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[DDTtab][[1]]]],#[[i]]]&];selectedTrailUp=selectedTrail];
If[Flatten[z1[[nroundup+2]]][[i]]==1,
selectedTrail=Select[selectedTrailUp,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[DDTtab][[1]]]],#[[i]]]&];selectedTrailUp=selectedTrail];
If[Flatten[isFreeXUp[[nroundup+2]]][[i]]==0&&Flatten[isFreeSBUp[[nroundup+2]]][[i]]==1&&Flatten[isFreeXL[[nroundup+2]]][[i]]==1&&Flatten[isFreeSBL[[nroundup+2]]][[i]]==0,
selectedTrail=Select[selectedTrailUp,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[BCTtab][[1]]]],#[[i]]]&];
selectedTrailUp=selectedTrail];
If[Flatten[t3[[nroundup+2]]][[i]]==1,
selectedTrail=Select[selectedTrailUp,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[UBCTtab][[1]]]],#[[i]]]&];
selectedTrailUp=selectedTrail];
If[Flatten[t4[[nroundup+2]]][[i]]==1,selectedTrail=Select[selectedTrailUp,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[LBCTtab][[1]]]],#[[i]]]&];
selectedTrailUp=selectedTrail];
If[Flatten[t5[[nroundup+2]]][[i]]==1,selectedTrail=Select[selectedTrailUp,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[EBCTtab][[1]]]],#[[i]]]&];selectedTrailUp=selectedTrail]
,{i,2,16}]



selectedTrailL=DeleteDuplicates[Map[ShiftRowsInv[MixColumnsInv[#]]&,trailmiglioriL[[All,1,2]]]];

Do[
If[Flatten[t2[[nroundm+1]]][[i]]==1,selectedTrail=Select[selectedTrailL,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[DDTtab][[2]]]],#[[i]]]&];selectedTrailL=selectedTrail];
If[Flatten[z2[[nroundm+1]]][[i]]==1,selectedTrail=Select[selectedTrailL,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[DDTtab][[2]]]],#[[i]]]&];selectedTrailL=selectedTrail];
If[Flatten[isFreeXUp[[nroundm+1]]][[i]]==0&&Flatten[isFreeSBUp[[nroundm+1]]][[i]]==1&&Flatten[isFreeXL[[nroundm+1]]][[i]]==1&&Flatten[isFreeSBL[[nroundm+1]]][[i]]==0,
selectedTrail=Select[selectedTrailL,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[BCTtab][[2]]]],#[[i]]]&];selectedTrailL=selectedTrail];
If[Flatten[t3[[nroundm+1]]][[i]]==1,selectedTrail=Select[selectedTrailL,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[UBCTtab][[3]]]],#[[i]]]&];
selectedTrailL=selectedTrail];
If[Flatten[t4[[nroundm+1]]][[i]]==1,selectedTrail=Select[selectedTrailL,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[LBCTtab][[3]]]],#[[i]]]&];selectedTrailL=selectedTrail];
If[Flatten[t5[[nroundm+1]]][[i]]==1,selectedTrail=Select[selectedTrailL,MemberQ[Map[IntegerString[#,16]&,DeleteDuplicates[Transpose[EBCTtab][[4]]]],#[[i]]]&];selectedTrailL=selectedTrail]
,{i,1,16}]


selectedTrailL=Map[MixColumns[ShiftRows[#]]&,selectedTrailL];

TK1={"9","e","b","9","3","6","4","0","d","0","8","8","d","a","6","3"};
TK2={"7","6","a","3","9","d","1","c","8","b","e","a","7","1","e","1"};
(*viene calcolata sperimentalmente la probabilit\[AGrave] della middle part*)
numPairs= 1000;
r=0;

Do[
Do[
count=0;
Do[

x1=RandomInteger[{0,16^4-1},16]//IntegerString[#,16,1]&;
x2= BitXor[Map[FromDigits[#,16]&,x1],Map[FromDigits[#,16]&,selectedTrailUp[[i]]]]//IntegerString[#,16]&;
EM1out=SKINNY64Encrypt[x1,2,"03", {"e","0","9","6","b","4","3","9","0","3","d","a","8","6","d","8"} , {"d","8","f","a","5","2","3","6","7","2","1","2","c","c","f","5"} ,{"0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"}];
EM2out=SKINNY64Encrypt[x2,2,"03", {"e","0","9","6","b","4","3","9","0","3","d","a","8","6","d","8"} , {"d","8","f","a","5","2","3","6","7","2","1","2","c","c","f","5"} ,{"0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"}];
y1=BitXor[Map[FromDigits[#,16]&,EM1out],Map[FromDigits[#,16]&,selectedTrailL[[j]]]]//IntegerString[#,16]&;
y2=BitXor[Map[FromDigits[#,16]&,EM2out],Map[FromDigits[#,16]&,selectedTrailL[[j]]]]//IntegerString[#,16]&;
EM1in=SKINNY64Decrypt[y1,2,"0f", {"0","9","e","4","9","3","b","6","3","8","0","6","d","d","8","a"}  ,  {"1","d","a","4","e","6","b","5","4","b","f","8","2","e","8","4"} ,{"0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"}];
EM2in=SKINNY64Decrypt[y2,2,"0f", {"0","9","e","4","9","3","b","6","3","8","0","6","d","d","8","a"}  ,  {"1","d","a","4","e","6","b","5","4","b","f","8","2","e","8","4"} ,{"0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"}];

If[IntegerString[BitXor[Map[FromDigits[#,16]&,EM1in],Map[FromDigits[#,16]&,EM2in]],16,1]===selectedTrailUp[[i]],count++];
,{numPairs}];
If[r<N[count/numPairs],r=N[count/numPairs];diffOut1=selectedTrailUp[[i]];diffOut2=selectedTrailL[[j]]];
,{i,Length[selectedTrailUp]}];
,{j,Length[selectedTrailL]}];

Print["I differenziali di input e output per la  middle part scelti sono:"];
Print[diffOut1];
Print[diffOut2];
Print["Con probabilit\[AGrave] di:",r];

diffIn1=Select[trailmiglioriUP,#[[1,2]]==diffOut1&][[1,1,1]];
diffIn2=Select[trailmiglioriL,#[[1,2]]==diffOut2&][[1,1,1]];
p=Select[trailmiglioriUP,#[[1,2]]==diffOut1&][[1,2]];
q=Select[trailmiglioriL,#[[1,2]]==diffOut2&][[1,2]];
P= Times[Power[p,2],Power[q,2],r];
Print["Step4: viene fatta una selezione sui differenziali trovati e successivamente viene calcolata la probabilit\[AGrave] sperimentale per la middle part"]
Print["I differenziali delezionati in grado di massimizzare la probabilit\[AGrave] del boomerang sono:"];
Print["Differenziale di input sull'upper part:",diffIn1];
Print["Differenziale di input sulla lower part:",diffIn2];
Print["Differenziale di output sull'upper part:",diffOut1];
Print["Differenziale di output sulla lower part:",diffOut2];
Print["Probabilit\[AGrave] stimata del Boomerang:",P]


(*Esportazione dei dati*)
dati=<|
"diffIn1"->diffIn1,
"diffIn2"->diffIn2,
"diffOut1"->diffOut1,
"diffOut2"->diffOut2
|>;

Export["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\ris_diff_trail.wl",dati];
