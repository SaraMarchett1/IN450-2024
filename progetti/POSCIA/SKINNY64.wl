(* ::Package:: *)

(*SubCell*)
sbox={"c","6","9","0","1","a","2","b","3","8","5","d","4","e","7","f"};

SubCells[state_List]:=Module[{declist,SC},declist=FromDigits[#,16]&/@state;
SC=sbox[[declist+1]];
SC];

(*AddConstant*)
updaterc[rc_]:=Module[{LFSR,bin},bin=IntegerDigits[FromDigits[rc,16],2,6];
LFSR=Append[Drop[bin,1],BitXor[bin[[1]],bin[[2]],1]];
IntegerString[FromDigits[LFSR,2],16,2]];


updateci[rc_] := Module[{bin, matrix, ci},
  bin = IntegerDigits[FromDigits[rc, 16], 2, 6];
  matrix = {
    FromDigits[Take[bin, -4], 2],
    0, 0, 0,
    FromDigits[Take[bin, 2], 2],
    0, 0, 0,
    2, 0, 0, 0, 0, 0, 0, 0
  };
  ci = IntegerString[#, 16]& /@ matrix;
  ci
];

AddCostant[state_List,ci_List]:=Module[{d1,d2,sum},d1=FromDigits[#,16]&/@state;
d2=FromDigits[#,16]&/@ci;
sum=MapThread[BitXor,{d1,d2}];
IntegerString[#,16]&/@sum];



(*AddRoundTweakey*)
LFSR2[cell_]:=Module[{bin,update},bin=IntegerDigits[FromDigits[cell,16],2,4];
update=Append[Drop[bin,1],Mod[bin[[1]]+bin[[2]],2]];
IntegerString[FromDigits[update,2],16]];

LFSR3[cell_]:=Module[{bin,update},bin=IntegerDigits[FromDigits[cell,16],2,4];
update=Prepend[Drop[bin,-1],Mod[bin[[-1]]+bin[[1]],2]];
IntegerString[FromDigits[update, 2], 16, 1]];


updatetweakey[TK1_List,TK2_List,TK3_List]:=Module[{PT,PTK1,PTK2,PTK3,LFSRTK2,LFSRTK3},PT={9,15,8,13,10,14,12,11,0,1,2,3,4,5,6,7}+1;
PTK1=TK1[[#]]&/@PT;
PTK2=TK2[[#]]&/@PT;
PTK3=TK3[[#]]&/@PT;
LFSRTK2=Join[Map[LFSR2,PTK2[[1;;8]]],PTK2[[9;;]]];
LFSRTK3=Join[Map[LFSR3,PTK3[[1;;8]]],PTK3[[9;;]]];
{PTK1,LFSRTK2,LFSRTK3}];

AddRoundTweakey[state_List,TK1_List,TK2_List,TK3_List]:=Module[{d1,d2,d3,d4,sum},d1=FromDigits[#,16]&/@state[[1;;8]];
d2=FromDigits[#,16]&/@TK1[[1;;8]];
d3=FromDigits[#,16]&/@TK2[[1;;8]];
d4=FromDigits[#,16]&/@TK3[[1;;8]];
sum=MapThread[BitXor,{d1,d2,d3,d4}];
Join[IntegerString[#,16]&/@sum,state[[9;;]]]];

(*ShiftRows*)
ShiftRows[state_List]:=Module[{P,SR},P={0,1,2,3,7,4,5,6,10,11,8,9,13,14,15,12}+1;
SR=state[[#]]&/@P;
SR];

(*MixColumns*)
MixColumns[state_List]:=Module[{R1,R2,R3,R4,MC},R1={IntegerString[BitXor[BitXor[FromDigits[state[[1]],16],FromDigits[state[[9]],16]],FromDigits[state[[13]],16]],16],state[[1]],IntegerString[BitXor[FromDigits[state[[5]],16],FromDigits[state[[9]],16]],16],IntegerString[BitXor[FromDigits[state[[1]],16],FromDigits[state[[9]],16]],16]};
R2={IntegerString[BitXor[BitXor[FromDigits[state[[2]],16],FromDigits[state[[10]],16]],FromDigits[state[[14]],16]],16],state[[2]],IntegerString[BitXor[FromDigits[state[[6]],16],FromDigits[state[[10]],16]],16],IntegerString[BitXor[FromDigits[state[[2]],16],FromDigits[state[[10]],16]],16]};
R3={IntegerString[BitXor[BitXor[FromDigits[state[[3]],16],FromDigits[state[[11]],16]],FromDigits[state[[15]],16]],16],state[[3]],IntegerString[BitXor[FromDigits[state[[7]],16],FromDigits[state[[11]],16]],16],IntegerString[BitXor[FromDigits[state[[3]],16],FromDigits[state[[11]],16]],16]};
R4={IntegerString[BitXor[BitXor[FromDigits[state[[4]],16],FromDigits[state[[12]],16]],FromDigits[state[[16]],16]],16],state[[4]],IntegerString[BitXor[FromDigits[state[[8]],16],FromDigits[state[[12]],16]],16],IntegerString[BitXor[FromDigits[state[[4]],16],FromDigits[state[[12]],16]],16]};
MC=Transpose[{R1,R2,R3,R4}];
Flatten[MC]];


(*fRound*)
fRound[state_List,ci_List,TK1_List,TK2_List,TK3_List]:=Module[{s1,s2,s3,s4,s5},s1=SubCells[state];
s2=AddCostant[s1,ci];
s3=AddRoundTweakey[s2,TK1,TK2,TK3];
s4=ShiftRows[s3];
s5=MixColumns[s4];
s5];
(*FUNZIONI INVERTITE*)
MixColumnsInv[state_List]:=Module[{R1,R2,R3,R4,MC},R1={state[[5]],IntegerString[BitXor[BitXor[FromDigits[state[[5]],16],FromDigits[state[[9]],16]],FromDigits[state[[13]],16]],16],IntegerString[BitXor[FromDigits[state[[5]],16],FromDigits[state[[13]],16]],16],IntegerString[BitXor[FromDigits[state[[1]],16],FromDigits[state[[13]],16]],16]};
R2={state[[6]],IntegerString[BitXor[BitXor[FromDigits[state[[6]],16],FromDigits[state[[10]],16]],FromDigits[state[[14]],16]],16],IntegerString[BitXor[FromDigits[state[[6]],16],FromDigits[state[[14]],16]],16],IntegerString[BitXor[FromDigits[state[[2]],16],FromDigits[state[[14]],16]],16]};
R3={state[[7]],IntegerString[BitXor[BitXor[FromDigits[state[[7]],16],FromDigits[state[[11]],16]],FromDigits[state[[15]],16]],16],IntegerString[BitXor[FromDigits[state[[7]],16],FromDigits[state[[15]],16]],16],IntegerString[BitXor[FromDigits[state[[3]],16],FromDigits[state[[15]],16]],16]};
R4={state[[8]],IntegerString[BitXor[BitXor[FromDigits[state[[8]],16],FromDigits[state[[12]],16]],FromDigits[state[[16]],16]],16],IntegerString[BitXor[FromDigits[state[[8]],16],FromDigits[state[[16]],16]],16],IntegerString[BitXor[FromDigits[state[[4]],16],FromDigits[state[[16]],16]],16]};

MC=Transpose[{R1,R2,R3,R4}];
Flatten[MC]];

ShiftRowsInv[state_List]:=Module[{P,SR},P={0,1,2,3,5,6,7,4,10,11,8,9,15,12,13,14}+1;
SR=state[[#]]&/@P;
SR];

updatetweakeyInv[TK1_List,TK2_List,TK3_List]:=Module[{PT,PTK1,PTK2,PTK3,LFSRTK2,LFSRTK3},PT={8,9,10,11,12,13,14,15,2,0,4,7,6,3,5,1}+1;
LFSRTK2=Join[Map[LFSR3,TK2[[1;;8]]],TK2[[9;;]]];
LFSRTK3=Join[Map[LFSR2,TK3[[1;;8]]],TK3[[9;;]]];
PTK1=TK1[[#]]&/@PT;
PTK2=LFSRTK2[[#]]&/@PT;
PTK3=LFSRTK3[[#]]&/@PT;
{PTK1,PTK2,PTK3}];

updatercInv[rc_]:=Module[{LFSR,bin},bin=IntegerDigits[FromDigits[rc,16],2,6];
LFSR=Prepend[Drop[bin,-1],BitXor[bin[[1]],bin[[-1]],1]];
IntegerString[FromDigits[LFSR,2],16,2]];

sboxInv={"3","4" ,"6","8","c","a","1","e","9","2","5","7","0","b","d","f"};

SubCellsInv[state_List]:=Module[{declist,SC},declist=FromDigits[#,16]&/@state;
SC=sboxInv[[declist+1]];
SC];

fRoundInv[state_List,ci_List,TK1_List,TK2_List,TK3_List]:=Module[{s1,s2,s3,s4,s5},
s1 = MixColumnsInv[state];
s2 = ShiftRowsInv[s1];
s3 = AddRoundTweakey[s2,TK1,TK2,TK3];
s4 = AddCostant[s3,ci];
s5 =SubCellsInv[s4];
s5];

(*Ecrypt Function*)
SKINNY64Encrypt[plaintext_List,rounds_Integer,rc_,TK1_List,TK2_List,TK3_List]:=Module[{state= plaintext,rcup=rc,ciup,TK1UP = TK1,TK2UP =TK2,TK3UP = TK3},
Do[
rcup=updaterc[rcup];
ciup = updateci[rcup];
state = fRound[state,ciup,TK1UP,TK2UP,TK3UP];
{TK1UP,TK2UP,TK3UP}=updatetweakey[TK1UP,TK2UP,TK3UP];
,{r,1,rounds}];
state];

(*Decrypt Function*)
SKINNY64Decrypt[chipertext_List,rounds_Integer,lrc_,lTK1_List,lTK2_List,lTK3_List]:=Module[{state = chipertext,ciup,TK1UP = lTK1,TK2UP = lTK2,TK3UP = lTK3,rcup = lrc},
Do[
ciup = updateci[rcup];
{TK1UP,TK2UP,TK3UP}=updatetweakeyInv[TK1UP,TK2UP,TK3UP];
state = fRoundInv[state,ciup,TK1UP,TK2UP,TK3UP];
rcup=updatercInv[rcup];
,{r,1,rounds}];
state];









