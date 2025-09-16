(* ::Package:: *)

Import["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\SKINNY64.wl"];
dati=Get["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\ris_diff_trail.wl"];
dati2=Get["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\ris_B_pro.wl"];

diffIn1=dati["diffIn1"];
diffIn2=dati["diffIn2"];
diffOut1=dati["diffOut1"];
diffOut2=dati["diffOut2"];

quartetto=dati2["quartetto"];

TK1={"9","e","b","9","3","6","4","0","d","0","8","8","d","a","6","3"};
TK2={"7","6","a","3","9","d","1","c","8","b","e","a","7","1","e","1"};
TK3=Table["0",{16}];
rc="00";
KEY=Table["0",32];



(*il key ricovery viene  fatto modificando un niddle alla volta la chiave e vengono poi osservate le probabilit\[AGrave] decifrando con le chiavi cos\[IGrave] ottenuti*) 
KeyProb={};
Do[
Do[
KeyGuess = {"9","6","0","3","e","b","9","4","8","a","3","d","0","8","d","6","a","b","2","d","5","7","c","9","7","9","9","c","e","1","4","1"};
niddleguess=IntegerString[a,16];
KeyGuess[[POS]]=niddleguess;
HiddenKey=Table["*",32];
HiddenKey[[POS]]=niddleguess;


n=0;
Do[
{G1,G2,G3,G4}=SKINNY64Encrypt[#,6,"00",TK1,TK2,TK3]&/@quartetto[[i]];
{H1,H2,H3,H4}=SKINNY64Decrypt[#,1,"3e",KeyGuess[[1;;16]],KeyGuess[[17;;32]],Table["0",16]]&/@{G1,G2,G3,G4};
{I1,I2,I3,I4}=SKINNY64Decrypt[#,2,"3e",KeyGuess[[1;;16]],KeyGuess[[17;;32]],Table["0",16]]&/@{G1,G2,G3,G4};
{L1,L2,L3,L4}=SKINNY64Decrypt[#,4,"3e",KeyGuess[[1;;16]],KeyGuess[[17;;32]],Table["0",16]]&/@{G1,G2,G3,G4};
condizione1 = IntegerString[BitXor[Map[FromDigits[#,16]&,H1],Map[FromDigits[#,16]&,H3]],16,1];
condizione2 = IntegerString[BitXor[Map[FromDigits[#,16]&,H2],Map[FromDigits[#,16]&,H4]],16,1];
condizione3 = IntegerString[BitXor[Map[FromDigits[#,16]&,I1],Map[FromDigits[#,16]&,I3]],16,1];
condizione4 = IntegerString[BitXor[Map[FromDigits[#,16]&,I2],Map[FromDigits[#,16]&,I4]],16,1];
condizione5 = IntegerString[BitXor[Map[FromDigits[#,16]&,L1],Map[FromDigits[#,16]&,L2]],16,1];
condizione6 = IntegerString[BitXor[Map[FromDigits[#,16]&,L3],Map[FromDigits[#,16]&,L4]],16,1];

If[condizione1===diffIn2&&condizione2===diffIn2&&condizione3===diffOut2&&condizione4===diffOut2&&condizione5===diffOut1&&condizione6===diffOut1,n++];
,{i,Length[quartetto]}];
Print[HiddenKey,N[n/Length[quartetto]]];
If[N[n/Length[quartetto]]==1,KEY[[POS]]=IntegerString[a,16]];
,{a,0,15}];
,{POS,1,32 }];
Print["Il programma consiste nel key ricovery, nella chiave di round orriginale viene modificato un niddle alla volta, vengono poi osservate le probabilit\[AGrave] decifrando con le chiavi cos\[IGrave] ottenute"];
Print["La chiave individuata \[EGrave]:",KEY]












