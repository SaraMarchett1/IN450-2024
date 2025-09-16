(* ::Package:: *)

Import["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\SKINNY64.wl"];
dati=Get["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\ris_diff_trail.wl"];

diffIn1=dati["diffIn1"];
diffIn2=dati["diffIn2"];
diffOut1=dati["diffOut1"];
diffOut2=dati["diffOut2"];

(*Parametri*)
TK1={"9","e","b","9","3","6","4","0","d","0","8","8","d","a","6","3"};
TK2={"7","6","a","3","9","d","1","c","8","b","e","a","7","1","e","1"};
TK3=Table["0",{16}];
rc="00";
numPairs= 100000;



(*Differenze per il boomerang attack*)
quartetto={};
(*Ciclo che calcola la probabbolit\[AGrave] sperimentale della chiusura del boomerang*)
count=0;
Do[
P1=RandomInteger[{0,16^4-1},16]//IntegerString[#,16,1]&;
P2 = BitXor[Map[FromDigits[#,16]&,P1],Map[FromDigits[#,16]&,diffIn1]]//IntegerString[#,16]&;
Y1=SKINNY64Encrypt[P1,2,rc,TK1,TK2,TK3];
Y2=SKINNY64Encrypt[P2,2,rc,TK1,TK2,TK3];
C1=SKINNY64Encrypt[P1,5,rc,TK1,TK2,TK3];
C2=SKINNY64Encrypt[P2,5,rc,TK1,TK2,TK3];
C3 = BitXor[Map[FromDigits[#,16]&,C1],Map[FromDigits[#,16]&,diffIn2]]//IntegerString[#,16]&;
C4 = BitXor[Map[FromDigits[#,16]&,C2],Map[FromDigits[#,16]&,diffIn2]]//IntegerString[#,16]&;
P3 = SKINNY64Decrypt[C3,5,"1f",{"8","a","3","d","0","8","d","6","0","9","e","4","9","3","b","6"},{"7","9","9","c","e","1","4","1","1","d","a","4","e","6","b","5"},TK3];
P4 = SKINNY64Decrypt[C4,5,"1f",{"8","a","3","d","0","8","d","6","0","9","e","4","9","3","b","6"},{"7","9","9","c","e","1","4","1","1","d","a","4","e","6","b","5"},TK3];
Y3=SKINNY64Encrypt[P3,2,rc,TK1,TK2,TK3];
Y4=SKINNY64Encrypt[P4,2,rc,TK1,TK2,TK3];
X1= SKINNY64Decrypt[C1,1,"1f",{"8","a","3","d","0","8","d","6","0","9","e","4","9","3","b","6"},{"7","9","9","c","e","1","4","1","1","d","a","4","e","6","b","5"},TK3];
X2 = SKINNY64Decrypt[C2,1,"1f",{"8","a","3","d","0","8","d","6","0","9","e","4","9","3","b","6"},{"7","9","9","c","e","1","4","1","1","d","a","4","e","6","b","5"},TK3];
X3 = SKINNY64Decrypt[C3,1,"1f",{"8","a","3","d","0","8","d","6","0","9","e","4","9","3","b","6"},{"7","9","9","c","e","1","4","1","1","d","a","4","e","6","b","5"},TK3];
X4 = SKINNY64Decrypt[C4,1,"1f",{"8","a","3","d","0","8","d","6","0","9","e","4","9","3","b","6"},{"7","9","9","c","e","1","4","1","1","d","a","4","e","6","b","5"},TK3];
diff5=IntegerString[BitXor[Map[FromDigits[#,16]&,Y3],Map[FromDigits[#,16]&,Y4]],16,1];
diff1=IntegerString[BitXor[Map[FromDigits[#,16]&,Y1],Map[FromDigits[#,16]&,Y2]],16,1];
diff2=IntegerString[BitXor[Map[FromDigits[#,16]&,P3],Map[FromDigits[#,16]&,P4]],16,1];
diff3=IntegerString[BitXor[Map[FromDigits[#,16]&,X1],Map[FromDigits[#,16]&,X3]],16,1];
diff4=IntegerString[BitXor[Map[FromDigits[#,16]&,X2],Map[FromDigits[#,16]&,X4]],16,1];
If[diff1 ===diffOut1&&diff2===diffIn1&&diff3===diffOut2&&diff4===diffOut2&&diff5===diffOut1,count++;quartetto=Append[quartetto,{P1,P2,P3,P4}] ];
,{numPairs}];
Print["Il programma verte al calcolo della probabilit\[AGrave] sperimentale del boomerang rispetto a quanto ottenuto in precedenza"]
Print["Probabilit\[AGrave] di chiusa del Booomerang ",N[count/numPairs]];

dati2=<|
"quartetto"->quartetto
|>;


Export["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\ris_B_pro.wl",dati2];

