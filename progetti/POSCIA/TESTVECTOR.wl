(* ::Package:: *)

Import["C:\\Users\\giada\\Desktop\\UNI\\AL_CR\\Progetto\\UTILI\\POSCIA\\SKINNY64.wl"];

(*TEST VECTOR*)
(*esempio skinny64-64*)
Print["Esempio per lo skinny64-64"];
TK1={"f","5","2","6","9","8","2","6","f","c","6","8","1","2","3","8"};
TK2=Table["0",{16}];
TK3=Table["0",{16}];
plaintext={"0","6","0","3","4","f","9","5","7","7","2","4","d","1","9","d"};
rc="00";
Print["Il Plaintext \[EGrave]:",plaintext];
ciphertext=SKINNY64Encrypt[plaintext,32,rc,TK1,TK2,TK3];
Print["Il Ciphertext \[EGrave]:",ciphertext];

lrc="38";
lTK1={"f","5","2","6","9","8","2","6","f","c","6","8","1","2","3","8"};

plaintext=SKINNY64Decrypt[ciphertext,32,lrc,lTK1,TK2,TK3];
Print["Decifrando si ottiene:",plaintext];
(*esempio skinny64-128*)
Print["Esempio per lo skinny64-128"];
rc="00";
TK1={"9","e","b","9","3","6","4","0","d","0","8","8","d","a","6","3"};
TK2={"7","6","a","3","9","d","1","c","8","b","e","a","7","1","e","1"};
TK3=Table["0",{16}];
plaintext={"c","f","1","6","c","f","e","8","f","d","0","f","9","8","a","a"};
Print["Il Plaintext \[EGrave]:",plaintext];

ciphertext=SKINNY64Encrypt[plaintext,36,rc,TK1,TK2,TK3];
Print["Il Ciphertext \[EGrave]:",ciphertext];

lrc="0d";
 lTK1={"0","9","e","4","9","3","b","6","3","8","0","6","d","d","8","a"};
 lTK2 = {"2","a","5","9","c","d","7","b","9","7","e","1","4","c","1","9"};
plaintext=SKINNY64Decrypt[ciphertext,36,lrc,lTK1,lTK2,TK3] ;
Print["Decifrando si ottiene:",plaintext];
(*esempio skinny64-192*)
Print["Esempio per lo skinny64-192"];
rc="00";
TK1={"e","d","0","0","c","8","5","b","1","2","0","d","6","8","6","1"};
TK2={"8","7","5","3","e","2","4","b","f","d","9","0","8","f","6","0"};
TK3={"b","2","d","b","b","4","1","b","4","2","2","d","f","c","d","0"};
plaintext={"5","3","0","c","6","1","d","3","5","e","8","6","6","3","c","3"};
Print["Il Plaintext \[EGrave]:",plaintext];
ciphertext=SKINNY64Encrypt[plaintext,40,rc,TK1,TK2,TK3];
Print["Il Ciphertext \[EGrave]:",ciphertext];

lrc="1a";
lTK1={"8","5","0","0","b","e","d","c","8","6","d","0","1","1","2","6"};
lTK2= {"d","a","b","c","8","3","1","4","2","7","0","5","0","2","f","3"};
lTK3={"e","7","3","2","3","3","f","3","5","2","2","f","0","e","f","d"};
plaintext=SKINNY64Decrypt[ciphertext,40,lrc,lTK1,lTK2,lTK3] ;
Print["Decifrando si ottiene:",plaintext];




