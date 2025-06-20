(* ::Package:: *)

Get["AES.wl"];

Print["load testAES"];


(* SUBBYTES STEP *)

(* Test funzione f *)
Print["Test f:"];
Print@TableForm@Table[{a,ff[a]}, {a,0,255}];

Quit[];

(* Test funzione g *)
Print["Test g:"];
(* Verifichiamo che det(A) \[EGrave] non nullo in F_2 *)
CirculantL[v_]:= Map[RotateLeft[v, #]&, Range[Length@v]-1];
A = CirculantL[{1,1,0,1}];
b = {1,1,0,1};
MatrixForm[A];
Det[A, Modulus->2]
(* Determiniamo b in modo che g non abbia punti fissi *)
b = {b0,b1,b2,b3};
x = {x0,x1,x2,x3};
Solve[A . x+b==x, x, Modulus->2];
(* Le componenti di b sono scelte in accordo con le condizioni date dalla riga precedente *)
Solve[A . x+b==x, x, Modulus->2]; (* Verifichiamo che b non crea punti fissi *)

(* Test funzione SRD *)
Print["Test SRD:"];
TableForm@Table[{a,SRD[a]}, {a,0,15}];

(* Test della funzione InvSRD *)
Print["Test InvSRD:"];
TableForm@Table[{a,SRD[a],InvSRD[SRD[a]]}, {a,0,15}];

(* TEST AES *)

(* Esempio di applicazione *)
Module[{X, Y, key, XX},
	X = StringToMatrix["0220023332111313"];
	key = StringToMatrix["F0D0DEADBEEFF00D"];
	Y = EncryptionAES[X, key];
	XX = DecryptionAES[Y, key];
	Print[StringForm["Enc_k(``) = ``", MatrixToString[X], MatrixToString[Y]]];
	Print[StringForm["Dec_k(``) = ``", MatrixToString[Y], MatrixToString[XX]]];
]
