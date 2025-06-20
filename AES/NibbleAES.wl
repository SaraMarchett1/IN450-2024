Get["finitefields.wl"];

Print["load AES"];


(* SUBBYTES STEP *)

(* Definizione della funzione f *)
ff[x_]:= If[x==0, 0, FongInverse[x]];

(* Definizione della funzione g *)
(* Costruiamo A come una matrice circolante generata dal vettore v=(1,1,0,1) *)
CirculantL[v_]:= Map[RotateLeft[v, #]&, Range[Length@v]-1];
A = CirculantL[{1,1,0,1}];
b = {1,1,0,1};
gg[x_]:= FromDigits[Mod[A . IntegerDigits[x, 2, Length@A]+b, 2], 2];

(* Definizione della funzione S_RD *)
SRD[x_]:= gg[ff[x]];

(* Definizione della funzione SubBytes *)
SubBytes[state_]:= Map[SRD, state, {2}]; (* Utilizziamo Map a livello 2 per applicare SRD a ogni componente della matrice state *)

(* Definizione dell'inversa della funzione SubBytes *)
Invgg[x_]:= FromDigits[Mod[A.(IntegerDigits[x, 2, Length@A]+b), 2], 2];
InvSRD[x_]:= ff[Invgg[x]];
InvSubBytes[state_]:= Map[InvSRD, state, {2}];


(* SHIFTROWS STEP *)

(* Definizione della funzione ShiftRows *)
ShiftRows[state_]:= MapThread[RotateLeft[#1, #2]&, {state, Range[0,3]}]; (* #1 si riferisce a state, #2 a Range[0,3] *)

(* Definizione dell'inversa della funzione ShiftRows *)
InvShiftRows[state_]:= MapThread[RotateRight[#1, #2]&, {state, Range[0,3]}];


(* MIXCOLUMNS STEP *)

(* Costruiamo MC come una matrice circolante generata dal vettore v=(2,3,1,1) *)
CirculantR[v_]:= Map[RotateRight[v, #]&, Range[Length@v]-1];
MC = CirculantR[{2,3,1,1}];
MatrixForm[MC];

(* Definizione della funzione MixColumns *)
MixColumns[state_]:= Inner[FieldMult, MC, state, FieldSum];

(* Costruiamo l'inversa di MC come una matrice circolante generata dal vettore v=(14,11,13,9) *)
InvMC = CirculantR[{14,11,13,9}];
MatrixForm[InvMC];

(* Definizione dell'inversa funzione MixColumns *)
InvMixColumns[state_]:= Inner[FieldMult, InvMC, state, FieldSum];


(* ADDKEY STEP *)

(* Definizione della funzione AddKey *)
AddKey[state_, roundKey_]:= BitXor[state, roundKey];

(* Definizione dell'inversa della funzione AddKey *)
InvAddKey[state_, roundKey_]:= BitXor[state, roundKey];


(* KEY SCHEDULE *)

fj[c_, j_]:= Module[{px, x, rc, out},
	px = x^4+x+1;
	rc = FromDigits[Reverse@CoefficientList[PolynomialRemainder[x^(j/4 - 1), px, x, Modulus->2], x, 4], 2];
	out = BitXor[Map[SRD, RotateLeft[c]], {rc,0,0,0}];
	Return[out];
];

LFSR[rkey_, j_]:= {
	Append[Drop[rkey, 1], If[Mod[j, 4]==0, BitXor[rkey[[1]], fj[rkey[[4]], j]], BitXor[rkey[[1]], rkey[[4]]]]],
	j+1
}

KeySchedule[rkey_, j_]:= Module[{out},
	out = NestList[LFSR[#[[1]], #[[2]]]&, {rkey, j}, 4];
	Return[out[[-1]]];
]

(* Definizione della funzione ExpKey *)
(* Input e output sono matrici 4x4 *)
ExpKey[key_]:= Map[Transpose[First[#]]&, NestList[KeySchedule[#[[1]], #[[2]]]&, {Transpose[key], 4}, 10]];


(* FUNZIONE DI CIFRATURA *)

(* Input e output sono matrici 4x4 *)
RoundFunction[state_, roundKey_] := AddKey[MixColumns[ShiftRows[SubBytes[state]]], roundKey];

EncryptionAES[plaintext_, key_]:= Module[{state, roundKey},
	roundKey = ExpKey[key]; (* è una lista di matrici *)
	state = AddKey[plaintext, roundKey[[1]]];
	state = Fold[RoundFunction, state, Drop[Drop[roundKey, 1], -1]];
	state = AddKey[ShiftRows[SubBytes[state]], roundKey[[-1]]];
	Return[state];
];

(* FUNZIONE DI DECIFRATURA*)

(* Input e output sono matrici 4x4 *)
InvRoundFunction[state_, roundKey_]:= InvSubBytes[InvShiftRows[InvMixColumns[InvAddKey[state, roundKey]]]];

DecryptionAES[ciphertext_, key_]:= Module[{state, roundKey},
	roundKey = ExpKey[key]; (* è una lista di matrici *)
	state = InvSubBytes[InvShiftRows[InvAddKey[ciphertext, roundKey[[-1]]]]];
	state = Fold[InvRoundFunction, state, Reverse[Drop[Drop[roundKey, 1], -1]]];
	state = InvAddKey[state, roundKey[[1]]];
	Return[state];
];


(* Funzioni di input e output *)
StringToMatrix[string_]:= Partition[IntegerDigits[FromDigits[string, 16], 16, 16], 4];
MatrixToString[matrix_]:= ToUpperCase[IntegerString[FromDigits[Flatten[matrix], 16], 16, 16]];
