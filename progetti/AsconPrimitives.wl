(* ::Package:: *)

(* ::Subtitle:: *)
(*ASCON PRIMITIVES AND UTILITIES*)


(*Parameters*)
wordBits = 64;
stateWords = 5;

(*Utilities*)
BitRotateRight64[n_, k_] :=
  BitAnd[BitOr[BitShiftRight[n, k], BitShiftLeft[n, 64 - k]], 2^64 - 1];

GetBits[state_, c_] := Module[{bits},
  bits = Map[BitGet[#, c] &, state];
  FromDigits[Reverse[bits], 2]
];

RebuildState[columns_List] := Module[{res},
  res = Table[
    FromDigits[Reverse[columns[[All, j]]], 2],
    {j, 1, 5}
  ];
  Map[BitAnd[#, 2^64 - 1] &, res]
];

RandomState[] := Table[RandomInteger[{0, 2^64 - 1}], {stateWords}];

XorState[a_List, b_List] := MapThread[BitXor, {a, b}];

(*Ascon S-box and inverse lists*)
sbox = {4, 11, 31, 20, 26, 21, 9, 2, 27, 5, 8, 18, 29, 3, 6, 28, 30, 19, 7, 14, 0, 13, 17, 24, 16, 12, 1, 25, 22, 10, 15, 23};
sboxInv = {20, 26, 7, 13, 0, 9, 14, 18, 10, 6, 29, 1, 25, 21, 19, 30, 24, 22, 11, 17, 3, 5, 28, 31, 23, 27, 4, 8, 15, 12, 16, 2};

(*S-box function and his inverse*)
AsconSbox[state_] := Module[{columns},
  columns = Table[IntegerDigits[sbox[[GetBits[state, i] + 1]], 2, 5], {i, 0, 63}];
  RebuildState[columns]
];

AsconSboxInv[state_] := Module[{columns, sub},
  columns = Table[Map[BitGet[#, i] &, state], {i, 0, 63}];
  sub = Map[IntegerDigits[sboxInv[[FromDigits[#, 2] + 1]], 2, 5] &, columns];
  newstate = Table[FromDigits[sub[[All, j]], 2], {j, 1, 5}];
  newstate
];

(*Ascon Linear layer and his inverse*)
AsconLinear[state_]:=Module[{x0,x1,x2,x3,x4},{x0,x1,x2,x3,x4}=state;
x0=BitXor[x0,BitXor[BitRotateRight64[x0,19],BitRotateRight64[x0,28]]]//BitAnd[#,2^64-1]&;
x1=BitXor[x1,BitXor[BitRotateRight64[x1,61],BitRotateRight64[x1,39]]]//BitAnd[#,2^64-1]&;
x2=BitXor[x2,BitXor[BitRotateRight64[x2,1],BitRotateRight64[x2,6]]]//BitAnd[#,2^64-1]&;
x3=BitXor[x3,BitXor[BitRotateRight64[x3,10],BitRotateRight64[x3,17]]]//BitAnd[#,2^64-1]&;
x4=BitXor[x4,BitXor[BitRotateRight64[x4,7],BitRotateRight64[x4,41]]]//BitAnd[#,2^64-1]&;
{x0,x1,x2,x3,x4}];

AsconLinearInv[state_] := Module[{x0, x1, x2, x3, x4},
  {x0, x1, x2, x3, x4} = state;
  x0 = BitXor[x0, BitXor[BitRotateRight64[x0, 45], BitRotateRight64[x0, 54]]];
  x1 = BitXor[x1, BitXor[BitRotateRight64[x1, 3], BitRotateRight64[x1, 23]]];
  x2 = BitXor[x2, BitXor[BitRotateRight64[x2, 58], BitRotateRight64[x2, 63]]];
  x3 = BitXor[x3, BitXor[BitRotateRight64[x3, 47], BitRotateRight64[x3, 54]]];
  x4 = BitXor[x4, BitXor[BitRotateRight64[x4, 23], BitRotateRight64[x4, 57]]];
  {x0, x1, x2, x3, x4}
];

(*XOR with Round constant*)
x2Xor[state_, RC_] := Module[{newstate = state},
  newstate[[3]] = BitXor[state[[3]], RC] // BitAnd[#, 2^64 - 1] &;
  newstate
];

(*Ascon forward and backward round*)
AsconRound[state_List, RC_Integer] :=
  AsconLinear[AsconSbox[x2Xor[state, RC]]];

AsconRoundInv[state_List, RC_Integer] :=
  x2Xor[AsconSboxInv[AsconLinearInv[state]], RC];
