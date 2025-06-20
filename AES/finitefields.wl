Print["load finitefields"];

poly=FromDigits[IntegerDigits[FromDigits["11B",16],2],x];
m=Exponent[poly,x];




(* Somma di due elementi del campo finito F_{2^4} *)
FieldSum[u_]:= u;
FieldSum[u_, v_]:= BitXor[u,v];
FieldSum[f1_, sf2__]:= FieldSum[f1, FieldSum[sf2]];
FieldSum[input1_List, input2_List]:= Map[FieldSum[input1[[#]], input2[[#]]]&, Range[Length[input1]]];


(* Prodotto di due elementi del campo finito F_{2^4} *)
Module[{px, x},
	px = poly; (* p(x)=x^4+x+1 è il polinomio irriducibile che definisce il campo *)

	(* Funzione per trasformare un elemento del campo finito F_{2^4} in un polinomio *)
	Int2Poly[numero_]:=
		If[numero>=0,
		IntegerDigits[numero, 2, m] . Reverse@Table[x^i, {i,0,m-1}],
		(* else *)
		Mod[Times[-1,IntegerDigits[numero, 2, m]], 2] . Reverse@Table[x^i, {i,0,m-1}]
	];

	(* Funzione per trasformare un polinomio in un elemento del campo finito F_{2^4} *)
	Poly2Int[poly_]:= FromDigits[Reverse@CoefficientList[poly, x, m], 2];

	(* Funzione per il calcolo del prodotto di due elementi del campo finito F_{2^4} *)
	FieldMult[f1_]:= f1;
	FieldMult[f1_, f2_]:= Poly2Int[PolynomialRemainder[Int2Poly[f1]*Int2Poly[f2], px, x, Modulus->2]];
	FieldMult[f1_, sf2__]:= FieldMult[f1, FieldMult[sf2]];
	FieldMult[input_List, input2_?NumericQ]:= Map[FieldMult[input[[#]], input2]&, Range[Length[input]]];
	FieldMult[input1_?NumericQ, input_List]:= Map[FieldMult[input[[#]], input1]&, Range[Length[input]]];
];


(* Divisibilità di un elemento del campo finito F_{2^m} per x *)
AlphaDivisible[u_]:= Mod[u,2]==0;
AlphaDivide[u_]:= Floor[u/2];

(* Grado di un elemento del campo finito F_{2^4} *)
(* Deg[u_]:= Module[{i}, i=0; While[Floor[u/2^i]>0, i++]; i-1];*)
Deg[u_]:= Floor[Log[2,u]];

(* Inverso di un elemento del campo finito F_{2^4} con l'algoritmo di Fong *)
FongInverse[a_]:= Module[{px, u, v, g1, g2, temp},
	px =poly; (* p(x)=x^4+x+1 è il polinomio irriducibile che definisce il campo *)
	{u, v, g1, g2} = {a, px, 1, 0};
	While[u=!=1,
		While[AlphaDivisible[u],
			u = AlphaDivide[u];
			If[AlphaDivisible[g1],
				g1 = AlphaDivide[g1],
				(*else*)
				g1 = AlphaDivide[FieldSum[g1, px]]
			]; (* fine If *)
		];(*fine While interno*)
		If[u==1,
			Return[g1], (* output *)
			If[Deg[u]<Deg[v], (* Utilizziamo una variabile temporanea per effettuare lo scambio *)
				{u,v}={v,u};
				{g1,g2}={g2,g1};
			]; (* fine If interno *)
		u = FieldSum[u, v];
		g1 = FieldSum[g1, g2];
		]; (* fine If esterno *)
	];(* fine While esterno *)
	Return[g1]; (* output nel caso u==1 dall'inizio *)
];
