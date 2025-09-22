(* ::Package:: *)

(* ::Subtitle:: *)
(*BOOMERANG ATTACK*)


(*IMPORT PRIMITIVES*)
SetDirectory[NotebookDirectory[]];
Get["AsconPrimitives.wl"];

(*Complete Ascon direct and inverse permutation*)
AsconPermutation[state_, RC_] := Fold[AsconRound, state, RC];
AsconPermutationInv[state_, RC_] := Fold[AsconRoundInv, state, Reverse[RC]];

(*Splitting the permutation in E0 and E1*)
E0[state_, RC_, cutAfter_] := AsconPermutation[state, Take[RC, cutAfter]];
E1[state_, RC_, cutAfter_] := AsconPermutation[state, Drop[RC, cutAfter]];
E1Inv[state_, RC_, cutAfter_] := AsconPermutationInv[state, Drop[RC, cutAfter]];

(*Boomerang quartet*)
QuartetTest[P_, Delta_, Nabla_, RC_, cutAfter_] := Module[{Pp, C, Cp, D, Dp, Q, Qp},
    Pp = XorState[P, Delta];
    C = AsconPermutation[P, RC];
    Cp = AsconPermutation[Pp, RC];
    D = XorState[C, Nabla];
    Dp = XorState[Cp, Nabla];
    Q = AsconPermutationInv[D, RC];
    Qp = AsconPermutationInv[Dp, RC];
    XorState[Q, Qp] === Delta
  ];

(*Probabbility estimators*)
EstimateP[nSamples_, Delta_, DeltaStar_, RC_, cutAfter_] := Module[{count = 0, P, mid1, mid2, diff},
    Do[
      P = RandomState[];
      mid1 = E0[P, RC, cutAfter];
      mid2 = E0[XorState[P, Delta], RC, cutAfter];
      diff = XorState[mid1, mid2];
      If[diff === DeltaStar, count++],
      {nSamples}
    ];
    N[count/nSamples]
  ];

EstimateQ[nSamples_, Nabla_, NablaStar_, RC_, cutAfter_] := Module[{count = 0, C, a, b, diff},
    Do[
      C = RandomState[];
      a = E1Inv[C, RC, cutAfter];
      b = E1Inv[XorState[C, Nabla], RC, cutAfter];
      diff = XorState[a, b];
      If[diff === NablaStar, count++],
      {nSamples}
    ];
    N[count/nSamples]
  ];

EstimateBoomerangProb[nSamples_, Delta_, Nabla_, RC_, cutAfter_] := Module[{count = 0, P},
    Do[
      P = RandomState[];
      If[QuartetTest[P, Delta, Nabla, RC, cutAfter], count++],
      {nSamples}
    ];
    N[count/nSamples]
  ];

(*Comparing theorical and estimated probability*)
CompareProbabilities[ nSamples_, Delta_, Nabla_, RC_, cutAfter_, deltaStar_, nablaStar_] := Module[{probP, probQ, probEstimated, probTheor},

    probP = EstimateP[nSamples, Delta, deltaStar, RC, cutAfter];
    probQ = EstimateQ[nSamples, Nabla, nablaStar, RC, cutAfter];
    probEstimated= EstimateBoomerangProb[nSamples, Delta, Nabla, RC, cutAfter];
    probTheor = N[probP^2 * probQ^2];
    
    (*Results*)
    Print["Number of samples: ", nSamples];
    Print["Estimated probability (P): ", probP];
    Print["Estimated probability(Q): ", probQ];
    Print["Boomerang theorical probability: ", ScientificForm[probTheor]];
    Print["Boomerang estimated probability: ", ScientificForm[probEstimated]];
    
];
