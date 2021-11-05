(* ::Package:: *)

BeginPackage["SpinorOperations`"]


(* ::Section:: *)
(*Messages*)


MassDimension::usage = "..."
Helicity::usage = "..."
Particles::usage = "..."
ToSp::usage = "..."
ToBracket::usage = "..."


(* ::Section:: *)
(*Numerical Kinematics*)


Begin["`Private`"]


(* ::Subsection:: *)
(*MassDimension*)


(*MassDimension[AML[a_, b_]] := 1;
MassDimension[SML[a_, b_]] := 1;
MassDimension[Sp[a__]] := 2;

(*MassDimension[x_]/;NumberQ[x]:=0;*)
MassDimension[exp_Times] := Plus @@ (MassDimension /@ List @@ exp)

MassDimension[Power[expr_, expo_]] := expo * MassDimension[expr]

MassDimension[exp_Plus] := Block[{dims = MassDimension /@ List @@ exp},
	dims = DeleteDuplicates[dims]; If[Length[dims] > 1,
		Message[MassDimension::hom, exp]
		,
		Return[dims[[1]]]
	]
]

MassDimension::hom = "`1` is not homogeneous in the mass dimension";

MassDimension[x_] := 0*)


(* ::Subsection:: *)
(*Spin*)


(*Helicity[AML[a_, b_], label_] := If[a == label || b == label,
	-1
	,
	0
];

Helicity[SML[a_, b_], label_] := If[a == label || b == label,
	1
	,
	0
];

(*Helicity[Sp[a__],label_]:=0;
Helicity[x_,label_]/;NumberQ[x]:=0;*)
Helicity[exp_Times, label_] := Plus @@ (Helicity[#, label]& /@ List @@ exp)

Helicity[Power[expr_, expo_], label_] := expo * Helicity[expr, label]

Helicity[exp_Plus, label_] := Block[{dims = Helicity[#, label]& /@ List @@ exp},
	dims = DeleteDuplicates[dims]; If[Length[dims] > 1,
		Message[Helicity::hom, exp, label]
		,
		Return[dims[[1]]]
	]
]
Helicity::hom = "`1` is not homogeneous in the helicity weight of particle `2`";

Helicity[x_, label_] := 0*)


(* ::Subsection:: *)
(*Particles*)


(*Particles[exp_]:=Sort@DeleteDuplicates@Cases[{exp},HoldPattern[AML[x__]|Sp[x__]]:>Sequence@@{x},\[Infinity]]*)


(* ::Subsection:: *)
(*ToSp and ToBracket*)


(*ToSp[exp_] := ReplaceAll[exp, AML[i_, j_]^a_. SML[i_, j_]^b_. :> -Sp[i, j]^Min[a, b] * AML[i, j]^Max[a - b, 0] * SML[i, j]^Max[b - a, 0]]
ToBracket[exp_] := ReplaceAll[ReplaceAll[exp, Sp[a__] /; (Length[{a}] > 2) :> Sum[Sp[#[[i]], #[[j]]]& @ {a}, {i, Length[{a}]}, {j, i + 1, Length[{a}]}]], Sp[i_, j_] :> AML[i, j] SML[j, i]]*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["SpinorOperations`*"]


EndPackage[]
