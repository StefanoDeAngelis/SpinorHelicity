(* ::Package:: *)

BeginPackage["SpinorHelicity`",{"HelicityVariables`","NumericalKinematics`"}]


(* ::Section:: *)
(*Messages*)


MassDimension::usage = "..."
Helicity::usage = "..."
Particles::usage = "..."
ToSp::usage = "..."
ToBracket::usage = "..."

SpinorComponents::usage = "..."
NEvaluate::usage = "..."


(* ::Section:: *)
(*Numerical Kinematics*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Particles*)


(*Particles[exp_]:=Sort@DeleteDuplicates@Cases[{exp},HoldPattern[AML[x__]|Sp[x__]]:>Sequence@@{x},\[Infinity]]*)


(* ::Subsection:: *)
(*ToSp and ToBracket*)


(*ToSp[exp_] := ReplaceAll[exp, AML[i_, j_]^a_. SML[i_, j_]^b_. :> -Sp[i, j]^Min[a, b] * AML[i, j]^Max[a - b, 0] * SML[i, j]^Max[b - a, 0]]
ToBracket[exp_] := ReplaceAll[ReplaceAll[exp, Sp[a__] /; (Length[{a}] > 2) :> Sum[Sp[#[[i]], #[[j]]]& @ {a}, {i, Length[{a}]}, {j, i + 1, Length[{a}]}]], Sp[i_, j_] :> AML[i, j] SML[j, i]]*)


(* ::Subsection::Closed:: *)
(*Spin components*)


SpinorComponents[exp_] :=
	DeleteCases[
		Flatten@
			CoefficientList[#1,Flatten@Table[z[i, n], {i, #2}, {n, 2}]],
		0
	]&[
		ReplaceRepeated[exp,SpinorMV[][i_]:>Sum[z[i, n]*SpinorMV[$up][i,n],{n, 2}]],
		Sort@DeleteDuplicates@Cases[exp,SpinorMV[][i_]:>i,\[Infinity]]
	]


(* ::Subsection:: *)
(*NEvaluate*)


Attributes[NEvaluate]={Listable};

NEvaluate[exp_] :=
	Block[{SpinorMV, AngleB, SquareB, AngleAngleChain, AngleSquareChain,
		 SquareSquareChain, TraceChain, MassUntilde, MassTilde},
		 
		SpinorMV[$down][i_, 1] := -SpinorMV[$up][i, 2];
		SpinorMV[$down][i_, 2] := SpinorMV[$up][i, 1];
		
		AngleB[a_, -b_] := -AngleB[a, b];
		AngleB[-a_, b_] := -AngleB[a, b];
		SquareB[-a_, b_] := -SquareB[a, b];
		SquareB[a_, -b_] := -SquareB[a, b];
		
		AngleB[SpinorML[i_], SpinorML[j_]] := NSpinorUndottedML[i] . NEpsilon[
			-1] . NSpinorUndottedML[j];
		AngleB[SpinorML[i_], SpinorMV[$up][j_, J_]] := NSpinorUndottedML[i]
			 . NEpsilon[-1] . NSpinorUndottedMV[j, J];
		AngleB[SpinorMV[$up][i_, I_], SpinorMV[$up][j_, J_]] := NSpinorUndottedMV[
			i, I] . NEpsilon[-1] . NSpinorUndottedMV[j, J];
			
		SquareB[SpinorML[i_], SpinorML[j_]] := NSpinorDottedML[i] . NEpsilon[
			] . NSpinorDottedML[j];
		SquareB[SpinorML[i_], SpinorMV[$up][j_, J_]] := NSpinorDottedML[i] 
			. NEpsilon[] . NSpinorDottedMV[j, J];
		SquareB[SpinorMV[$up][i_, I_], SpinorMV[$up][j_, J_]] := NSpinorDottedMV[
			i, I] . NEpsilon[] . NSpinorDottedMV[j, J];
			
		AngleAngleChain[SpinorML[i_], list_List, SpinorML[j_]] :=
			NSpinorUndottedML[i] .
				Dot[
					NEpsilon[-1]
					,
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n]]]
								,
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
							]
							,
							{n, Length[list]}
						]
				] . NSpinorUndottedML[j];
		AngleAngleChain[SpinorML[i_], list_List, SpinorMV[$up][j_, J_]] :=
			NSpinorUndottedML[i] .
				Dot[
					NEpsilon[-1]
					,
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n]]]
								,
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
							]
							,
							{n, Length[list]}
						]
				] . NSpinorUndottedMV[j, J];
		AngleAngleChain[SpinorMV[$up][i_, I_], list_List, SpinorMV[$up][j_,
			 J_]] :=
			NSpinorUndottedMV[i, I] .
				Dot[
					NEpsilon[-1]
					,
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n]]]
								,
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
							]
							,
							{n, Length[list]}
						]
				] . NSpinorUndottedMV[j, J];
				
		AngleSquareChain[SpinorML[i_], list_List, SpinorML[j_]] :=
			NSpinorUndottedML[i] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n]]]
								,
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
							]
							,
							{n, Length[list]}
						]
					,
					NEpsilon[]
				] . NSpinorDottedML[j];
		AngleSquareChain[SpinorMV[$up][i_, I_], list_List, SpinorML[j_]] :=
			
			NSpinorUndottedMV[i, I] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n]]]
								,
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
							]
							,
							{n, Length[list]}
						]
					,
					NEpsilon[]
				] . NSpinorDottedML[j];
		AngleSquareChain[SpinorML[i_], list_List, SpinorMV[$up][j_, J_]] :=
			
			NSpinorUndottedML[i] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n]]]
								,
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
							]
							,
							{n, Length[list]}
						]
					,
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
		AngleSquareChain[SpinorMV[$up][i_, I_], list_List, SpinorMV[$up][j_,
			 J_]] :=
			NSpinorUndottedMV[i, I] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n]]]
								,
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
							]
							,
							{n, Length[list]}
						]
					,
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
				
		SquareSquareChain[SpinorML[i_], list_List, SpinorML[j_]] :=
			NSpinorDottedML[i] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
								,
								NMomentum[list[[n]]]
							]
							,
							{n, Length[list]}
						]
					,
					NEpsilon[]
				] . NSpinorDottedML[j];
		SquareSquareChain[SpinorML[i_], list_List, SpinorMV[$up][j_, J_]] :=
			
			NSpinorDottedML[i] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
								,
								NMomentum[list[[n]]]
							]
							,
							{n, Length[list]}
						]
					,
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
		SquareSquareChain[SpinorMV[$up][i_, I_], list_List, SpinorMV[$up][j_,
			 J_]] :=
			NSpinorDottedMV[i, I] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1]
								,
								NMomentum[list[[n]]]
							]
							,
							{n, Length[list]}
						]
					,
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
				
		TraceChain[ list_List] :=
				Tr[
					Dot[
						Sequence @@
							Table[
								If[OddQ[n],
									NEpsilon[] . Transpose[NMomentum[list[[n]]]] . NEpsilon[-1],
									NMomentum[list[[n]]]
								]
								,
								{n, Length[list]}
							]
					]
				];
		
		MassUntilde[i_] := -AngleB[SpinorMV[$up][i, 1], SpinorMV[$up][i, 2]
			];
		MassTilde[i_] := SquareB[SpinorMV[$up][i, 1], SpinorMV[$up][i, 2]];
			
		Return[exp]
	]


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["SpinorOperations`*"]


EndPackage[]
