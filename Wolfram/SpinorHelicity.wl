(* ::Package:: *)

BeginPackage["SpinorHelicity`", {"HelicityVariables`", "NumericalKinematics`"}]


(* ::Section:: *)
(*Messages*)


MassDimension::usage = "..."
Helicity::usage = "..."
Particles::usage = "..."

ToSp::usage = "..."
ToBracket::usage = "..."

SpinorComponents::usage = "..."
z\[Zeta]::usage = "..."
SpinorComponentSum::usage = "..."
SingleComponent::usage = "..."

NEvaluate::usage = "..."


(* ::Section:: *)
(*Numerical Kinematics*)


(*TODOs:
	- define new (numerical) variables which stores the numerical evaluations of structures already encountered
	- define a spin function (which takes into account the possibility of having SpinorDottedMV[$up,$up][p1,a1,J1]SpinorUndottedMV[$up,$down][p1,a2,J1])
*)


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


(* ::Subsection::Closed:: *)
(*ToSp and ToBracket*)


(*ToSp[exp_] := ReplaceAll[exp, AML[i_, j_]^a_. SML[i_, j_]^b_. :> -Sp[i, j]^Min[a, b] * AML[i, j]^Max[a - b, 0] * SML[i, j]^Max[b - a, 0]]
ToBracket[exp_] := ReplaceAll[ReplaceAll[exp, Sp[a__] /; (Length[{a}] > 2) :> Sum[Sp[#[[i]], #[[j]]]& @ {a}, {i, Length[{a}]}, {j, i + 1, Length[{a}]}]], Sp[i_, j_] :> AML[i, j] SML[j, i]]*)


(* ::Subsection:: *)
(*Spin components*)


Options[SpinorComponents]={Echos->False}

SpinorComponents[exp_List,OptionsPattern[]]:=
	If[
		OptionValue[Echos],
		Prepend[
			SpinorComponents/@exp[[2;;]],
			SpinorComponents[exp[[1]],Echos->True]
		],
		SpinorComponents/@exp
	]

SpinorComponents[exp_,OptionsPattern[]] :=
    DeleteCases[
        Flatten@
            CoefficientList[#1,If[OptionValue[Echos],Echo[#],#]&@(Flatten@Table[z\[Zeta][a,I], {a, #2}, {I, 2}])],
        0
    ]&[
        ReplaceRepeated[exp,{SpinorUndottedMV[][a_]:>Sum[SpinorUndottedMV[$up][a,I]z\[Zeta][a,I],{I,2}],SpinorDottedMV[][a_]:>Sum[SpinorDottedMV[$up][a,I]z\[Zeta][a,I],{I,2}]}],
        Sort@DeleteDuplicates@Cases[exp,SpinorUndottedMV[][i_]|SpinorDottedMV[][i_]:>i,All]
    ]


(* ::Subsubsection::Closed:: *)
(*z\[Zeta] variables*)


z\[Zeta]Box[a_,I_] :=
	TemplateBox[{a,I}, "z\[Zeta]",
		DisplayFunction -> (SubscriptBox["z", RowBox[{RowBox[{#1}],",",RowBox[{#2}]}]]&),
		InterpretationFunction -> (RowBox[{"z\[Zeta]","[", RowBox[{#1}],",",RowBox[{#2}],"]"}]&)
	]

z\[Zeta] /: MakeBoxes[z\[Zeta][a_,I_], StandardForm | TraditionalForm] := z\[Zeta]Box[ToBoxes[a],ToBoxes[I]]


(* ::Subsubsection:: *)
(*SpinorComponentSum*)


Options[SpinorComponentSum]={SingleComponent->False}

SetAttributes[SpinorComponentSum,Listable]

SpinorComponentSum[exp_,OptionsPattern[]] := 
	ReplaceRepeated[
		exp,
		#
	]&@
		If[
			OptionValue[SingleComponent],
			{SpinorUndottedMV[][a_]:>SpinorUndottedMV[$up][a,1],SpinorDottedMV[][a_]:>SpinorDottedMV[$up][a,1]},
			{SpinorUndottedMV[][a_]:>Sum[SpinorUndottedMV[$up][a,I]z\[Zeta][a,I],{I,2}],SpinorDottedMV[][a_]:>Sum[SpinorDottedMV[$up][a,I]z\[Zeta][a,I],{I,2}]}
		]


(* ::Subsection::Closed:: *)
(*NEvaluate*)


Attributes[NEvaluate] = {Listable};

NEvaluate[exp_] :=
	Block[{SpinorDottedMV, SpinorUndottedMV, AngleB, SquareB, AngleAngleChain,
		 AngleSquareChain, SquareSquareChain, TraceChain, MassUntilde, MassTilde
		},
		SpinorDottedMV[$down][i_, 1] := -SpinorDottedMV[$up][i, 2];
		SpinorDottedMV[$down][i_, 2] := SpinorDottedMV[$up][i, 1];
		SpinorUndottedMV[$down][i_, 1] := -SpinorUndottedMV[$up][i, 2];
		SpinorUndottedMV[$down][i_, 2] := SpinorUndottedMV[$up][i, 1];
		AngleB[a_, -b_] := -AngleB[a, b];
		AngleB[-a_, b_] := -AngleB[a, b];
		SquareB[-a_, b_] := -SquareB[a, b];
		SquareB[a_, -b_] := -SquareB[a, b];
		AngleB[SpinorUndottedML[][i_], SpinorUndottedML[][j_]] := NSpinorUndottedML[i] . NEpsilon[-1] . NSpinorUndottedML[j];
		AngleB[SpinorUndottedML[][i_], SpinorUndottedMV[$up][j_, J_]] := NSpinorUndottedML[i] . NEpsilon[-1] . NSpinorUndottedMV[j, J];
		AngleB[SpinorUndottedMV[$up][i_, II_], SpinorUndottedMV[$up][j_, J_]] := NSpinorUndottedMV[i, II] . NEpsilon[-1] . NSpinorUndottedMV[j,J];
		SquareB[SpinorDottedML[][i_], SpinorDottedML[][j_]] := NSpinorDottedML[i] . NEpsilon[] . NSpinorDottedML[j];
		SquareB[SpinorDottedML[][i_], SpinorDottedMV[$up][j_, J_]] := NSpinorDottedML[i] . NEpsilon[] . NSpinorDottedMV[j, J];
		SquareB[SpinorDottedMV[$up][i_, II_], SpinorDottedMV[$up][j_, J_]] := NSpinorDottedMV[i, II] . NEpsilon[] . NSpinorDottedMV[j, J];
		AngleAngleChain[SpinorUndottedML[][i_], list_List, SpinorUndottedML[][j_]] :=
			NSpinorUndottedML[i] .
				Dot[
					NEpsilon[-1],
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n,1]]],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1]
							],
							{n, Length[list]}
						]
				] . NSpinorUndottedML[j];
		AngleAngleChain[SpinorUndottedML[][i_], list_List, SpinorUndottedMV[$up][j_, J_]] :=
			NSpinorUndottedML[i] .
				Dot[
					NEpsilon[-1],
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n,1]]],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1]
							],
							{n, Length[list]}
						]
				] . NSpinorUndottedMV[j, J];
		AngleAngleChain[SpinorUndottedMV[$up][i_, II_], list_List, SpinorUndottedMV[$up][j_, J_]] :=
			NSpinorUndottedMV[i, II] .
				Dot[
					NEpsilon[-1],
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n,1]]],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1]
							],
							{n, Length[list]}
						]
				] . NSpinorUndottedMV[j, J];
		AngleSquareChain[SpinorUndottedML[][i_], list_List, SpinorDottedML[][j_]] :=
			NSpinorUndottedML[i] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n,1]]],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1]
							],
							{n, Length[list]}
						],
					NEpsilon[]
				] . NSpinorDottedML[j];
		AngleSquareChain[SpinorUndottedMV[$up][i_, II_], list_List, SpinorDottedML[][j_]] :=
			NSpinorUndottedMV[i, II] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n,1]]],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1]
							],
							{n, Length[list]}
						],
					NEpsilon[]
				] . NSpinorDottedML[j];
		AngleSquareChain[SpinorUndottedML[][i_], list_List, SpinorDottedMV[$up][j_, J_]] :=
			NSpinorUndottedML[i] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n,1]]],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1]
							],
							{n, Length[list]}
						],
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
		AngleSquareChain[SpinorUndottedMV[$up][i_, II_], list_List, SpinorDottedMV[$up][j_, J_]] :=
			NSpinorUndottedMV[i, II] . NEpsilon[-1] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NMomentum[list[[n,1]]],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1]
							],
							{n, Length[list]}
						],
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
		SquareSquareChain[SpinorDottedML[][i_], list_List, SpinorDottedML[][j_]] :=
			NSpinorDottedML[i] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1],
								NMomentum[list[[n,1]]]
							],
							{n, Length[list]}
						],
					NEpsilon[]
				] . NSpinorDottedML[j];
		SquareSquareChain[SpinorDottedML[][i_], list_List, SpinorDottedMV[$up][j_, J_]] :=
			NSpinorDottedML[i] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1],
								NMomentum[list[[n,1]]]
							],
							{n, Length[list]}
						],
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
		SquareSquareChain[SpinorDottedMV[$up][i_, II_], list_List, SpinorDottedMV[$up][j_, J_]] :=
			NSpinorDottedMV[i, II] .
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1],
								NMomentum[list[[n,1]]]
							],
							{n, Length[list]}
						],
					NEpsilon[]
				] . NSpinorDottedMV[j, J];
		TraceChain[list_List] :=
			Tr[
				Dot[
					Sequence @@
						Table[
							If[OddQ[n],
								NEpsilon[] . Transpose[NMomentum[list[[n,1]]]] . NEpsilon[-1],
								NMomentum[list[[n,1]]]
							],
							{n, Length[list]}
						]
				]
			];
		MassUntilde[i_] := -AngleB[SpinorUndottedMV[$up][i, 1], SpinorUndottedMV[$up][i, 2]];
		MassTilde[i_] := SquareB[SpinorDottedMV[$up][i, 1], SpinorDottedMV[$up][i, 2]];
		Return[exp]
	]


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect @@ Names["SpinorOperations`*"]

EndPackage[]
