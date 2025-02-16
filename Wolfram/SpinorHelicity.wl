(* ::Package:: *)

BeginPackage["SpinorHelicity`", {"HelicityVariables`","NumericalKinematics`","YoungSymm`","DdimPackage`","DdimVariables`"}]


(* ::Section:: *)
(*Conflicts with DdimPackage*)


Unprotect[MassDimension];
ClearAll[MassDimension];


(* ::Section:: *)
(*Messages*)


MassDimension::usage = "..."
Helicity::usage = "..."
Labels::usage = "..."

ToSp::usage = "..."
ToBracket::usage = "..."

ContractLittleGroup::usage = "..."

SpinorComponents::usage = "..."
SpinorComponent::usage = "..."
OpenTraces::usage = "..."

UnboldSpinors::usage = "..."
BoldSpinors::usage = "..."
SpinorSubset::usage = "..."
LGPosition::usage = "..."
LGSymmetric::usage = "..."

(*z\[Zeta]::usage = "..."
SpinorComponentSum::usage = "..."
SingleComponent::usage = "..."*)

SpinorDerivative::usage = "..."

NEvaluate::usage = "..."


(* ::Section:: *)
(*Numerical Kinematics*)


(*TODOs:
	- define new (numerical) variables which stores the numerical evaluations of structures already encountered
	- define a function which can determine the spin/helicity of each particle (which takes into account the possibility of having SpinorDottedMV[$up,$up][p1,a1,J1]SpinorUndottedMV[$up,$down][p1,a2,J1])
	- an object for 4d momenta will NOT be defined. We can define abstract Pauli matrices to convert Ddim momenta
	- define a function which opens up the bold notation into symmetrised indices
	- define a function which checks that repeated indices are either up or down SpinorML[x_][a_,y_]:> {y,x} in Cases
	- define a function which checks indices: no repeated indices in the same position (as above), no numerical indices higher than 2, no mixed notation bold/unbold for massive (convert everything to bold)
	- write a function which checks there are no Dotten spinors inside Angles and the opposite
	- error message in SpinorDerivative if it finds a SpinorDottedMV[][lab] (or undotted version), i.e. no 
	- define a box for the SpinorDerivative and a new object on which the derivative get stuck (possible also the commutation of the derivatives and derivatives with more than one argument)
	- spins and massdimension functions should check homogeneity
*)
(*
	- UnboldSpinors presents two main problems, which should both be fixed by adding an Expand function at the beginning, possibly checking if there is a Plus at some level (for example, nested with a Times)
*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*MassDimension4D*)


Attributes[MassDimension]={Listable};

MassDimension[SpinorDottedML[___][__]] := 1/2;
MassDimension[SpinorUndottedML[___][__]] := 1/2;
MassDimension[SpinorDottedMV[___][__]] := 1/2;
MassDimension[SpinorUndottedMV[___][__]] := 1/2;
MassDimension[AngleB[a_,b_]] := 1;
MassDimension[SquareB[a_,b_]] := 1;
MassDimension[AngleAngleChain[a_,x_,b_]] := 1+Length[x];
MassDimension[AngleSquareChain[a_,x_,b_]] := 1+Length[x];
MassDimension[SquareSquareChain[a_,x_,b_]] := 1+Length[x];
MassDimension[MassUntilde[_]] := 1;
MassDimension[MassTilde[_]] := 1;

MassDimension[FieldStr[__]]:=1
MassDimension[Riemann[__]]:=2
MassDimension[Momentum[___]]:=1
MassDimension[FTrace[a_,x_,b_]]:=MassDimension[a]+MassDimension[b]+Length[x]
MassDimension[Mass[_]]:=1
MassDimension[Mandelstam[__]]:=2
MassDimension[DotProduct[a_,b_]]:=MassDimension[a]+MassDimension[b]

(*MassDimension[x_]/;NumberQ[x]:=0;*)
MassDimension[exp_Times] := Plus @@ (MassDimension /@ List @@ exp)

MassDimension[Power[expr_, expo_]] := expo * MassDimension[expr]

MassDimension[exp_Plus] := 
	Block[{dims = MassDimension /@ (List @@ exp)},
		dims = DeleteDuplicates[dims];
		If[
			Length[dims] > 1,
			Message[MassDimension::hom, exp],
			Return[dims[[1]]]
		]
	]

MassDimension::hom = "`1` is not homogeneous in the mass dimension";

MassDimension[x_] := 0


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
(*Labels*)


Labels[exp_Sum]:=Union[Sequence@@(Labels/@List@@exp)]
Labels[exp_Times]:=Union[Sequence@@(Labels/@(List@@exp))]
Labels[exp_Power]:=Labels[exp[[1]]]
Labels[exp_]:=
	Union[
		Cases[exp,SpinorUndottedML[pos___][lab_,ind___]|SpinorDottedML[pos___][lab_,ind___]|HoldPattern[SpinorUndottedMV[pos___][lab_,ind___]]|HoldPattern[SpinorDottedMV[pos___][lab_,ind___]]:>lab,All],
		Cases[exp,AngleAngleChain[a_,list_,b_]|SquareSquareChain[a_,list_,b_]|AngleSquareChain[a_,list_,b_]|TraceChain[list_]:>Sequence@@(Part[#,1]&/@list),All]
	]


(* ::Subsection::Closed:: *)
(*ToSp and ToBracket*)


(*ToSp[exp_] := ReplaceAll[exp, AML[i_, j_]^a_. SML[i_, j_]^b_. :> -Sp[i, j]^Min[a, b] * AML[i, j]^Max[a - b, 0] * SML[i, j]^Max[b - a, 0]]
ToBracket[exp_] := ReplaceAll[ReplaceAll[exp, Sp[a__] /; (Length[{a}] > 2) :> Sum[Sp[#[[i]], #[[j]]]& @ {a}, {i, Length[{a}]}, {j, i + 1, Length[{a}]}]], Sp[i_, j_] :> AML[i, j] SML[j, i]]*)


(* ::Subsection::Closed:: *)
(*ContractLittleGroup*)


ContractLittleGroup[exp_]:=
Block[{localexp,AngleB,SquareB,AngleAngleChain,SquareSquareChain,AngleSquareChain},

	AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos1_][i_, II_]] SquareSquareChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], 1, -1] * AngleSquareChain[a, Prepend[list,Momentum[i]], b];
	AngleB /: HoldPattern[AngleB[SpinorUndottedMV[pos1_][i_, II_], a_] SquareSquareChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] :=(*If[\[Not]MatchQ[pos1,pos2],0(*errore!*),*)If[MatchQ[pos1, $up], -1, 1] * AngleSquareChain[a, Prepend[list, Momentum[i]], b] (*]*);
	AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos1_][i_, II_]] SquareSquareChain[b_, list_List, SpinorUndottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], -1, 1] * AngleSquareChain[b, Append[list, Momentum[i]], a];
	AngleB /: HoldPattern[AngleB[SpinorUndottedMV[pos1_][i_, II_], a_] SquareSquareChain[b_, list_List, SpinorUndottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], 1, -1] * AngleSquareChain[b, Append[list, Momentum[i]], a];
	AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos1_][i_, II_]] AngleSquareChain[b_, list_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], -1, 1] * AngleAngleChain[b, Append[list, Momentum[i]], a];
	AngleB /: HoldPattern[AngleB[SpinorUndottedMV[pos1_][i_, II_], a_] AngleSquareChain[b_, list_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], 1, -1] * AngleAngleChain[b, Append[list, Momentum[i]], a];
	AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos1_][i_, II_]] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassUntilde[i] AngleSquareChain[a,list, b];
	AngleB /: HoldPattern[AngleB[SpinorUndottedMV[pos1_][i_, II_], a_] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassUntilde[i] AngleSquareChain[a,list, b];
	AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos1_][i_, II_]] AngleAngleChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassUntilde[i] AngleAngleChain[a,list, b];
	AngleB /: HoldPattern[AngleB[SpinorUndottedMV[pos1_][i_, II_], a_] AngleAngleChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassUntilde[i] AngleAngleChain[a,list, b];
	AngleB /: HoldPattern[AngleB[SpinorUndottedMV[pos1_][i_, II_], a_] AngleAngleChain[b_, list_List, SpinorUndottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassUntilde[i] AngleAngleChain[b,list, a];
	AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos1_][i_, II_]] AngleAngleChain[b_, list_List, SpinorUndottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassUntilde[i] AngleAngleChain[b,list, a];

	SquareB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos2_][i_, II_]] * SquareB[b_, SpinorDottedMV[pos1_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] * AngleSquareChain[a, {Momentum[i]}, b];
	SquareB /: HoldPattern[AngleB[SpinorUndottedMV[pos2_][i_, II_], a_] * SquareB[b_, SpinorDottedMV[pos1_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] * AngleSquareChain[a, {Momentum[i]}, b];
	SquareB /: HoldPattern[AngleB[SpinorUndottedMV[pos2_][i_, II_], a_] * SquareB[SpinorDottedMV[pos1_][i_, II_], b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] * AngleSquareChain[a, {Momentum[i]}, b];
	SquareB /: HoldPattern[AngleB[a_, SpinorUndottedMV[pos2_][i_, II_]] * SquareB[SpinorDottedMV[pos1_][i_, II_], b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] * AngleSquareChain[a, {Momentum[i]}, b];
	SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[pos1_][i_, II_]] AngleAngleChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] * AngleSquareChain[b, Reverse @ Prepend[list, Momentum[i]], a];
	SquareB /: HoldPattern[SquareB[SpinorDottedMV[pos1_][i_, II_], a_] AngleAngleChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] :=(*If[\[Not]MatchQ[pos1,pos2],0(*errore!*),*)If[MatchQ[pos1, $down], -1,1] * AngleSquareChain[b, Reverse @ Prepend[list, Momentum[i]], a] (*]*);
	SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[pos1_][i_, II_]] AngleAngleChain[b_, list_List, SpinorUndottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] * AngleSquareChain[b, Append[list,Momentum[i]], a];
	SquareB /: HoldPattern[SquareB[SpinorDottedMV[pos1_][i_, II_], a_] AngleAngleChain[b_, list_List, SpinorUndottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] * AngleSquareChain[b, Append[list,Momentum[i]], a];
	SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[pos1_][i_, II_]] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] * SquareSquareChain[a, Prepend[list,Momentum[i]], b];
	SquareB /: HoldPattern[SquareB[SpinorDottedMV[pos1_][i_, II_], a_] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] * SquareSquareChain[a, Prepend[list,Momentum[i]], b];
	SquareB /: HoldPattern[SquareB[SpinorDottedMV[pos1_][i_, II_], a_] AngleSquareChain[b_, list_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassTilde[i] AngleSquareChain[b, list, a];
	SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[pos1_][i_, II_]] AngleSquareChain[b_, list_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassTilde[i] AngleSquareChain[b, list, a];
	SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[pos1_][i_, II_]] SquareSquareChain[SpinorDottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassTilde[i] SquareSquareChain[a,list, b];
	SquareB /: HoldPattern[SquareB[SpinorDottedMV[pos1_][i_, II_], a_] SquareSquareChain[SpinorDottedMV[pos2_][i_, II_], list_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassTilde[i] SquareSquareChain[a,list, b];
	SquareB /: HoldPattern[SquareB[SpinorDottedMV[pos1_][i_, II_], a_] SquareSquareChain[b_, list_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassTilde[i] SquareSquareChain[b,list, a];
	SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[pos1_][i_, II_]] SquareSquareChain[b_, list_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassTilde[i] SquareSquareChain[b,list, a];
	
	AngleAngleChain /: HoldPattern[AngleAngleChain[a_, list1_List, SpinorUndottedMV[pos1_][i_, II_]] SquareSquareChain[SpinorDottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], 1, -1] * AngleSquareChain[a, Join[list1, Prepend[list2, Momentum[i]]], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorUndottedMV[pos1_][i_, II_], list1_List, a_] SquareSquareChain[SpinorDottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], -1, 1] * AngleSquareChain[a, Join[Reverse @ list1, Prepend[list2, Momentum[i]]], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorUndottedMV[pos1_][i_, II_], list1_List, a_] SquareSquareChain[b_, list2_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], 1, -1] * AngleSquareChain[a, Join[Reverse @ list1, Prepend[Reverse @ list2, Momentum[i]]], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[a_, list1_List, SpinorUndottedMV[pos1_][i_, II_]] SquareSquareChain[b_, list2_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], -1, 1] * AngleSquareChain[a, Join[list1, Prepend[Reverse @ list2, Momentum[i]]], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[a_, list1_List, SpinorUndottedMV[pos1_][i_, II_]] AngleSquareChain[b_, list2_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], -1, 1] * AngleAngleChain[b, Join[list2, Prepend[Reverse @ list1, Momentum[i]]], a];
	AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorUndottedMV[pos1_][i_, II_], list1_List, a_] AngleSquareChain[b_, list2_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $up], 1, -1] * AngleAngleChain[b, Join[list2, Prepend[list1, Momentum[i]]], a];
	AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorUndottedMV[pos1_][i_, II_], list1_List, a_] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassUntilde[i] AngleSquareChain[a, Join[Reverse @ list1, list2], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[a_, list1_List, SpinorUndottedMV[pos1_][i_, II_]] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassUntilde[i] AngleSquareChain[a, Join[list1, list2], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorUndottedMV[$down][i_, II_], list1_List, a_] AngleAngleChain[SpinorUndottedMV[$up][i_, II_], list2_List, b_]] := -MassUntilde[i] AngleAngleChain[a, Join[Reverse @ list1, list2], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[a_, list1_List, SpinorUndottedMV[$down][i_, II_]] AngleAngleChain[b_, list2_List, SpinorUndottedMV[$up][i_, II_]]] := -MassUntilde[i] AngleAngleChain[a, Join[list1, Reverse @ list2], b];
	AngleAngleChain /: HoldPattern[AngleAngleChain[a_, list1_List, SpinorUndottedMV[pos1_][i_, II_]] AngleAngleChain[SpinorUndottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassUntilde[i] AngleAngleChain[a, Join[list1, list2], b];
	(*AngleAngleChain /: AngleAngleChain[SpinorMV[$up][II_,II_],list_List,SpinorMV[$down][II_,II_]] := -MassUntilde[i]TraceChain[RotateRight@list];*)
	AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorUndottedMV[$down][i_, II_], list_List, SpinorUndottedMV[$up][i_, II_]]] := -MassUntilde[i] TraceChain[RotateRight @ list];
	
	SquareSquareChain /: HoldPattern[SquareSquareChain[a_, list1_List, SpinorDottedMV[pos1_][i_, II_]] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] * SquareSquareChain[a, Join[list1, Prepend[list2, Momentum[i]]], b];
	SquareSquareChain /: HoldPattern[SquareSquareChain[SpinorDottedMV[pos1_][i_, II_], list1_List, a_] AngleSquareChain[SpinorUndottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] * SquareSquareChain[a, Join[Reverse @ list1, Prepend[list2, Momentum[i]]], b];
	SquareSquareChain /: HoldPattern[SquareSquareChain[SpinorDottedMV[pos1_][i_, II_], list1_List, a_] AngleSquareChain[b_, list2_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], 1, -1] MassTilde[i] AngleSquareChain[b, Join[list2, list1], a];
	SquareSquareChain /: HoldPattern[SquareSquareChain[a_, list1_List, SpinorDottedMV[pos1_][i_, II_]] AngleSquareChain[b_, list2_List, SpinorDottedMV[pos2_][i_, II_]]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassTilde[i] AngleSquareChain[b, Join[list2, Reverse @ list1], a];
	SquareSquareChain /: HoldPattern[SquareSquareChain[SpinorDottedMV[$down][i_, II_], list1_List, a_] SquareSquareChain[SpinorDottedMV[$up][i_, II_], list2_List, b_]] := MassTilde[i] SquareSquareChain[a, Join[Reverse @ list1, list2], b];
	SquareSquareChain /: HoldPattern[SquareSquareChain[a_, list1_List, SpinorDottedMV[$down][i_, II_]] SquareSquareChain[b_, list2_List, SpinorDottedMV[$up][i_, II_]]] := MassTilde[i] SquareSquareChain[a, Join[list1, Reverse @ list2], b];
	SquareSquareChain /: HoldPattern[SquareSquareChain[a_, list1_List, SpinorDottedMV[pos1_][i_, II_]] SquareSquareChain[SpinorDottedMV[pos2_][i_, II_], list2_List, b_]] /; \[Not]MatchQ[pos1, pos2] := If[MatchQ[pos1, $down], -1, 1] MassTilde[i] SquareSquareChain[a, Join[list1, list2], b];
	(*SquareSquareChain /: SquareSquareChain[SpinorMV[$up][II_,II_],list_List,SpinorMV[$down][II_,II_]] := -MassTilde[i]TraceChain[list];*)
	SquareSquareChain /: HoldPattern[SquareSquareChain[SpinorDottedMV[$down][i_, II_], list_List, SpinorDottedMV[$up][i_, II_]]] := +MassTilde[i] TraceChain[list];
	
	AngleSquareChain /: HoldPattern[AngleSquareChain[SpinorUndottedMV[$down][i_, II_], list1_List, a_] AngleSquareChain[SpinorUndottedMV[$up][i_, II_], list2_List, b_]] := MassUntilde[i] SquareSquareChain[a, Join[Reverse @ list1, list2], b];
	AngleSquareChain /: HoldPattern[AngleSquareChain[a_, list1_List, SpinorDottedMV[$down][i_, II_]] AngleSquareChain[b_, list2_List, SpinorDottedMV[$up][i_, II_]]] := - MassTilde[i] AngleAngleChain[a, Join[list1, Reverse @ list2], b];
	AngleSquareChain /: HoldPattern[AngleSquareChain[a_, list1_List, SpinorDottedMV[$down][i_, II_]] AngleSquareChain[SpinorUndottedMV[$up][i_, II_], list2_List, b_]] := AngleSquareChain[a, Join[list1, {Momentum[i]}, list2], b];
	AngleSquareChain /: HoldPattern[AngleSquareChain[a_, list1_List, SpinorDottedMV[$up][i_, II_]] AngleSquareChain[SpinorUndottedMV[$down][i_, II_], list2_List, b_]] := -AngleSquareChain[a, Join[list1, {Momentum[i]}, list2], b];
	AngleSquareChain /: HoldPattern[AngleSquareChain[SpinorUndottedMV[$down][i_, II_], list_List, SpinorDottedMV[$up][i_, II_]]] := -TraceChain[Prepend[list, Momentum[i]]];
	AngleSquareChain /: HoldPattern[AngleSquareChain[SpinorUndottedMV[$up][i_, II_], list_List, SpinorDottedMV[$down][i_, II_]]] := TraceChain[Prepend[list, Momentum[i]]];
	
	localexp=exp
]


(* ::Subsection::Closed:: *)
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
        ReplaceRepeated[exp,{HoldPattern[SpinorUndottedMV[][a_]]:>Sum[SpinorUndottedMV[$up][a,I]z\[Zeta][a,I],{I,2}],HoldPattern[SpinorDottedMV[][a_]]:>Sum[SpinorDottedMV[$up][a,I]z\[Zeta][a,I],{I,2}]}],
        Sort@DeleteDuplicates@Cases[exp,SpinorUndottedMV[][i_]|SpinorDottedMV[][i_]:>i,All]
    ]


(* ::Subsubsection::Closed:: *)
(*z\[Zeta] variables*)


(*z\[Zeta]Box[a_,I_] :=
	TemplateBox[{a,I}, "z\[Zeta]",
		DisplayFunction -> (SubscriptBox["z", RowBox[{RowBox[{#1}],",",RowBox[{#2}]}]]&),
		InterpretationFunction -> (RowBox[{"z\[Zeta]","[", RowBox[{#1}],",",RowBox[{#2}],"]"}]&)
	]

z\[Zeta] /: MakeBoxes[z\[Zeta][a_,I_], StandardForm | TraditionalForm] := z\[Zeta]Box[ToBoxes[a],ToBoxes[I]]*)


(* ::Subsubsection:: *)
(*SpinorComponent*)


SetAttributes[SpinorComponent,Listable]

SpinorComponent[exp_] :=
	Block[{localexp,SpinorUndottedMV,SpinorDottedMV},
		SpinorUndottedMV[][a_]:=SpinorUndottedMV[$up][a,1];
		SpinorDottedMV[][a_]:=SpinorDottedMV[$up][a,1];
		localexp=exp
	]


(* ::Subsubsection::Closed:: *)
(*SpinorComponentSum*)


(*Options[SpinorComponentSum]={SingleComponent->False}

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
		]*)


(* ::Subsection:: *)
(*OpenTraces*)


(* ::Subsubsection::Closed:: *)
(*Auxiliary function: OpenTrace*)


OpenTrace[AngleAngleChain[a_,list_List,b_],n_Integer]:=
	(
		AngleB[a,SpinorUndottedMV[$up][#[[1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>"1"]]]*
		Product[AngleB[SpinorUndottedMV[$up][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorUndottedMV[$up][#[[i+1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i+1]]]],{i,2,Length@#-1,2}]*
		Product[SquareB[SpinorDottedMV[$down][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorDottedMV[$down][#[[i+1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i+1]]]],{i,1,Length@#-1,2}]*
		AngleB[SpinorUndottedMV[$up][#[[-1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[Length[#]]]],b]
	)&@
		(Part[#,1]&/@list)

OpenTrace[SquareSquareChain[a_,list_List,b_],n_Integer]:=
	(
		SquareB[a,SpinorDottedMV[$down][#[[1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>"1"]]]*
		Product[SquareB[SpinorDottedMV[$down][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorDottedMV[$down][#[[i+1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i+1]]]],{i,2,Length@#-1,2}]*
		Product[AngleB[SpinorUndottedMV[$up][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorUndottedMV[$up][#[[i+1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i+1]]]],{i,1,Length@#-1,2}]*
		SquareB[SpinorDottedMV[$down][#[[-1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[Length[#]]]],b]
	)&@
		(Part[#,1]&/@list)
		
OpenTrace[AngleSquareChain[a_,list_List,b_],n_Integer]:=
	(
		AngleB[a,SpinorUndottedMV[$up][#[[1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>"1"]]]*
		Product[SquareB[SpinorDottedMV[$down][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorDottedMV[$down][#[[i+1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i+1]]]],{i,1,Length@#-1,2}]*
		Product[AngleB[SpinorUndottedMV[$up][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorUndottedMV[$up][#[[i+1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i+1]]]],{i,2,Length@#-1,2}]*
		SquareB[SpinorDottedMV[$down][#[[-1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[Length[#]]]],b]
	)&@
		(Part[#,1]&/@list)

OpenTrace[TraceChain[list_List],n_Integer]:=
	(
		Product[AngleB[SpinorUndottedMV[$up][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorUndottedMV[$up][#[[i+1]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i+1]]]],{i,1,Length@#-1,2}]*
		Product[SquareB[SpinorDottedMV[$down][#[[i]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[i]]],SpinorDottedMV[$down][#[[Mod[i+1,Length[#]]]],ToExpression["\[CurlyCapitalUpsilon]"<>ToString[n]<>ToString[Mod[i+1,Length[#]]]]]],{i,2,Length@#,2}]
	)&@
		(Part[#,1]&/@list)
		
OpenTrace[exp_,n_]:=exp


(* ::Subsubsection::Closed:: *)
(*OpenTraces*)


OpenTraces[exp_Plus]:=Plus@@(OpenTraces/@(List@@exp))
OpenTraces[exp_Power]/;exp[[2]]>=0:=Block[{i=1},Times@@(OpenTrace[#,i++]&/@(ConstantArray[exp[[1]],exp[[2]]]))]
OpenTraces[exp_Times]:=Block[{i=1},Times@@(OpenTrace[#,i++]&/@Flatten[ReplaceAll[List@@exp,Power[x_,y_]/;y>=0:>ConstantArray[x,y]]])]
OpenTraces[exp_?(MatchQ[Head[#],AngleAngleChain|SquareSquareChain|AngleSquareChain|TraceChain]&)]:=OpenTrace[exp,1]
OpenTraces[exp_]:=exp


(* ::Subsection::Closed:: *)
(*UnboldSpinors*)


Options[UnboldSpinors]={SpinorSubset->False,LGPosition->$up,LGSymmetric->True};

UnboldSpinors[exp_List,opts:OptionsPattern[]]:=UnboldSpinors[#,opts]&/@exp

UnboldSpinors[exp_Plus,opts:OptionsPattern[]]:=Plus@@(UnboldSpinors[#,opts]&/@(List@@exp))

UnboldSpinors[exp_,OptionsPattern[]]:=
	Block[{i=1,j=1,tab={},particles,localexp=exp,DelayedTimes},
		localexp=localexp/.Power[x_,y_]/;y>0&&y\[Element]Integers:>DelayedTimes[Sequence@@ConstantArray[x,y]];
		If[
			ListQ[OptionValue[SpinorSubset]],
			particles=OptionValue[SpinorSubset],
			particles=Labels[exp]
		];
		Do[
			localexp=ReplaceRepeated[localexp(*exp, instead than localexp, just gives the last replacement in Do*),
					{
						SpinorUndottedMV[][a]:>SpinorUndottedMV[OptionValue[LGPosition]][a,ToExpression["\[CapitalPi]"<>ToString[i++]]],
						SpinorDottedMV[][a]:>SpinorDottedMV[OptionValue[LGPosition]][a,ToExpression["\[CapitalPi]"<>ToString[i++]]]
					}
				];
			tab=Append[tab,Table[ToExpression["\[CapitalPi]"<>ToString[k]],{k,j,i-1}]];
			j=i,
			{a,particles}
		];
		localexp=localexp/.DelayedTimes->Times;
		localexp=If[OptionValue[LGSymmetric],MultipleSymmetrise[localexp,Sequence@@tab],localexp]
	]


(* ::Subsection::Closed:: *)
(*BoldSpinors*)


Options[BoldSpinors]={SpinorSubset->False};

BoldSpinors[exp_,OptionsPattern[]]:=
	Block[{SpinorDottedMV,SpinorUndottedMV,localexp=ContractLittleGroup[exp]},
		If[
			ListQ[OptionValue[SpinorSubset]],
			
			SpinorDottedMV[pos_][lab_,J_]/;MemberQ[OptionValue[SpinorSubset],lab]:=SpinorDottedMV[][lab];
			SpinorUndottedMV[pos_][lab_,J_]/;MemberQ[OptionValue[SpinorSubset],lab]:=SpinorUndottedMV[][lab],
			
			SpinorDottedMV[pos_][lab_,J_]:=SpinorDottedMV[][lab];
			SpinorUndottedMV[pos_][lab_,J_]:=SpinorUndottedMV[][lab]
		];
		localexp(*=localexp*)
	]


(* ::Subsection::Closed:: *)
(*SpinorDerivative*)


InvertPosition[$down]:=$up;
InvertPosition[$up]:=$down;


SpinorDerivative[sum_Plus,spin_]:=Plus@@(SpinorDerivative[#,spin]&/@List@@sum)
SpinorDerivative[Times[a_,b_],spin_]:=SpinorDerivative[a,spin]*Times[b]+a*SpinorDerivative[b,spin]
SpinorDerivative[Power[a_,b_],spin_]:=b*Power[a,b-1]*SpinorDerivative[a,spin]
SpinorDerivative[x_?NumericQ,spin_]:=0

SpinorDerivative[SpinorDottedML[pos1_][lab_,ind1_],SpinorDottedML[pos2_][lab_,ind2_]]:=EpsilonLorentzDotted[pos1,InvertPosition[pos2]][ind1,ind2]
SpinorDerivative[SpinorUndottedML[pos1_][lab_,ind1_],SpinorUndottedML[pos2_][lab_,ind2_]]:=EpsilonLorentzUndotted[pos1,InvertPosition[pos2]][ind1,ind2]
SpinorDerivative[SpinorDottedMV[pos1_,pJ1_][lab_,ind1_,J1_],SpinorDottedMV[pos2_,pJ2_][lab_,ind2_,J2_]]:=EpsilonLorentzDotted[pos1,InvertPosition[pos2]][ind1,ind2]EpsilonSpin[pJ1,InvertPosition[pJ2]][J1,J2]
SpinorDerivative[SpinorUndottedMV[pos1_,pJ1_][lab_,ind1_,J1_],SpinorUndottedMV[pos2_,pJ2_][lab_,ind2_,J2_]]:=EpsilonLorentzUndotted[pos1,InvertPosition[pos2]][ind1,ind2]EpsilonSpin[pJ1,InvertPosition[pJ2]][J1,J2]
SpinorDerivative[exp_?(MatchQ[Head[#],AngleAngleChain|SquareSquareChain|AngleSquareChain|TraceChain]&),spinor_]:=ContractLittleGroup[SpinorDerivative[OpenTrace[exp,1],spinor]]

SpinorDerivative[SquareB[SpinorDottedML[][l1_],SpinorDottedML[][l2_]],spin_]:=(*Module, not Block, because we want to distinguish the local variables*)
	Module[{a1,a2},Expand[EpsilonLorentzDotted[$up,$up][a1,a2]SpinorDerivative[SpinorDottedML[$down][l1,a1]*SpinorDottedML[$down][l2,a2],spin]]]
SpinorDerivative[SquareB[SpinorDottedML[][l1_],SpinorDottedMV[pos2_][l2_,J2_]],spin_]:=Module[{a1,a2},Expand[EpsilonLorentzDotted[$up,$up][a1,a2]SpinorDerivative[SpinorDottedML[$down][l1,a1]*SpinorDottedMV[$down,pos2][l2,a2,J2],spin]]]
SpinorDerivative[SquareB[SpinorDottedMV[pos1_][l1_,J1_],SpinorDottedMV[pos2_][l2_,J2_]],spin_]:=Module[{a1,a2},Expand[EpsilonLorentzDotted[$up,$up][a1,a2]SpinorDerivative[SpinorDottedMV[$down,pos1][l1,a1,J1]*SpinorDottedMV[$down,pos2][l2,a2,J2],spin]]]

SpinorDerivative[AngleB[SpinorUndottedML[][l1_],SpinorUndottedML[][l2_]],spin_]:=Module[{a1,a2},Expand[EpsilonLorentzUndotted[$down,$down][a1,a2]SpinorDerivative[SpinorUndottedML[$up][l1,a1]*SpinorUndottedML[$up][l2,a2],spin]]]
SpinorDerivative[AngleB[SpinorUndottedML[][l1_],SpinorUndottedMV[pos2_][l2_,J2_]],spin_]:=Module[{a1,a2},Expand[EpsilonLorentzUndotted[$down,$down][a1,a2]SpinorDerivative[SpinorUndottedML[$up][l1,a1]*SpinorUndottedMV[$up,pos2][l2,a2,J2],spin]]]
SpinorDerivative[AngleB[SpinorUndottedMV[pos1_][l1_,J1_],SpinorUndottedMV[pos2_][l2_,J2_]],spin_]:=Module[{a1,a2},Expand[EpsilonLorentzUndotted[$down,$down][a1,a2]SpinorDerivative[SpinorUndottedMV[$up,pos1][l1,a1,J1]*SpinorUndottedMV[$up,pos2][l2,a2,J2],spin]]]

SpinorDerivative[exp_,spin_]:=0


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


Protect @@ Names["SpinorHelicity`*"]

EndPackage[]
