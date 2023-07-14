(* ::Package:: *)

BeginPackage["NumericalKinematics`"]


(* ::Section:: *)
(*Messages*)


NSpinorDottedML::usage = "..."
NSpinorUndottedML::usage = "..."
NSpinorDottedMV::usage = "..."
NSpinorUndottedMV::usage = "..."
NMomentum::usage = "..."

NEpsilon::usage = "..."

ClearKinematics::usage = "..."
GenerateKinematics::usage = "..."
MassiveParticles::usage = "..."
Identical::usage = "..."
Masses::usage = "..."
InternalTwistors::usage = "..."
HighestNumber::usage = "..."
Chirality::usage = "..."
Echos::usage = "..."
ReturnTwistors::usage = "..."


(* ::Section:: *)
(*Numerical Kinematics*)


(* ::Text:: *)
(*TODOs:*)


(*- Option "Label" in GenerateKinematics*)

(*- Option "MomentumConservation"->False in GenerateKinematics*)

Begin["`Private`"]


(* ::Subsection:: *)
(*Numerical variables*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


NSpinorUndottedMLBox[label_] :=
	TemplateBox[{label}, "NSpinorUndottedML",
		DisplayFunction -> (SubscriptBox["\[Lambda]", RowBox[{#}]]&),
		InterpretationFunction -> (RowBox[{"NSpinorUndottedML","[", RowBox[{#}], "]"}]&)
	]

NSpinorDottedMLBox[label_] :=
	TemplateBox[{label}, "NSpinorDottedML",
		DisplayFunction -> (SubscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#}]]&), 
		InterpretationFunction -> (RowBox[{"NSpinorDottedML", "[", RowBox[{#}], "]"}]&)
	]

NSpinorUndottedMVBox[label_, indexSpin_] :=
	TemplateBox[{label, indexSpin}, "NSpinorUndottedMV", 
		DisplayFunction -> (SubsuperscriptBox["\[Lambda]", RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"NSpinorUndottedMV", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

NSpinorDottedMVBox[label_, indexSpin_] :=
	TemplateBox[{label, indexSpin}, "NSpinorDottedMV", 
		DisplayFunction -> (SubsuperscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"NSpinorDottedMV", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

NMomentumBox[label_] :=
	TemplateBox[{label}, "NMomentum",
		DisplayFunction -> (SubscriptBox["p", RowBox[{#}]]&), 
		InterpretationFunction -> (RowBox[{"NMomentum", "[", RowBox[{#}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


NSpinorUndottedML /: MakeBoxes[NSpinorUndottedML[a_], StandardForm | TraditionalForm] := NSpinorUndottedMLBox[ToBoxes[a]]

NSpinorDottedML /: MakeBoxes[NSpinorDottedML[a_], StandardForm | TraditionalForm ] := NSpinorDottedMLBox[ToBoxes[a]]

NSpinorUndottedMV /: MakeBoxes[NSpinorUndottedMV[a_, b_], StandardForm | TraditionalForm] := NSpinorUndottedMVBox[ToBoxes[a], ToBoxes[b]]

NSpinorDottedMV /: MakeBoxes[NSpinorDottedMV[a_, b_], StandardForm | TraditionalForm] := NSpinorDottedMVBox[ToBoxes[a], ToBoxes[b]]

NMomentum /: MakeBoxes[NMomentum[a_], StandardForm | TraditionalForm] := NMomentumBox[ToBoxes[a]]


(* ::Subsection:: *)
(*Auxiliary functions*)


MomentumTwistor[p_Integer, d_Integer:4] := (*RandomInteger[{0, p}, {d}]*)RandomSample[Range[0,p],d]

MomentumTwistors[n_Integer, p_Integer] := (*RandomInteger[{0, p}, {n, 4}]*)Table[MomentumTwistor[p],n]

TwoBracket[twi1_List, twi2_List] := twi1[[1]] * twi2[[2]] - twi1[[2]] * twi2[[1]]

OrthogonalTwistor[twi1_List, twi2_List, twi3_List] :=
	Table[
		Sum[
			If[
				DuplicateFreeQ[{i, j, k, l}],
				Signature[{i, j, k, l}],
				0
			] * twi1[[j]] * twi2[[k]] * twi3[[l]],
			{j, 4},
			{k, 4},
			{l, 4}
		],
		{i, 4}
	]

FourBracket[twi1_List, twi2_List, twi3_List, twi4_List] :=
	Sum[
		If[
			DuplicateFreeQ[{i, j, k, l}],
			Signature[{i, j, k, l}],
			0
		] * twi1[[i]] * twi2[[j]] * twi3[[k]] * twi4[[l]],
		{i, 4},
		{j, 4},
		{k, 4},
		{l, 4}
	]

DualTwistors[twistors_] :=
	Table[
		OrthogonalTwistor[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 1]]], twistors[[Mod[i + 1, #, 1]]]] / (TwoBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 1]]]] * TwoBracket[twistors[[Mod[i, #, 1]]], twistors[[Mod[i + 1, #, 1]]]]),
		{i, #}
	]&@Length[twistors]

PlanarKinematics[twistors_] :=
	Table[
		FourBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #,1]]], twistors[[Mod[i + 1, #, 1]]], twistors[[Mod[i + 2, #, 1]]]] / (TwoBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 1]]]] * TwoBracket[twistors[[Mod[i + 1, #, 1]]], twistors[[Mod[i + 2, #, 1]]]]),
		{i, #}
	]&@Length[twistors]

CheckKinematics[twistors_List] :=
	If[MemberQ[#, 0],
			True,
			False
		]&@Flatten@(Table[TwoBracket[twistors[[i]], twistors[[j]]], {i,#}, {j, i + 1, #}]&@Length[twistors])

NEpsilon[a_:1] /; NumberQ[a] := {{0, a}, {-a, 0}}


(* ::Subsection::Closed:: *)
(*Clear Kinematics*)


ClearKinematics[] :=
	(
		ClearAll[NSpinorDottedML, NSpinorUndottedML, NSpinorDottedMV, NSpinorUndottedMV, NMomentum];
		NSpinorUndottedML /: MakeBoxes[NSpinorUndottedML[a_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorUndottedMLBox[ToBoxes[a]];
		NSpinorDottedML /: MakeBoxes[NSpinorDottedML[a_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorDottedMLBox[ToBoxes[a]];
		NSpinorUndottedMV /: MakeBoxes[NSpinorUndottedMV[a_, b_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorUndottedMVBox[ToBoxes[a], ToBoxes[b]];
		NSpinorDottedMV /: MakeBoxes[NSpinorDottedMV[a_, b_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorDottedMVBox[ToBoxes[a], ToBoxes[b]];
		NMomentum /: MakeBoxes[NMomentum[a_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NMomentumBox[ToBoxes[a]]
	)



(* ::Subsection::Closed:: *)
(*Generate Kinematics*)


Options[GenerateKinematics]=
	{
		MassiveParticles->{},
		Identical->{},
		Masses->{}(*we give rules for fixed masses, the rules must be associated with the first element of the Identical lists*),
		InternalTwistors->{}(*for the moment is just for factorisation channels, then we have just one argument. Prototype: {fixed momentum,{{twistor before},{twistor(s)},{twistor after}}}*),
		HighestNumber->20,
		Chirality->"Minus"(*or "Plus"*),
		Echos->True,
		ReturnTwistors->{}
	};

GenerateKinematics[particles_?(MatchQ[Head[#],Integer|List]&),OptionsPattern[]]:=
	Module[
		{
			twistors,
			duals,
			massive=If[MatchQ[Head[particles],List],Flatten[Position[particles,#]&/@OptionValue[MassiveParticles]],OptionValue[MassiveParticles]],
			n=If[MatchQ[Head[particles],List],Length@particles,particles],
			identicals=If[MatchQ[Head[particles],List],Flatten[Position[particles,#]&/@#]&/@OptionValue[Identical],OptionValue[Identical]],
			x,
			y,
			z,
			ret
		},

		(*ClearKinematics[];*)

		twistors=MomentumTwistors[n,OptionValue[HighestNumber]];


		While[
			CheckKinematics[twistors],
			twistors=MomentumTwistors[n,OptionValue[HighestNumber]]
		];

twistors=List/@twistors;

If[MatchQ[massive,{}],

If[\[Not]MatchQ[OptionValue[InternalTwistors],{}],
y=Position[particles,OptionValue[InternalTwistors][[1]]][[1,1]];
twistors=ReplacePart[twistors,{If[y!=1,y-1,-1],-1}->OptionValue[InternalTwistors][[2,3]]];
twistors=ReplacePart[twistors,{y}->Reverse@OptionValue[InternalTwistors][[2,2]]];
twistors=ReplacePart[twistors,{If[y!=n,y+1,+1],1}->OptionValue[InternalTwistors][[2,1]]];
];

duals=List/@DualTwistors[Sequence@@@twistors];

massive=Table[1,n], (*the fully massless case is the simplest*)

duals=MapAt[Append[#,MomentumTwistor[OptionValue[HighestNumber]]]&,twistors,List/@massive];

While[
CheckKinematics[Sequence@@@duals],
duals=MapAt[Append[#,MomentumTwistor[OptionValue[HighestNumber]]]&,twistors,List/@massive];
];

twistors=duals; (*temporarily, duals are the usual twistors, because we need to insert the additional (massive) ones and check them every time*)
massive=Length/@duals;

If[\[Not]MatchQ[OptionValue[InternalTwistors],{}],
y=Position[particles,OptionValue[InternalTwistors][[1]]][[1,1]];
twistors=ReplacePart[twistors,{If[y!=1,y-1,-1],-1}->OptionValue[InternalTwistors][[2,3]]];
twistors=ReplacePart[twistors,{y}->Reverse@OptionValue[InternalTwistors][[2,2]]];
twistors=ReplacePart[twistors,{If[y!=n,y+1,+1],1}->OptionValue[InternalTwistors][[2,1]]];
];

If[\[Not]MatchQ[OptionValue[Identical],{}],

twistors=ReplacePart[twistors,Thread[Rule[#,Table[x[i],{i,Length[#]}]]]]&@(Prepend[{2,4},#]&/@Flatten[(*Drop[#,1]&/@*)identicals(*OptionValue[Identical]*)]);

duals=If[Length[#]==2,#[[1]],0]&/@FoldPairList[TakeDrop,PlanarKinematics[Sequence@@@twistors],massive]; (*the masses*)

If[MatchQ[OptionValue[Masses],{}],

duals=
Flatten@
Solve[
Equal@@@
Join[
Thread[{duals[[#]]&/@(#[[1]]&/@identicals),RandomInteger[{1,OptionValue[HighestNumber]},Length@identicals]}],
Map[duals[[#]]&,Flatten[Subsequences[#,{2}]&/@identicals(*OptionValue[Identical]*),1],{2}]
]
],

If[\[Not]SubsetQ[#[[1]]&/@OptionValue[Identical],#[[1]]&/@OptionValue[Masses]],Message[GenerateKinematics::InvMasses];Return[Null]];

duals=
Flatten@
Solve[
Equal@@@
Join[
(MapAt[duals[[#]]&,#,{1}]&/@
(DeleteDuplicates[#,MatchQ[#1[[1]],#2[[1]]]&]&@
Join[
Transpose@MapAt[Flatten@Position[particles,Alternatives@@#]&,Transpose[List@@@OptionValue[Masses]],{1}],
Transpose@{(#[[1]]&/@identicals),RandomInteger[{1,OptionValue[HighestNumber]},Length@identicals]}
])),
Map[duals[[#]]&,Flatten[Subsequences[#,{2}]&/@identicals(*OptionValue[Identical]*),1],{2}]
]
]

];

twistors=twistors/.duals;

];

If[\[Not]FreeQ[twistors,x[_]],
GenerateKinematics[particles,MassiveParticles->OptionValue[MassiveParticles],Identical->OptionValue[Identical],Masses->OptionValue[Masses],HighestNumber->OptionValue[HighestNumber],InternalTwistors->OptionValue[InternalTwistors],Chirality->OptionValue[Chirality],Echos->OptionValue[Echos]];
Return[Null]
];

duals=FoldPairList[TakeDrop,DualTwistors[Sequence@@@twistors],massive]

];

If[
\[Not]MatchQ[OptionValue[ReturnTwistors],{}],
z=Flatten[Position[particles,OptionValue[ReturnTwistors][[1]]]][[1]];
ret=List[OptionValue[ReturnTwistors][[1]],{twistors[[z-1,-1]],twistors[[z]],twistors[[z+1,1]]}],
{}
];(*it works only for one twistor to return*)

twistors=Map[Part[#,;;2]&,twistors,{2}];
duals=Map[Part[#,3;;4]&,duals,{2}];

If[
(CheckKinematics[Sequence@@@twistors]||CheckKinematics[Sequence@@@duals])&&n>3,

GenerateKinematics[particles,MassiveParticles->OptionValue[MassiveParticles],Identical->OptionValue[Identical],Masses->OptionValue[Masses],HighestNumber->OptionValue[HighestNumber],InternalTwistors->OptionValue[InternalTwistors],Chirality->OptionValue[Chirality],Echos->OptionValue[Echos]],

x=If[MatchQ[Head[particles],List],particles,Range@n];

x=
Flatten@{
Table[If[massive[[i]]==1,{NSpinorUndottedML[x[[i]]]},{NSpinorUndottedMV[x[[i]],1],NSpinorUndottedMV[x[[i]],2]}],{i,n}],
Table[If[massive[[i]]==1,{NSpinorDottedML[x[[i]]]},{NSpinorDottedMV[x[[i]],1],NSpinorDottedMV[x[[i]],2]}],{i,n}],
Table[NMomentum[x[[i]]],{i,n}]
};

If[MatchQ[OptionValue[Chirality],"Plus"],{twistors,duals}={duals,twistors}];

massive=
Plus@@@
FoldPairList[
TakeDrop,
(TensorProduct[Sequence@@#]&/@Transpose[{Sequence@@@twistors,Sequence@@@duals}]),
massive
];

If[\[Not]MatchQ[OptionValue[InternalTwistors],{}],
x=DeleteCases[x,_?(\[Not]FreeQ[#,particles[[y]]]&)];
twistors=Delete[twistors,y];
duals=Delete[duals,y];
massive=Delete[massive,y];
];

twistors=Flatten[twistors,1];

duals=If[Length[#]==2,Reverse[{-1,1}*#],#]&/@duals;

Set@@@(If[OptionValue[Echos],Echo[#],#]&@Thread[Rule[x,Flatten[#,1]&@{twistors,Flatten[duals,1],massive}]]);

If[\[Not]MatchQ[OptionValue[ReturnTwistors],{}],Return[ret]]
];

];

GenerateKinematics::InvMasses="'Masses' can be assigned only to the first elements in the sublists of 'Identicals'";


(* ::Subsection::Closed:: *)
(*End*)


End[]



(* ::Section:: *)
(*Attributes*)


Protect@@
	DeleteCases[
		Names["NumericalKinematics`*"],
		_?(MatchQ[#,Alternatives @@ (ToString /@ {NSpinorDottedML, NSpinorUndottedML, NSpinorDottedMV, NSpinorUndottedMV, NMomentum})]&)
	]

EndPackage[]
