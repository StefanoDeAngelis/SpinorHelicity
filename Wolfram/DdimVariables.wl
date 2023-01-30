(* ::Package:: *)

BeginPackage["DdimVariables`",{"DdimFunctions`"}]


(* ::Section:: *)
(*Messages*)


Metric::usage = "..."

EpsilonPol::usage = "..."
FieldStr::usage = "..."
Riemann::usage = "..."
Momentum::usage = "..."

FTrace::usage = "..."

Mass::usage = "..."

Mandelstam::usage = "..."
DotProduct::usage = "..."

DD::usage = "..."
ToLabel::usage = "..."


(* ::Section:: *)
(*Spinor Helicity Variables*)


(*
Features:
	- complex polarisation vector
*)


Begin["`Private`"]


(* ::Subsection:: *)
(*toLabel*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


ToLabelBox[a_,b_] :=
    TemplateBox[{a,b},"ToLabel",
        DisplayFunction -> (SubscriptBox[RowBox[{#1}],RowBox[{#2}]]&),
        InterpretationFunction -> (RowBox[{#1<>#2}]&)]
        
ToLabelBox[a_] :=
    TemplateBox[{a},"ToLabel",
        DisplayFunction -> (RowBox[{#}]&),
        InterpretationFunction -> (RowBox[{#}]&)]


(* ::Subsubsection::Closed:: *)
(*Properties*)


ToLabel[a_]:=
	If[
		StringLength[#]>1
			(*&&DigitQ[StringDrop[#,1]]*)
			&&LetterQ[StringPart[#,1]],
		ToLabelBox[ToBoxes[ToExpression[StringPart[#,1]]],ToBoxes[ToExpression[StringDrop[#,1]]]],
		ToLabelBox[ToBoxes[a]]
	]&@ToString[a]


(* ::Subsection:: *)
(*Metric*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MetricBox[\[Mu]_,\[Nu]_] :=
    TemplateBox[{\[Mu],\[Nu]}, "Metric",
        DisplayFunction -> (SuperscriptBox["\[Eta]",RowBox[{##}]]&),
        InterpretationFunction -> (RowBox[{"Metric","[",RowBox[{#1,",",#2}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "metric" -> MetricBox["\[SelectionPlaceholder]","\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Metric /: MakeBoxes[Metric[\[Mu]_,\[Nu]_], StandardForm | TraditionalForm] := MetricBox[ToBoxes[\[Mu]],ToBoxes[\[Nu]]]

DD /: MakeBoxes[DD,StandardForm | TraditionalForm] := TemplateBox[{"D"},"DD",DisplayFunction->(RowBox[{#}]&),InterpretationFunction->(RowBox[{"DD"}]&)]


Metric[\[Mu]_,\[Nu]_] /; \[Not]OrderedQ[{\[Mu],\[Nu]}] := Metric[\[Nu],\[Mu]]

Metric[\[Mu]_,\[Mu]_] := DD;

Metric /: Metric[\[Mu]_,\[Nu]_]^2 := DD;


Metric /: Metric[\[Mu]_,\[Nu]_] Metric[\[Rho]_,\[Nu]_] := Metric[\[Mu],\[Rho]]
Metric /: Metric[\[Mu]_,\[Nu]_] Metric[\[Nu]_,\[Rho]_] := Metric[\[Mu],\[Rho]]
Metric /: Metric[\[Mu]_,\[Nu]_] Metric[\[Mu]_,\[Rho]_] := Metric[\[Nu],\[Rho]]

Metric /: Metric[\[Mu]_,\[Nu]_] Momentum[a_,\[Nu]_] := Momentum[a,\[Mu]]
Metric /: Metric[\[Mu]_,\[Nu]_] Momentum[a_,\[Mu]_] := Momentum[a,\[Nu]]

Metric /: Metric[\[Mu]_,\[Nu]_] EpsilonPol[a_,\[Rho]___,\[Nu]_,\[Sigma]___] := EpsilonPol[a,\[Rho],\[Mu],\[Sigma]]
Metric /: Metric[\[Nu]_,\[Rho]_] EpsilonPol[a_,\[Mu]___,\[Nu]_,\[Sigma]___] := EpsilonPol[a,\[Mu],\[Rho],\[Sigma]]


(* ::Subsection:: *)
(*Polarisation tensor*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


EpsilonPolBox[a_,\[Mu]__] :=
    TemplateBox[{a,\[Mu]}, "EpsilonPol",
        DisplayFunction -> (SubsuperscriptBox["\[CurlyEpsilon]",RowBox[{#1}],RowBox[{##2}]]&),
        InterpretationFunction -> (RowBox[{"EpsilonPol","[",RowBox[{#1}],",",TemplateSlotSequence[2,","],"]"}]&)] (*The RowBox of ##2 gives a problem when we copy and paste the 
        EpsilonPol with two indices*)

EpBox[a_] :=
    TemplateBox[{a}, "EpsilonPol",
        DisplayFunction -> (SubscriptBox["\[CurlyEpsilon]",RowBox[{#1}]]&),
        InterpretationFunction -> (RowBox[{"EpsilonPol","[",RowBox[{#1}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "eps" -> EpsilonPolBox["\[SelectionPlaceholder]","\[Placeholder]"]]]


(* ::Subsubsection:: *)
(*Properties*)


EpsilonPol /: MakeBoxes[EpsilonPol[a_,\[Mu]__], StandardForm | TraditionalForm] := EpsilonPolBox[ToLabel[a],Sequence@@(ToBoxes/@{\[Mu]})]
EpsilonPol /: MakeBoxes[EpsilonPol[a_], StandardForm | TraditionalForm] := EpBox[ToLabel[a]]


EpsilonPol[a_,\[Mu]__] /; \[Not]OrderedQ[{\[Mu]}] := EpsilonPol[a,Sequence@@Sort[{\[Mu]}]]
EpsilonPol[a_,\[Mu]_,\[Mu]_] := 0

(*EpsilonPol /: EpsilonPol[a_,\[Mu]__] Momentum[a_,\[Nu]_] /; MemberQ[{\[Mu]},\[Nu]] := 0*)
EpsilonPol /: EpsilonPol[a_,\[Mu]__] EpsilonPol[a_,\[Nu]__] /; \[Not]MatchQ[Complement[{\[Mu]},{\[Nu]}],{\[Mu]}] := 0


(* ::Subsection:: *)
(*Field strength tensor*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


FieldStrBox[a_,\[Mu]_,\[Nu]_] :=
    TemplateBox[{a,\[Mu],\[Nu]}, "FieldStr",
        DisplayFunction -> (SubsuperscriptBox["F",RowBox[{#1}],RowBox[{#2,#3}]]&),
        InterpretationFunction -> (RowBox[{"FieldStr","[",RowBox[{#1}],",",RowBox[{#2}],",",RowBox[{#3}],"]"}]&)]
        
FStrBox[a_] :=
    TemplateBox[{a}, "FieldStr",
        DisplayFunction -> (SubscriptBox["F",RowBox[{#1}]]&),
        InterpretationFunction -> (RowBox[{"FieldStr","[",RowBox[{#1}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "fieldst" -> FieldStrBox["\[SelectionPlaceholder]","\[Placeholder]","\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


FieldStr /: MakeBoxes[FieldStr[a_,\[Mu]_,\[Nu]_], StandardForm | TraditionalForm] := FieldStrBox[ToLabel[a],ToBoxes[\[Mu]],ToBoxes[\[Nu]]]
FieldStr /: MakeBoxes[FieldStr[a_], StandardForm | TraditionalForm] := FStrBox[ToLabel[a]]


FieldStr[a_,\[Mu]_,\[Nu]_] /; \[Not]OrderedQ[{\[Mu],\[Nu]}] := - FieldStr[a,\[Nu],\[Mu]]
FieldStr[a_,\[Mu]_,\[Mu]_] := 0

(*FieldStr /: FieldStr[a_,\[Mu]_,\[Nu]_] Momentum[a_,\[Rho]_] /; MemberQ[{\[Mu],\[Nu]},\[Rho]] := 0
FieldStr /: FieldStr[a_,\[Mu]_,\[Nu]_] FieldStr[a_,\[Rho]_,\[Sigma]_] /; \[Not]DuplicateFreeQ[{\[Mu],\[Nu],\[Rho],\[Sigma]}] := 0*)


(* ::Subsection:: *)
(*Riemann tensor*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


RiemannBox[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] :=
    TemplateBox[{a,\[Mu],\[Nu],\[Rho],\[Sigma]}, "Riemann",
        DisplayFunction -> (SubsuperscriptBox["R",RowBox[{#1}],RowBox[{#2,#3,#4,#5}]]&),
        InterpretationFunction -> (RowBox[{"Riemann","[",RowBox[{#1}],",",RowBox[{#2}],",",RowBox[{#3}],",",RowBox[{#4}],",",RowBox[{#5}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "riemann" -> RiemannBox["\[SelectionPlaceholder]","\[Placeholder]","\[Placeholder]","\[Placeholder]","\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Riemann /: MakeBoxes[Riemann[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_], StandardForm | TraditionalForm] := RiemannBox[ToLabel[a],ToBoxes[\[Mu]],ToBoxes[\[Nu]],ToBoxes[\[Rho]],ToBoxes[\[Sigma]]]


Riemann[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]OrderedQ[{\[Mu],\[Nu]}] := - Riemann[a,\[Nu],\[Mu],\[Rho],\[Sigma]]
Riemann[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]OrderedQ[{\[Rho],\[Sigma]}] := - Riemann[a,\[Mu],\[Nu],\[Sigma],\[Rho]]
Riemann[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]DuplicateFreeQ[{\[Mu],\[Nu],\[Rho],\[Sigma]}] := 0
Riemann[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]OrderedQ[{\[Nu],\[Sigma]}] := Riemann[a,\[Rho],\[Sigma],\[Mu],\[Nu]]
Riemann[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; (OrderedQ[{\[Rho],\[Mu]}]&&OrderedQ[{\[Rho],\[Nu]}]) := - Riemann[a,\[Nu],\[Rho],\[Mu],\[Sigma]] - Riemann[a,\[Rho],\[Mu],\[Nu],\[Sigma]]

(*Riemann /: Riemann[a_,\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] Momentum[a_,\[Alpha]_] /; MemberQ[{\[Mu],\[Nu],\[Rho],\[Sigma]},\[Alpha]] := 0
Riemann /: Riemann[a_,\[Mu]1_,\[Nu]1_,\[Rho]1_,\[Sigma]1_] Riemann[a_,\[Mu]2_,\[Nu]2_,\[Rho]2_,\[Sigma]2_] /; \[Not]DuplicateFreeQ[{\[Mu]1,\[Nu]1,\[Rho]1,\[Sigma]1,\[Mu]2,\[Nu]2,\[Rho]2,\[Sigma]2}] := 0*)


(* ::Subsection:: *)
(*Momentum*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MomentumBox[a_,\[Mu]_] :=
    TemplateBox[{a,\[Mu]}, "Momentum",
        DisplayFunction -> (SubsuperscriptBox["p",RowBox[{#1}],RowBox[{#2}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1}],",",RowBox[{#2}],"]"}]&)]
        
MomentumBoxLetter[a_,\[Mu]_] :=
	TemplateBox[{a,\[Mu]}, "Momentum",
        DisplayFunction -> (SuperscriptBox[RowBox[{#1}],RowBox[{#2}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1}],",",RowBox[{#2}],"]"}]&)]
        
MomentumBoxLetterNumber[a_,b_,\[Mu]_] :=
	TemplateBox[{a,b,\[Mu]}, "Momentum",
        DisplayFunction -> (SubsuperscriptBox[RowBox[{#1}],RowBox[{#2}],RowBox[{#3}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1<>#2}],",",RowBox[{#3}],"]"}]&)]

MomBox[a_] :=
    TemplateBox[{a}, "Momentum",
        DisplayFunction -> (SubscriptBox["p",RowBox[{#1}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1}],"]"}]&)]

MomBoxLetter[a_] :=
	TemplateBox[{a}, "Momentum",
        DisplayFunction -> (RowBox[{#1}]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1}],"]"}]&)]
        
MomBoxLetterNumber[a_,b_] :=
	TemplateBox[{a,b}, "Momentum",
        DisplayFunction -> (SubscriptBox[RowBox[{#1}],RowBox[{#2}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1<>#2}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mom" -> MomentumBox["\[SelectionPlaceholder]","\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Momentum /: MakeBoxes[Momentum[a_,\[Mu]_], StandardForm | TraditionalForm] := 
	If[LetterQ[StringPart[#,1]],
		If[
			StringLength[#]>1(*&&DigitQ[StringDrop[#,1]]*),
			MomentumBoxLetterNumber[ToBoxes[ToExpression[StringPart[#,1]]],ToBoxes[ToExpression[StringDrop[#,1]]],ToBoxes[\[Mu]]],
			MomentumBoxLetter[ToBoxes[a],ToBoxes[\[Mu]]]
		],
		MomentumBox[ToBoxes[a],ToBoxes[\[Mu]]]
	]&@ToString[a]
	
Momentum /: MakeBoxes[Momentum[a_], StandardForm | TraditionalForm] := 
	If[LetterQ[StringPart[#,1]],
		If[
			StringLength[#]>1(*&&DigitQ[StringDrop[#,1]]*),
			MomBoxLetterNumber[ToBoxes[ToExpression[StringPart[#,1]]],ToBoxes[ToExpression[StringDrop[#,1]]]],
			MomBoxLetter[ToBoxes[a]]
		],
		MomBox[ToBoxes[a]]
	]&@ToString[a]

Momentum[a_,\[Mu]_] /; MatchQ[Head[a],Times]&&a[[1]]==-1 := - Momentum[-a,\[Mu]]
Momentum[a_,\[Mu]_] /; a < 0 := - Momentum[-a,\[Mu]]

Momentum[x_Plus,\[Mu]_] := Plus@@(Momentum[#,\[Mu]]&/@(List@@x))

Momentum[a_] /; MatchQ[Head[a],Times]&&a[[1]]==-1 := - Momentum[-a]
Momentum[a_] /; a < 0 := - Momentum[-a]

Momentum[x_Plus] := Plus@@(Momentum[#]&/@(List@@x))

Momentum /: Momentum[a_,\[Nu]_]^2 := DotProduct[Momentum[a],Momentum[a]]


(* ::Subsection:: *)
(*FTrace*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


FTraceBox[a_,c__,b_] :=
    TemplateBox[{a,b,c}, "FTrace",
        DisplayFunction -> (RowBox[{#1,"\[CenterDot]",TemplateSlotSequence[3,"\[CenterDot]"],"\[CenterDot]",#2}]&),
        InterpretationFunction->(RowBox[{"FTrace","[",RowBox[{#1,",","{",TemplateSlotSequence[3,","],"}",",",#2}],"]"}]&)
        ]

FTraceBox[a_List] :=
    TemplateBox[{Sequence@@a}, "FTrace",
        DisplayFunction -> (RowBox[{TemplateSlotSequence[1,"\[CenterDot]"]}]&),
        InterpretationFunction->(RowBox[{"FTrace","[",RowBox[{"{",TemplateSlotSequence[1,","],"}"}],"]"}]&)
        ]


(* ::Subsubsection::Closed:: *)
(*Properties*)


FTrace /: MakeBoxes[FTrace[a_,c_List,b_], StandardForm | TraditionalForm] := FTraceBox[ToBoxes[a],Sequence@@(ToBoxes/@c),ToBoxes[b]]
FTrace /: MakeBoxes[FTrace[a_List], StandardForm | TraditionalForm] := FTraceBox[ToBoxes/@a]

FTrace[a_Plus,c_List,b_]:=FTrace[#,c,b]&/@a
FTrace[a_,c_List,b_Plus]:=FTrace[a,c,#]&/@b
FTrace[a_,{c___,d_Plus,e___},b_]:=FTrace[a,{c,#,e},b]&/@d

FTraceNotOrderedQ[{a_,b_}]:=If[MatchQ[a,b],Nothing,!OrderedQ[{a,b}]]

FTrace[a_,c_List,b_]/;!OrderedQ[{a,b}]:=(-1)^Length[c]*FTrace[b,Reverse@c,a]
FTrace[a_,c_List,a_]/;(If[MatchQ[#,{}],False,Part[#,1]]&@(FTraceNotOrderedQ/@Transpose[{c,Reverse@c}])):=(-1)^Length[c]*FTrace[a,Reverse@c,a]
FTrace[list_List]/;(First@Ordering[list]!=1):=FTrace[RotateLeft[list,First@Ordering[list]-1]]

FTrace[Times[x_,a__],c_,b_]/;!MatchQ[x,Momentum[_]|EpsilonPol[_]]:=x*FTrace[Times[a],c,b]
FTrace[a_,c_,Times[x_,b__]]/;!MatchQ[x,Momentum[_]|EpsilonPol[_]]:=x*FTrace[a,c,Times[b]]


(* ::Subsection:: *)
(*Masses*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MassBox[label_]:=
	TemplateBox[{label},"Mass",
		DisplayFunction->(SubscriptBox["M",RowBox[{#1}]]&),
		InterpretationFunction -> (RowBox[{"Mass","[",RowBox[{#1}],"]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Mass /: MakeBoxes[Mass[a_], StandardForm | TraditionalForm] := MassBox[ToLabel[a]]


(* ::Subsection:: *)
(*Mandelstam invariants*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MandelstamBox[a__] :=
    TemplateBox[{a}, "Mandelstam",
        DisplayFunction -> (SubscriptBox["s",RowBox[{##}]]&),
        InterpretationFunction -> (RowBox[{"Mandelstam","[",RowBox[{TemplateSlotSequence[1,","]}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mand" -> MandelstamBox["\[SelectionPlaceholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Mandelstam /: MakeBoxes[Mandelstam[a__], StandardForm | TraditionalForm] := MandelstamBox[Sequence@@(ToBoxes/@{a})]


Mandelstam[a__]/;\[Not]OrderedQ[{a}]:=Mandelstam[Sequence@@Sort[{a}]]


(* ::Subsection:: *)
(*Scalar product*)


(* ::Subsubsection::Closed:: *)
(*Boxes and shortcuts*)


DotProductBox[a_,b_] :=
    TemplateBox[{a,b}, "DotProduct",
        DisplayFunction -> (RowBox[{#1,"\[CenterDot]",#2}]&),
        InterpretationFunction -> (RowBox[{"DotProduct","[",RowBox[{#1,",",#2}],"]"}]&)]
        
DotProduct2Box[a_] :=
    TemplateBox[{a}, "DotProduct",
        DisplayFunction -> (SuperscriptBox[RowBox[{#1}],"2"]&),
        InterpretationFunction -> (RowBox[{"DotProduct","[",RowBox[{#1,",",#1}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "dotmm" -> DotProduct[Momentum["\[SelectionPlaceholder]"],Momentum["\[Placeholder]"]]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "dotem" -> DotProduct[EpsilonPol["\[SelectionPlaceholder]"],Momentum["\[Placeholder]"]]]]
        
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "dotee" -> DotProduct[EpsilonPol["\[SelectionPlaceholder]"],EpsilonPol["\[Placeholder]"]]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


DotProduct /: MakeBoxes[DotProduct[a_,a_], StandardForm | TraditionalForm] := DotProduct2Box[ToBoxes[a]]
DotProduct /: MakeBoxes[DotProduct[a_,b_], StandardForm | TraditionalForm] := DotProductBox[ToBoxes[a],ToBoxes[b]]


DotProduct[a_,b_] /; \[Not]OrderedQ[{a,b}] := DotProduct[b,a]

DotProduct[a_Plus,b_]:=DotProduct[#,b]&/@a
DotProduct[a_,b_Plus]:=DotProduct[a,#]&/@b

(*DotProduct[Momentum[a_],Momentum[a_]] := Mass[a]^2*)

DotProduct[Times[a_,b_,x___],c_]/;MatchQ[a,Momentum[_]|EpsilonPol[_]]&&MatchQ[b,Momentum[_]|EpsilonPol[_]]:=$Failed
DotProduct[a_,Times[b_,c_,x___]]/;MatchQ[b,Momentum[_]|EpsilonPol[_]]&&MatchQ[c,Momentum[_]|EpsilonPol[_]]:=$Failed

DotProduct[Times[x___,a_],b_]/;MatchQ[a,Momentum[_]|EpsilonPol[_]]:=Times[x]*DotProduct[a,b]
DotProduct[a_,Times[x___,b_]]/;MatchQ[b,Momentum[_]|EpsilonPol[_]]:=Times[x]*DotProduct[a,b]


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["DdimVariables`*"]


EndPackage[]
