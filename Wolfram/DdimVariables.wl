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


(* ::Section:: *)
(*Spinor Helicity Variables*)


Begin["`Private`"]


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


Metric[\[Mu]_,\[Nu]_] /; \[Not]OrderedQ[{\[Mu],\[Nu]}] := Metric[\[Nu],\[Mu]]

Metric[\[Mu]_,\[Mu]_] := DD;


Metric /: Metric[\[Mu]_,\[Nu]_] Metric[\[Rho]_,\[Nu]_] := Metric[\[Mu],\[Rho]]
Metric /: Metric[\[Mu]_,\[Nu]_] Metric[\[Nu]_,\[Rho]_] := Metric[\[Mu],\[Rho]]
Metric /: Metric[\[Nu]_,\[Mu]_] Metric[\[Nu]_,\[Rho]_] := Metric[\[Mu],\[Rho]]

Metric /: Metric[\[Mu]_,\[Nu]_] Momentum[a_][\[Nu]_] := Momentum[a][\[Mu]]
Metric /: Metric[\[Mu]_,\[Nu]_] Momentum[a_][\[Mu]_] := Momentum[a][\[Nu]]

Metric /: Metric[\[Mu]_,\[Nu]_] EpsilonPol[a_][\[Rho]___,\[Nu]_,\[Sigma]___] := EpsilonPol[a][\[Rho],\[Mu],\[Sigma]]
Metric /: Metric[\[Nu]_,\[Rho]_] EpsilonPol[a_][\[Mu]___,\[Nu]_,\[Sigma]___] := EpsilonPol[a][\[Mu],\[Rho],\[Sigma]]


(* ::Subsection:: *)
(*Polarisation tensor*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


EpsilonPolBox[a_][\[Mu]__] :=
    TemplateBox[{a,\[Mu]}, "EpsilonPol",
        DisplayFunction -> (SubsuperscriptBox["\[CurlyEpsilon]",RowBox[{#1}],RowBox[{##2}]]&),
        InterpretationFunction -> (RowBox[{"EpsilonPol","[",RowBox[{#1}],"]","[",RowBox[{##2}],"]"}]&)] (*The RowBox of ##2 gives a problem when we copy and paste the 
        EpsilonPol with two indices*)

EpBox[a_] :=
    TemplateBox[{a}, "EpsilonPol",
        DisplayFunction -> (SubscriptBox["\[CurlyEpsilon]",RowBox[{#1}]]&),
        InterpretationFunction -> (RowBox[{"EpsilonPol","[",RowBox[{#1}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "eps" -> EpsilonPolBox["\[SelectionPlaceholder]"]["\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


EpsilonPol /: MakeBoxes[EpsilonPol[a_][\[Mu]__], StandardForm | TraditionalForm] := EpsilonPolBox[ToBoxes[a]][Sequence@@(ToBoxes/@{\[Mu]})]
EpsilonPol /: MakeBoxes[EpsilonPol[a_], StandardForm | TraditionalForm] := EpBox[ToBoxes[a]]


EpsilonPol[a_][\[Mu]__] /; \[Not]OrderedQ[{\[Mu]}] := EpsilonPol[a][Sequence@@Sort[{\[Mu]}]]
EpsilonPol[a_][\[Mu]_,\[Mu]_] := 0

EpsilonPol /: EpsilonPol[a_][\[Mu]__] Momentum[a_][\[Nu]_] /; MemberQ[{\[Mu]},\[Nu]] := 0
EpsilonPol /: EpsilonPol[a_][\[Mu]__] EpsilonPol[a_][\[Nu]__] /; \[Not]MatchQ[Complement[{\[Mu]},{\[Nu]}],{\[Mu]}] := 0


(* ::Subsection:: *)
(*Field strength tensor*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


FieldStrBox[a_][\[Mu]_,\[Nu]_] :=
    TemplateBox[{a,\[Mu],\[Nu]}, "FieldStr",
        DisplayFunction -> (SubsuperscriptBox["F",RowBox[{#1}],RowBox[{#2,#3}]]&),
        InterpretationFunction -> (RowBox[{"FieldStr","[",RowBox[{#1}],"]","[",RowBox[{#2}],",",RowBox[{#3}],"]"}]&)]
        
FStrBox[a_] :=
    TemplateBox[{a}, "FieldStr",
        DisplayFunction -> (SubscriptBox["F",RowBox[{#1}]]&),
        InterpretationFunction -> (RowBox[{"FieldStr","[",RowBox[{#1}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "fieldst" -> FieldStrBox["\[SelectionPlaceholder]"]["\[Placeholder]","\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


FieldStr /: MakeBoxes[FieldStr[a_][\[Mu]_,\[Nu]_], StandardForm | TraditionalForm] := FieldStrBox[ToBoxes[a]][ToBoxes[\[Mu]],ToBoxes[\[Nu]]]
FieldStr /: MakeBoxes[FieldStr[a_], StandardForm | TraditionalForm] := FStrBox[ToBoxes[a]]


FieldStr[a_][\[Mu]_,\[Nu]_] /; \[Not]OrderedQ[{\[Mu],\[Nu]}] := - FieldStr[a][\[Nu],\[Mu]]
FieldStr[a_][\[Mu]_,\[Mu]_] := 0

FieldStr /: FieldStr[a_][\[Mu]_,\[Nu]_] Momentum[a_][\[Rho]_] /; MemberQ[{\[Mu],\[Nu]},\[Rho]] := 0
FieldStr /: FieldStr[a_][\[Mu]_,\[Nu]_] FieldStr[a_][\[Rho]_,\[Sigma]_] /; \[Not]DuplicateFreeQ[{\[Mu],\[Nu],\[Rho],\[Sigma]}] := 0


(* ::Subsection:: *)
(*Riemann tensor*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


RiemannBox[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] :=
    TemplateBox[{a,\[Mu],\[Nu],\[Rho],\[Sigma]}, "Riemann",
        DisplayFunction -> (SubsuperscriptBox["R",RowBox[{#1}],RowBox[{#2,#3,#4,#5}]]&),
        InterpretationFunction -> (RowBox[{"Riemann","[",RowBox[{#1}],"]","[",RowBox[{#2}],",",RowBox[{#3}],",",RowBox[{#4}],",",RowBox[{#5}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "riemann" -> RiemannBox["\[SelectionPlaceholder]"]["\[Placeholder]","\[Placeholder]","\[Placeholder]","\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Riemann /: MakeBoxes[Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_], StandardForm | TraditionalForm] := RiemannBox[ToBoxes[a]][ToBoxes[\[Mu]],ToBoxes[\[Nu]],ToBoxes[\[Rho]],ToBoxes[\[Sigma]]]


Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]OrderedQ[{\[Mu],\[Nu]}] := - Riemann[a][\[Nu],\[Mu],\[Rho],\[Sigma]]
Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]OrderedQ[{\[Rho],\[Sigma]}] := - Riemann[a][\[Mu],\[Nu],\[Sigma],\[Rho]]
Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]DuplicateFreeQ[{\[Mu],\[Nu],\[Rho],\[Sigma]}] := 0
Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; \[Not]OrderedQ[{\[Nu],\[Sigma]}] := Riemann[a][\[Rho],\[Sigma],\[Mu],\[Nu]]
Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] /; (OrderedQ[{\[Rho],\[Mu]}]&&OrderedQ[{\[Rho],\[Nu]}]) := - Riemann[a][\[Nu],\[Rho],\[Mu],\[Sigma]] - Riemann[a][\[Rho],\[Mu],\[Nu],\[Sigma]]

Riemann /: Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_] Momentum[a_][\[Alpha]_] /; MemberQ[{\[Mu],\[Nu],\[Rho],\[Sigma]},\[Alpha]] := 0
Riemann /: Riemann[a_][\[Mu]1_,\[Nu]1_,\[Rho]1_,\[Sigma]1_] Riemann[a_][\[Mu]2_,\[Nu]2_,\[Rho]2_,\[Sigma]2_] /; \[Not]DuplicateFreeQ[{\[Mu]1,\[Nu]1,\[Rho]1,\[Sigma]1,\[Mu]2,\[Nu]2,\[Rho]2,\[Sigma]2}] := 0


(* ::Subsection:: *)
(*Momentum*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MomentumBox[a_][\[Mu]_] :=
    TemplateBox[{a,\[Mu]}, "Momentum",
        DisplayFunction -> (SubsuperscriptBox["p",RowBox[{#1}],RowBox[{#2}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1}],"]","[",RowBox[{#2}],"]"}]&)]

MomBox[a_] :=
    TemplateBox[{a}, "Momentum",
        DisplayFunction -> (SubscriptBox["p",RowBox[{#1}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mom" -> MomentumBox["\[SelectionPlaceholder]"]["\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Momentum /: MakeBoxes[Momentum[a_][\[Mu]_], StandardForm | TraditionalForm] := MomentumBox[ToBoxes[a]][ToBoxes[\[Mu]]]
Momentum /: MakeBoxes[Momentum[a_], StandardForm | TraditionalForm] := MomBox[ToBoxes[a]]

(*Momentum /: Momentum[a_][\[Nu]_] Momentum[a_][\[Nu]_] := Mass[a]^2
Momentum /: Momentum[a_][\[Nu]_]^2 := Mass[a]^2*)

Momentum[a_][\[Mu]_] /; MatchQ[Head[a],Times]&&a[[1]]==-1 := - Momentum[-a][\[Mu]]
Momentum[a_][\[Mu]_] /; a < 0 := - Momentum[-a][\[Mu]]


(* ::Subsection:: *)
(*FTrace*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


ClearAll[FTraceBox]


FTraceBox[a_,c__,b_] :=
    TemplateBox[{b,Sequence@@Riffle[{a,c,b},"\[CenterDot]"],Sequence@@Riffle[{c},","]}, "FTrace",
        DisplayFunction -> (RowBox[{TemplateSlotSequence[{2,4+2*Length[{c}]}]}]&),
        InterpretationFunction->(RowBox[{"FTrace","[",RowBox[{#2,",","{",TemplateSlotSequence[5+2*Length[{c}]],"}",",",#1}],"]"}]&)
        ]


(* ::Subsubsection::Closed:: *)
(*Properties*)


FTrace /: MakeBoxes[FTrace[a_,c_List,b_], StandardForm | TraditionalForm] := FTraceBox[ToBoxes[a],Sequence@@(ToBoxes/@c),ToBoxes[b]]

FTrace[a_Plus,c_List,b_]:=FTrace[#,c,b]&/@a
FTrace[a_,c_List,b_Plus]:=FTrace[a,c,#]&/@b
FTrace[a_,{c___,d_Plus,e___},b_]:=FTrace[a,{c,#,e},b]&/@d

FTrace[a_,c_List,b_]/;!OrderedQ[{a,b}]:=(-1)^Length[c]*FTrace[b,Reverse@c,a]


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


Mass /: MakeBoxes[Mass[a_], StandardForm | TraditionalForm] := MassBox[ToBoxes[a]]


(* ::Subsection:: *)
(*Mandelstam invariants*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MandelstamBox[a__] :=
    TemplateBox[{a}, "Mandelstam",
        DisplayFunction -> (SubscriptBox["s",RowBox[{##}]]&),
        InterpretationFunction -> (RowBox[{"Mandelstam","[",RowBox[{##}],"]"}]&)]


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


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "dotmm" -> DotProduct[Momentum["\[SelectionPlaceholder]"],Momentum["\[Placeholder]"]]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "dotem" -> DotProduct[EpsilonPol["\[SelectionPlaceholder]"],Momentum["\[Placeholder]"]]]]
        
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "dotee" -> DotProduct[EpsilonPol["\[SelectionPlaceholder]"],EpsilonPol["\[Placeholder]"]]]]


(* ::Subsubsection:: *)
(*Properties*)


DotProduct /: MakeBoxes[DotProduct[a_,b_], StandardForm | TraditionalForm] := DotProductBox[ToBoxes[a],ToBoxes[b]]


DotProduct[a_,b_] /; \[Not]OrderedQ[{a,b}] := DotProduct[b,a]

DotProduct[a_Plus,b_]:=DotProduct[#,b]&/@a
DotProduct[a_,b_Plus]:=DotProduct[a,#]&/@b

(*DotProduct[Momentum[a_],Momentum[a_]] := Mass[a]^2*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["DdimVariables`*"]


EndPackage[]
