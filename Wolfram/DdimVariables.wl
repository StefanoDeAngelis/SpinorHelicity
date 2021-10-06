(* ::Package:: *)

BeginPackage["DdimVariables`"]


(* ::Section:: *)
(*Messages*)


Metric::usage = "..."
EpsilonPol::usage = "..."
Momentum::usage = "..."

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


Metric /: Metric[\[Mu]_,\[Nu]_] Metric[\[Rho]_,\[Nu]_] := Metric[\[Mu],\[Rho]]
Metric /: Metric[\[Mu]_,\[Nu]_] Metric[\[Nu]_,\[Rho]_] := Metric[\[Mu],\[Rho]]
Metric /: Metric[\[Nu]_,\[Mu]_] Metric[\[Nu]_,\[Rho]_] := Metric[\[Mu],\[Rho]]

Metric /: Metric[\[Mu]_,\[Nu]_] Momentum[a_][\[Nu]_] := Momentum[a][\[Mu]]

Metric /: Metric[\[Mu]_,\[Nu]_] EpsilonPol[a_][\[Rho]___,\[Nu]_,\[Sigma]___] := EpsilonPol[a][\[Rho],\[Mu],\[Sigma]]


(* ::Subsection:: *)
(*Polarisation tensor*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


EpsilonPolBox[a_][\[Mu]__] :=
    TemplateBox[{a,\[Mu]}, "EpsilonPol",
        DisplayFunction -> (SubsuperscriptBox["\[Epsilon]",RowBox[{#1}],RowBox[{##2}]]&),
        InterpretationFunction -> (RowBox[{"EpsilonPol","[",RowBox[{#1}],"]","[",RowBox[{##2}],"]"}]&)]

EpsilonPolBox[a_] :=
    TemplateBox[{a}, "EpsilonPol",
        DisplayFunction -> (SubscriptBox["\[Epsilon]",RowBox[{#1}]]&),
        InterpretationFunction -> (RowBox[{"EpsilonPol","[",RowBox[{#1}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "eps" -> EpsilonPolBox["\[SelectionPlaceholder]"]["\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


EpsilonPol /: MakeBoxes[EpsilonPol[a_][\[Mu]__], StandardForm | TraditionalForm] := EpsilonPolBox[ToBoxes[a]][Sequence@@(ToBoxes/@{\[Mu]})]
EpsilonPol /: MakeBoxes[EpsilonPol[a_], StandardForm | TraditionalForm] := EpsilonPolBox[ToBoxes[a]]


EpsilonPol[a_][\[Mu]__] /; \[Not]OrderedQ[{\[Mu]}] := EpsilonPol[a][Sequence@@Sort[{\[Mu]}]]


(* ::Subsection:: *)
(*Momentum*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MomentumBox[a_][\[Mu]_] :=
    TemplateBox[{a,\[Mu]}, "Momentum",
        DisplayFunction -> (SubsuperscriptBox["p",RowBox[{#1}],RowBox[{#2}]]&),
        InterpretationFunction -> (RowBox[{"Momentum","[",RowBox[{#1}],"]","[",RowBox[{#2}],"]"}]&)]

MomentumBox[a_] :=
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
Momentum /: MakeBoxes[Momentum[a_], StandardForm | TraditionalForm] := MomentumBox[ToBoxes[a]]


(* ::Subsection:: *)
(*Masses*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MassBox[label_]:=
	TemplateBox[{label},"Mass",
		DisplayFunction->(SubscriptBox["m",RowBox[{#1}]]&),
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
    TemplateBox[{a,b}, "Dot",
        DisplayFunction -> (RowBox[{#1,"\[CenterDot]",#2}]&),
        InterpretationFunction -> (RowBox[{"Dot","[",RowBox[{#1,",",#2}],"]"}]&)]


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


DotProduct /: MakeBoxes[DotProduct[a_,b_], StandardForm | TraditionalForm] := DotProductBox[ToBoxes[a],ToBoxes[b]]


DotProduct[a_,b_] /; \[Not]OrderedQ[{a,b}] := DotProduct[b,a]

DotProduct[Momentum[a_],Momentum[a_]] := Mass[a]^2


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["DdimVariables`*"]


EndPackage[]
