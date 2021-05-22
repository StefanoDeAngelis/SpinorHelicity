(* ::Package:: *)

BeginPackage["HelicityVariables`"]


(* ::Section:: *)
(*Messages*)


SpinorUnDottenML::usage = "..."
SpinorDottenML::usage = "..."

$up::usage = "..."
$down::usage = "..."

SML::usage = "..."
AML::usage = "..."
Sp::usage = "..."


(* ::Section:: *)
(*Spinor Helicity Variables*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Massless spinors*)


(* ::Subsubsection::Closed:: *)
(*Boxes and shortcuts*)


SpinorUnDottenUpMLBox[label_,index_]:=
	TemplateBox[{label,index},"SpinorUnDottenML",
		DisplayFunction->(SubsuperscriptBox["\[Lambda]",RowBox[{#1}],RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUnDottenML[$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

SpinorUnDottenDownMLBox[label_,index_]:=
	TemplateBox[{label,index},"SpinorUnDottenDownML",
		DisplayFunction->(SubscriptBox["\[Lambda]",RowBox[{#1,#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUnDottenML[$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

SpinorDottenUpMLBox[label_,index_]:=
	TemplateBox[{label,index},"SpinorDottenUpML",
		DisplayFunction->(SubsuperscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1}],OverscriptBox[RowBox[{#2}],"."]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottenML[$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

SpinorDottenDownMLBox[label_,index_]:=
	TemplateBox[{label,index},"SpinorDottenDownML",
		DisplayFunction->(SubscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1,OverscriptBox[#2,"."]}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottenML[$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]


SpinorUnDottenML[$up] /: MakeBoxes[SpinorUnDottenML[$up][a_, b_], StandardForm | TraditionalForm] := SpinorUnDottenUpMLBox[ToBoxes[a], ToBoxes[b]]

SpinorUnDottenML[$down] /: MakeBoxes[SpinorUnDottenML[$down][a_, b_], StandardForm | TraditionalForm] := SpinorUnDottenDownMLBox[ToBoxes[a], ToBoxes[b]]

SpinorDottenML[$up] /: MakeBoxes[SpinorDottenML[$up][a_, b_], StandardForm | TraditionalForm] := SpinorDottenUpMLBox[ToBoxes[a], ToBoxes[b]]

SpinorDottenML[$down] /: MakeBoxes[SpinorDottenML[$down][a_, b_], StandardForm | TraditionalForm] := SpinorDottenDownMLBox[ToBoxes[a], ToBoxes[b]]


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "suum" -> SpinorUnDottenUpMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sudm" -> SpinorUnDottenDownMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sdum" -> SpinorDottenUpMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sddm" -> SpinorDottenDownMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SpinorUnDottenML /: SpinorUnDottenML[$up][l1_,a_] SpinorUnDottenML[$down][l2_,a_] := AML[l1,l2];
SpinorDottenML /: SpinorDottenML[$up][l1_,a_] SpinorDottenML[$down][l2_,a_] := -SML[l1,l2];


(* ::Subsection:: *)
(*Momentum*)


(* ::Subsubsection:: *)
(*Boxes and shortcuts*)


(*MomentumUpBox[label_,indexDotted_,indexUnDotted_]:=
	TemplateBox[{label,indexDotted,indexUnDotted},"Momentum",
		DisplayFunction\[Rule](SubsuperscriptBox["p",RowBox[{#1}],RowBox[{#2,#3}]]&),
		InterpretationFunction -> (RowBox[{"Momentum[$up]","[",RowBox[{#1,",",#2",",#3}],"]"}]&)
	]

MomentumDownBox[label_,indexUnDotted_,indexDotted_]:=
	TemplateBox[{label,indexUnDotted,indexDotted},"Momentum",
		DisplayFunction\[Rule](SubscriptBox["p",RowBox[{#1,#2,#3}]]&),
		InterpretationFunction -> (RowBox[{"Momentum[$down]","[",RowBox[{#1,",",#2",",#3}],"]"}]&)
	]

Momentum[$up] /: MakeBoxes[SpinorUnDottenML[$up][label_,a_, b_], StandardForm | TraditionalForm] := SpinorUnDottenUpMLBox[ToBoxes[label],ToBoxes[a], ToBoxes[b]]

Momentum[$down] /: MakeBoxes[SpinorUnDottenML[$down][label_,a_, b_], StandardForm | TraditionalForm] := SpinorUnDottenDownMLBox[ToBoxes[label],ToBoxes[a], ToBoxes[b]]


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mum" -> MomentumUpBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mdm" -> MomentumDownBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]*)


(* ::Subsection:: *)
(*Massless square brackets*)


(* ::Subsubsection::Closed:: *)
(*Boxes and shortcuts*)


SMLBox[a_, b_] :=
    TemplateBox[{a, b}, "SML",
        DisplayFunction -> (RowBox[{"[",RowBox[{#1,"\[MediumSpace]",#2}],"]"}]&),
        InterpretationFunction -> (RowBox[{"SML","[",RowBox[{#1,",",#2}],"]"}]&)]

SML /: MakeBoxes[SML[a_, b_], StandardForm | TraditionalForm] := SMLBox[ToBoxes[a], ToBoxes[b]]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sbm" -> SMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SML[a_, b_] /; (a == b) := 0
SML[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SML[b, a]


(* ::Subsection:: *)
(*Massless angle brackets*)


(* ::Subsubsection::Closed:: *)
(*Boxes and shortcuts*)


AMLBox[a_, b_] :=
    TemplateBox[{a, b}, "AML",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",RowBox[{#1,"\[MediumSpace]",#2}],"\[RightAngleBracket]"}]&),
        InterpretationFunction -> (RowBox[{"AML","[",RowBox[{#1,",",#2}],"]"}]&)]

AML /: MakeBoxes[AML[a_, b_], StandardForm | TraditionalForm] := AMLBox[ToBoxes[a], ToBoxes[b]]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "abm" -> AMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


AML[a_, b_] /; (a == b) := 0
AML[a_, b_] /; \[Not]OrderedQ[{a,b}] := -AML[b, a]


(* ::Subsection:: *)
(*Mandelstam invariants*)


(* ::Subsubsection::Closed:: *)
(*Boxes and shortcuts*)


SpBox[a__] :=
    TemplateBox[{a}, "Sp",
        DisplayFunction -> (SubscriptBox["s",RowBox[{##}]]&),
        InterpretationFunction -> (RowBox[{"Sp","[",RowBox[{##}],"]"}]&)]

Sp /: MakeBoxes[Sp[a__], StandardForm | TraditionalForm] := SpBox[Sequence@@(ToBoxes/@{a})]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mal" -> SpBox["\[SelectionPlaceholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


Sp[a__]/;\[Not]OrderedQ[{a}]:=Sp[Sequence@@Sort[{a}]]
Sp[a_,b_]/;a==b:=0


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["HelicityStructures`*"]


EndPackage[]
