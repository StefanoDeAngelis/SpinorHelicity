(* ::Package:: *)

BeginPackage["HelicityVariables`"]


(* ::Section:: *)
(*Messages*)


SML::usage = "..."
AML::usage = "..."
Sp::usage = "..."


(* ::Section:: *)
(*Spinor Helicity Variables*)


Begin["`Private`"]


SMLBox[a_, b_] :=
    TemplateBox[{a, b}, "SML",
        DisplayFunction -> (RowBox[{"[",RowBox[{#1,"\[MediumSpace]",#2}],"]"}]&),
        InterpretationFunction -> (RowBox[{"SML","[",RowBox[{#1,",",#2}],"]"}]&)]

SML /: MakeBoxes[SML[a_, b_], StandardForm | TraditionalForm] := SMLBox[ToBoxes[a], ToBoxes[b]]

SML[a_, b_] /; (a == b) := 0
SML[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SML[b, a]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sb" -> SMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]


AMLBox[a_, b_] :=
    TemplateBox[{a, b}, "AML",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",RowBox[{#1,"\[MediumSpace]",#2}],"\[RightAngleBracket]"}]&),
        InterpretationFunction -> (RowBox[{"AML","[",RowBox[{#1,",",#2}],"]"}]&)]

AML /: MakeBoxes[AML[a_, b_], StandardForm | TraditionalForm] := AMLBox[ToBoxes[a], ToBoxes[b]]

AML[a_, b_] /; (a == b) := 0
AML[a_, b_] /; \[Not]OrderedQ[{a,b}] := -AML[b, a]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ab" -> AMLBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]


SpBox[a__] :=
    TemplateBox[{a}, "Sp",
        DisplayFunction -> (SubscriptBox["s",RowBox[{##}]]&),
        InterpretationFunction -> (RowBox[{"Sp","[",RowBox[{##}],"]"}]&)]

Sp /: MakeBoxes[Sp[a__], StandardForm | TraditionalForm] := SpBox[Sequence@@(ToBoxes/@{a})]

Sp[a__]/;\[Not]OrderedQ[{a}]:=Sp[Sequence@@Sort[{a}]]
Sp[a_,b_]/;a==b:=0

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mal" -> SpBox["\[SelectionPlaceholder]"]]]


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["HelicityStructures`*"]


EndPackage[]
