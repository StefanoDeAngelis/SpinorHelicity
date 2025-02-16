(* ::Package:: *)

BeginPackage["HelicityVariables`", {"DdimVariables`"}]



(* ::Section:: *)
(*Messages*)


SpinorUndottedML::usage = "..."
SpinorDottedML::usage = "..."

SpinorUndottedMV::usage = "..."
SpinorDottedMV::usage = "..."

EpsilonSpin::usage = "..."

EpsilonLorentzDotted::usage = "..."
EpsilonLorentzUndotted::usage = "..."

MassUntilde::usage = "..."
MassTilde::usage = "..."

$up::usage = "..."
$down::usage = "..."

SquareB::usage = "..."
AngleB::usage = "..."

AngleAngleChain::usage = "..."
SquareSquareChain::usage = "..."
AngleSquareChain::usage = "..."
TraceChain::usage = "..."

$massless::usage = "..."
DeclareMassless::usage = "..."
UndeclareMassless::usage = "..."
ClearMassless::usage = "..."


(* ::Section:: *)
(*Spinor Helicity Variables*)


Begin["`Private`"]


(*
TODOs:
	- add a function which expand a possible sum of momenta in the chains or add the property as upvalue
*)


(* ::Subsection:: *)
(*Massless spinors*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


(* ::Text:: *)
(*Undotted spinors with indices:*)


SpinorUndottedMLBox[$up][label_, index_] := 
	TemplateBox[{label, index}, "SpinorUndottedML",
		DisplayFunction -> (SubsuperscriptBox["\[Lambda]", RowBox[{#1}], RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedML[$up]","[", RowBox[{#1, ",", #2}], "]"}]&)
	]

SpinorUndottedMLBox[$down][label_, index_] := 
	TemplateBox[{label, index}, "SpinorUndottedDownML",
		DisplayFunction -> (SubscriptBox["\[Lambda]", RowBox[{#1, #2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedML[$down]","[", RowBox[{#1, ",", #2}], "]"}]&)
	]


(* ::Text:: *)
(*Dotted spinors with indices:*)


SpinorDottedMLBox[$up][label_, index_] := 
	TemplateBox[{label, index}, "SpinorDottedUpML",
		DisplayFunction -> (SubsuperscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1}], OverscriptBox[RowBox[{#2}], "."]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedML[$up]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

SpinorDottedMLBox[$down][label_, index_] := 
	TemplateBox[{label, index}, "SpinorDottedDownML", 
		DisplayFunction -> (SubscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1, OverscriptBox[#2, "."]}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedML[$down]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]


(* ::Text:: *)
(*Spinors inside angle and square brackets:*)


SpinorUndottedMLBox[][label_] := 
	TemplateBox[{label}, "SpinorUndottedML",
		DisplayFunction -> (RowBox[{#1}]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedML[]", "[", RowBox[{#1}], "]"}]&)
	]

SpinorDottedMLBox[][label_] := 
	TemplateBox[{label}, "SpinorDottedML",
		DisplayFunction -> (RowBox[{#1}]&), 
		InterpretationFunction -> (RowBox[{"SpinorDottedML[]", "[", RowBox[{#1}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ 
		Append[
			InputAliases /. Options[EvaluationNotebook[], InputAliases],
			"suum" -> SpinorUndottedMLBox[$up]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
	]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[ InputAliases /. Options[EvaluationNotebook[], InputAliases], "sudm" -> SpinorUndottedMLBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[ InputAliases /. Options[EvaluationNotebook[], InputAliases], "sdum" -> SpinorDottedMLBox[$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[ InputAliases /. Options[EvaluationNotebook[], InputAliases], "sddm" -> SpinorDottedMLBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


(* ::Text:: *)
(*Visualisation properties:*)


SpinorUndottedML[$up] /: MakeBoxes[SpinorUndottedML[$up][a_, b_], StandardForm | TraditionalForm] := SpinorUndottedMLBox[$up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

SpinorUndottedML[$down] /: MakeBoxes[SpinorUndottedML[$down][a_, b_], StandardForm | TraditionalForm] := SpinorUndottedMLBox[$down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

SpinorDottedML[$up] /: MakeBoxes[SpinorDottedML[$up][a_, b_], StandardForm | TraditionalForm] := SpinorDottedMLBox[$up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

SpinorDottedML[$down] /: MakeBoxes[SpinorDottedML[$down][a_, b_], StandardForm | TraditionalForm] := SpinorDottedMLBox[$down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

SpinorUndottedML[] /: MakeBoxes[SpinorUndottedML[][a_], StandardForm | TraditionalForm] := SpinorUndottedMLBox[][DdimVariables`ToLabel[a]]

SpinorDottedML[] /: MakeBoxes[SpinorDottedML[][a_], StandardForm | TraditionalForm] := SpinorDottedMLBox[][DdimVariables`ToLabel[a]]

(*SpinorUndottedML[pos_][l1_Plus,a_] := Plus @@ (SpinorUndottedML[pos][#,a] &/@ (List@@l1))SpinorDottedML[pos_][l1_Plus,a_] := Plus @@ (SpinorDottedML[pos][#,a] &/@ (List@@l1))SpinorML[l_Plus] := Plus @@ (SpinorML /@ List@@l)*)


(* ::Text:: *)
(*Crossing properties of spinors*)


SpinorUndottedML /: SpinorUndottedML[pos___][Times[-1,l___],a___] := I SpinorUndottedML[pos][Times[l],a]
SpinorDottedML /: SpinorDottedML[pos___][Times[-1,l___],a___] := I SpinorDottedML[pos][Times[l],a]

(*SpinorUndottedML /: SpinorUndottedML[][Times[-1,l___]] := I SpinorUndottedML[][Times[l]]
SpinorDottedML /: SpinorDottedML[][Times[-1,l___]] := I SpinorDottedML[][Times[l]]*)


(* ::Text:: *)
(*Automatic contractions into angle and square brackets:*)


SpinorUndottedML /: SpinorUndottedML[$up][l1_, a_] SpinorUndottedML[$down][l2_, a_] := AngleB[SpinorUndottedML[][l1], SpinorUndottedML[][l2]];

SpinorDottedML /: SpinorDottedML[$up][l1_, a_] SpinorDottedML[$down][l2_, a_] := -SquareB[SpinorDottedML[][l1], SpinorDottedML[][l2]];


(* ::Subsection:: *)
(*Massive spinors*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


(* ::Text:: *)
(*Undotted spinors with LG and Lorentz indices:*)


SpinorUndottedMVBox[$up, $up][label_, indexLorentz_, indexSpin_] := 
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorUndottedMV", 
		DisplayFunction -> (SubsuperscriptBox["\[Lambda]", RowBox[{#1}], RowBox[{#2, #3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$up,$up]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]

SpinorUndottedMVBox[$up, $down][label_, indexLorentz_, indexSpin_] := 
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorUndottedMV", 
		DisplayFunction -> (SubsuperscriptBox["\[Lambda]", RowBox[{#1, #3}], RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$up,$down]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]

SpinorUndottedMVBox[$down, $up][label_, indexLorentz_, indexSpin_] := 
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorUndottedMV", 
		DisplayFunction -> (SubsuperscriptBox["\[Lambda]", RowBox[{#1, #2}], RowBox[{#3}]]&), 
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$down,$up]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]

SpinorUndottedMVBox[$down, $down][label_, indexLorentz_, indexSpin_] := 
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorUndottedMV", 
		DisplayFunction -> (SubscriptBox["\[Lambda]", RowBox[{#1, #2, #3}]]&), 
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$down,$down]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]


(* ::Text:: *)
(*Dotted spinors with LG and Lorentz indices:*)


SpinorDottedMVBox[$up, $up][label_, indexLorentz_, indexSpin_] := 
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorDottedMV", 
		DisplayFunction -> (SubsuperscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1}], RowBox[{OverscriptBox[RowBox[{#2}], "."], #3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$up,$up]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]

SpinorDottedMVBox[$up, $down][label_, indexLorentz_, indexSpin_] :=
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorDottedMV", 
		DisplayFunction -> (SubsuperscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1, #3}], OverscriptBox[RowBox[{#2}], "."]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$up,$down]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]

SpinorDottedMVBox[$down, $up][label_, indexLorentz_, indexSpin_] := 
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorDottedMV", 
		DisplayFunction -> (SubsuperscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1, OverscriptBox[RowBox[{#2}], "."]}], RowBox[{#3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$down,$up]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]

SpinorDottedMVBox[$down, $down][label_, indexLorentz_, indexSpin_] := 
	TemplateBox[{label, indexLorentz, indexSpin}, "SpinorDottedMV",
		DisplayFunction -> (SubscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1, OverscriptBox[RowBox[{#2}], "."], " ", #3}]]&), 
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$down,$down]", "[", RowBox[{#1, ",", #2, ",", #3}], "]"}]&)
	]


(* ::Text:: *)
(*Undotted and dotted spinors (Lorentz invariants) with explicit LG indices:*)


SpinorUndottedMVBox[$up][label_, spin_] := 
	TemplateBox[{label, spin}, "SpinorUndottedMV", 
		DisplayFunction -> (SuperscriptBox[RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$up]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]
	 
SpinorUndottedMVBox[$down][label_, spin_] :=
	TemplateBox[{label, spin}, "SpinorUndottedMV",
		DisplayFunction -> (SubscriptBox[RowBox[{#1}], RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$down]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

SpinorDottedMVBox[$up][label_, spin_] := 
	TemplateBox[{label, spin}, "SpinorDottedMV",
		DisplayFunction -> (SuperscriptBox[RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$up]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

SpinorDottedMVBox[$down][label_, spin_] := 
	TemplateBox[{label, spin}, "SpinorDottedMV", 
		DisplayFunction -> (SubscriptBox[RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$down]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]


(* ::Text:: *)
(*Undotted and dotted spinors (Lorentz invariants) with implicit  LG indices:*)


SpinorUndottedMVBox[][label_] := 
	TemplateBox[{label}, "SpinorUndottedMV",
		DisplayFunction -> (StyleBox[#, Bold]&), 
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[]", "[", RowBox[{#}], "]"}]&)
	]

SpinorDottedMVBox[][label_] := 
	TemplateBox[{label}, "SpinorDottedMV",
		DisplayFunction -> (StyleBox[#, Bold]&), 
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[]", "[", RowBox[{#}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Suuu" -> SpinorUndottedMVBox[$up, $up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Suud" -> SpinorUndottedMVBox[$up, $down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sudu" -> SpinorUndottedMVBox[$down, $up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sudd" -> SpinorUndottedMVBox[$down, $down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sduu" -> SpinorDottedMVBox[$up, $up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sdud" -> SpinorDottedMVBox[$up, $down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sddu" -> SpinorDottedMVBox[$down, $up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]

SetOptions[EvaluationNotebook[], InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sddd" -> SpinorDottedMVBox[$down, $down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SpinorUndottedMV[$up, $up] /: MakeBoxes[SpinorUndottedMV[$up, $up][a_, b_, c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$up, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorUndottedMV[$up, $down] /: MakeBoxes[SpinorUndottedMV[$up, $down][a_, b_, c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$up, $down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorUndottedMV[$down, $up] /: MakeBoxes[SpinorUndottedMV[$down, $up][a_, b_, c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$down, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorUndottedMV[$down, $down] /: MakeBoxes[SpinorUndottedMV[$down, $down][a_, b_, c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$down, $down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorUndottedMV[$up] /: MakeBoxes[SpinorUndottedMV[$up][a_, J_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[J]]

SpinorUndottedMV[$down] /: MakeBoxes[SpinorUndottedMV[$down][a_, J_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[J]]

SpinorUndottedMV[] /: MakeBoxes[SpinorUndottedMV[][a_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[][DdimVariables`ToLabel[a]]

SpinorDottedMV[$up, $up] /: MakeBoxes[SpinorDottedMV[$up, $up][a_, b_, c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$up, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorDottedMV[$up, $down] /: MakeBoxes[SpinorDottedMV[$up, $down][a_, b_, c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$up, $down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorDottedMV[$down, $up] /: MakeBoxes[SpinorDottedMV[$down, $up][a_, b_, c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$down, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorDottedMV[$down, $down] /: MakeBoxes[SpinorDottedMV[$down, $down][a_, b_, c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$down, $down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b], DdimVariables`ToLabel[c]]

SpinorDottedMV[$up] /: MakeBoxes[SpinorDottedMV[$up][a_, J_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[J]]

SpinorDottedMV[$down] /: MakeBoxes[SpinorDottedMV[$down][a_, J_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[J]]

SpinorDottedMV[] /: MakeBoxes[SpinorDottedMV[][a_], StandardForm | TraditionalForm] := SpinorDottedMVBox[][DdimVariables`ToLabel[a]]

SpinorDottedMV /: SpinorDottedMV[pos1_, $down][i_, a_, II_] SpinorDottedMV[pos2_, $up][i_, b_, II_]/;!NumericQ[II] := MassTilde[i] EpsilonLorentzDotted[pos1, pos2][a, b]

SpinorUndottedMV /: SpinorUndottedMV[pos1_, $up][i_, a_, II_] SpinorUndottedMV[pos2_, $down][i_, b_, II_]/;!NumericQ[II] := MassUntilde[i] EpsilonLorentzUndotted[pos1, pos2][a, b]

SpinorUndottedMV /: SpinorUndottedMV[$up, pos_][l1_, a_, J_] SpinorUndottedML[$down][l2_, a_] := -AngleB[SpinorUndottedML[][l2], SpinorUndottedMV[pos][l1, J]];

SpinorUndottedMV /: SpinorUndottedMV[$down, pos_][l1_, a_, J_] SpinorUndottedML[$up][l2_, a_] := AngleB[SpinorUndottedML[][l2], SpinorUndottedMV[pos][l1, J]];

SpinorUndottedMV /: SpinorUndottedMV[$down, pos1_][l1_, a_, J_] SpinorUndottedMV[$up, pos2_][l2_, a_, KK_] := AngleB[SpinorUndottedMV[pos1][l1, J], SpinorUndottedMV[pos2][l2, KK]];

SpinorDottedMV /: SpinorDottedMV[$up, pos_][l1_, a_, J_] SpinorDottedML[$down][l2_, a_] := SquareB[SpinorDottedML[][l2], SpinorDottedMV[pos][l1, J]];

SpinorDottedMV /: SpinorDottedMV[$down, pos_][l1_, a_, J_] SpinorDottedML[$up][l2_, a_] := -SquareB[SpinorDottedML[][l2], SpinorDottedMV[pos][l1, J]]; 

SpinorDottedMV /: SpinorDottedMV[$down, pos1_][l1_, a_, J_] SpinorDottedMV[$up, pos2_][l2_, a_, KK_] := -SquareB[SpinorDottedMV[pos1][l1, J], SpinorDottedMV[pos2][l2, KK]];

(*SpinorUndottedMV[pos1_,pos2_][l1_Plus,a_,J_] := Plus @@ (SpinorUndottedML[pos1,pos2][#,a,J] &/@ (List@@l1))SpinorDottedMV[pos1_,pos2_][l1_Plus,a_,J_] := Plus @@ (SpinorDottedML[pos1,pos2][#,a,J] &/@ (List@@l1))SpinorMV[pos_][l_Plus,J_] := Plus @@ (SpinorML[pos][#,J] &/@ List@@l)*)


(* ::Text:: *)
(*Crossing properties of spinors:*)


SpinorDottedMV /: SpinorDottedMV[pos1___,pos2_][Times[-1,l___],a___] := SpinorDottedMV[pos1,pos2/.{$down->$up,$up->$down}][Times[l],a]
SpinorUndottedMV /: SpinorUndottedMV[pos1___,pos2_][Times[-1,l___],a___] := SpinorUndottedMV[pos1,pos2/.{$down->$up,$up->$down}][Times[l],a]

(*SpinorUndottedML /: SpinorUndottedML[][Times[-1,l___]] := I SpinorUndottedML[][Times[l]]
SpinorDottedML /: SpinorDottedML[][Times[-1,l___]] := I SpinorDottedML[][Times[l]]*)


(* ::Text:: *)
(*If a spinor is declared to be massless:*)


SpinorDottedMV[pos1_,pos2_][lab_,ind1_,ind2_]/;MemberQ[$massless,lab]:=SpinorDottedML[pos1][lab,ind1]
SpinorDottedMV[pos1_][lab_,ind1_]/;MemberQ[$massless,lab]:=SpinorDottedML[][lab]
SpinorDottedMV[][lab_]/;MemberQ[$massless,lab]:=SpinorDottedML[][lab]

SpinorUndottedMV[pos1_,pos2_][lab_,ind1_,ind2_]/;MemberQ[$massless,lab]:=SpinorUndottedML[pos1][lab,ind1]
SpinorUndottedMV[pos1_][lab_,ind1_]/;MemberQ[$massless,lab]:=SpinorUndottedML[][lab]
SpinorUndottedMV[][lab_]/;MemberQ[$massless,lab]:=SpinorUndottedML[][lab]


(* ::Subsection:: *)
(*Epsilon Tensors - Lorentz*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


EpsilonLorentzUndottedBox[$up, $up][II_, J_] :=
	TemplateBox[{II, J}, "EpsilonLorentzUndotted",
		DisplayFunction -> (SuperscriptBox["\[Epsilon]", RowBox[{#1, #2}]]&), 
		InterpretationFunction -> (RowBox[{"EpsilonLorentzUndotted[$up,$up]","[", RowBox[{#1, ",", #2}], "]"}]&)
	]

EpsilonLorentzUndottedBox[$down, $up][II_, J_] := 
	TemplateBox[{II, J}, "EpsilonLorentzUndotted", 
		DisplayFunction -> (SubsuperscriptBox["\[Delta]", RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"EpsilonLorentzUndotted[$down,$up]","[", RowBox[{#1, ",", #2}], "]"}]&)
	]

EpsilonLorentzUndottedBox[$down, $down][II_, J_] := 
	TemplateBox[{II, J}, "EpsilonLorentzUndotted", 
		DisplayFunction -> (SubscriptBox["\[Epsilon]", RowBox[{#1, #2}]]&), 
		InterpretationFunction -> (RowBox[{"EpsilonLorentzUndotted[$down,$down]","[", RowBox[{#1, ",", #2}], "]"}]&)
	]

EpsilonLorentzDottedBox[$up, $up][II_, J_] := 
	TemplateBox[{II, J}, "EpsilonLorentzDotted",
		DisplayFunction -> (SuperscriptBox["\[Epsilon]", RowBox[{OverscriptBox[#1, "."], OverscriptBox[#2, "."]}]]&), 
		InterpretationFunction -> (RowBox[{"EpsilonLorentzDotted[$up,$up]","[", RowBox[{#1, ",", #2}], "]"}]&)
	]

EpsilonLorentzDottedBox[$down, $up][II_, J_] := 
	TemplateBox[{II, J}, "EpsilonLorentzDotted", 
		DisplayFunction -> (SubsuperscriptBox["\[Delta]", OverscriptBox[#1, "."], OverscriptBox[#2, "."]]&),
		InterpretationFunction -> (RowBox[{"EpsilonLorentzDotted[$down,$up]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

EpsilonLorentzDottedBox[$down, $down][II_, J_] := 
	TemplateBox[{II, J},"EpsilonLorentzDotted", 
		DisplayFunction -> (SubscriptBox["\[Epsilon]", RowBox[{OverscriptBox[#1, "."], OverscriptBox[#2, "."]}]]&), 
		InterpretationFunction -> (RowBox[{"EpsilonLorentzDotted[$down,$down]", "[", RowBox[{#1, ",",#2}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases],
		"ELuuu" -> EpsilonLorentzUndottedBox[$up, $up]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]

SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases],
		"ELudu" -> EpsilonLorentzUndottedBox[$down, $up]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"ELudd" -> EpsilonLorentzUndottedBox[$down, $down]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"ELduu" -> EpsilonLorentzDottedBox[$up, $up]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases],
		"ELddu" -> EpsilonLorentzDottedBox[$down, $up]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"ELddd" -> EpsilonLorentzDottedBox[$down, $down]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]


(* ::Subsubsection::Closed:: *)
(*Properties*)


(* ::Text:: *)
(*Graphical properties:*)


EpsilonLorentzUndotted[$up, $up] /: MakeBoxes[EpsilonLorentzUndotted[$up, $up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzUndottedBox[$up, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonLorentzUndotted[$down, $up] /: MakeBoxes[EpsilonLorentzUndotted[$down, $up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzUndottedBox[$down, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonLorentzUndotted[$down, $down] /: MakeBoxes[EpsilonLorentzUndotted[$down, $down][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzUndottedBox[$down, $down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonLorentzDotted[$up, $up] /: MakeBoxes[EpsilonLorentzDotted[$up, $up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzDottedBox[$up, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonLorentzDotted[$down, $up] /: MakeBoxes[EpsilonLorentzDotted[$down, $up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzDottedBox[$down, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonLorentzDotted[$down, $down] /: MakeBoxes[EpsilonLorentzDotted[$down, $down][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzDottedBox[$down, $down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]


(* ::Text:: *)
(*Undotted Epsilon contractions:*)


EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$up, $down][a_, b_] := EpsilonLorentzUndotted[$down, $up][b, a]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $up][a_, b_] EpsilonLorentzUndotted[$down, pos4_][b_, c_]] := EpsilonLorentzUndotted[pos1, pos4][a, c]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $down][a_, b_] EpsilonLorentzUndotted[$up, pos4_][b_, c_]] := EpsilonLorentzUndotted[pos1, pos4][a, c]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$up, pos1_][b_, a_] EpsilonLorentzUndotted[$down, pos4_][b_, c_]] := If[MatchQ[pos1, $up], -1, 1] EpsilonLorentzUndotted[pos1, pos4][a, c]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$down, pos1_][b_, a_] EpsilonLorentzUndotted[$up, pos4_][b_, c_]] := If[MatchQ[pos1, $down], -1, 1] EpsilonLorentzUndotted[pos1, pos4][a, c]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $up][a_, b_] EpsilonLorentzUndotted[pos4_, $down][c_, b_]] := If[MatchQ[pos1, $up], -1, 1] EpsilonLorentzUndotted[pos1, pos4][a, c]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $down][a_, b_] EpsilonLorentzUndotted[pos4_, $up][c_, b_]] := If[MatchQ[pos1, $down], -1, 1] EpsilonLorentzUndotted[pos1, pos4][a, c]

EpsilonLorentzUndotted[pos_, pos_][a_, b_] /; \[Not]OrderedQ[{a, b}] := -EpsilonLorentzUndotted[pos, pos][b, a]

EpsilonLorentzUndotted[$down, $up][a_, a_] := 2


(* ::Text:: *)
(*Dotted Epsilon contractions:*)


EpsilonLorentzDotted /: EpsilonLorentzDotted[$up, $down][a_, b_] := EpsilonLorentzDotted[$down, $up][b, a]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $up][a_, b_] EpsilonLorentzDotted[$down, pos4_][b_, c_]] := EpsilonLorentzDotted[pos1, pos4][a, c]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $down][a_, b_] EpsilonLorentzDotted[$up, pos4_][b_, c_]] := EpsilonLorentzDotted[pos1, pos4][a, c]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$up, pos1_][b_, a_] EpsilonLorentzDotted[$down, pos4_][b_, c_]] := If[MatchQ[pos1, $up], -1, 1] EpsilonLorentzDotted[pos1, pos4][a, c]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$down, pos1_][b_, a_] EpsilonLorentzDotted[$up, pos4_][b_, c_]] := If[MatchQ[pos1, $down], -1, 1] EpsilonLorentzDotted[pos1, pos4][a, c]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $up][a_, b_] EpsilonLorentzDotted[pos4_, $down][c_, b_]] := If[MatchQ[pos1, $up], -1, 1] EpsilonLorentzDotted[pos1, pos4][a, c]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $down][a_, b_] EpsilonLorentzDotted[pos4_, $up][c_, b_]] := If[MatchQ[pos1, $down], -1, 1] EpsilonLorentzDotted[pos1, pos4][a, c]

EpsilonLorentzDotted[pos_, pos_][a_, b_] /; \[Not]OrderedQ[{a, b}] := -EpsilonLorentzDotted[pos, pos][b, a]

EpsilonLorentzDotted[$down, $up][a_, a_] := 2


(* ::Text:: *)
(*Raise/lower Dotted indices:*)


EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $up][a_, b_] SpinorDottedMV[$down, pos3_][mom_, b_, J_]] := SpinorDottedMV[pos1, pos3][mom, a, J]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $down][a_, b_] SpinorDottedMV[$up, pos3_][mom_, b_, J_]] := SpinorDottedMV[pos1, pos3][mom, a, J]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$up, pos1_][a_, b_] SpinorDottedMV[$down, pos3_][mom_, a_, II_]] := If[MatchQ[pos1, $up], -1, 1] SpinorDottedMV[pos1, pos3][mom, b, II]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$down, pos1_][a_, b_] SpinorDottedMV[$up, pos3_][mom_, a_, II_]] := If[MatchQ[pos1, $down], -1, 1] SpinorDottedMV[pos1, pos3][mom, b, II]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $up][a_, b_] SpinorUndottedMV[$down, pos3_][mom_, b_, J_]] := SpinorUndottedMV[pos1, pos3][mom, a, J]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $down][a_, b_] SpinorUndottedMV[$up, pos3_][mom_, b_, J_]] := SpinorUndottedMV[pos1, pos3][mom, a, J]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$up, pos1_][a_, b_] SpinorUndottedMV[$down, pos3_][mom_, a_, II_]] := If[MatchQ[pos1, $up], -1, 1] SpinorUndottedMV[pos1, pos3][mom, b, II]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$down, pos1_][a_, b_] SpinorUndottedMV[$up, pos3_][mom_, a_, II_]] := If[MatchQ[pos1, $down], -1, 1] SpinorUndottedMV[pos1, pos3][mom, b, II]


(* ::Text:: *)
(*Raise/lower Undotted indices:*)


EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $up][a_, b_] SpinorDottedML[$down][mom_, b_]] := SpinorDottedML[pos1][mom, a]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_, $down][a_, b_] SpinorDottedML[$up][mom_, b_]] := SpinorDottedML[pos1][mom, a]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$up, pos1_][a_, b_] SpinorDottedML[$down][mom_, a_]] := If[MatchQ[pos1, $up], -1, 1] SpinorDottedML[pos1][mom, b]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$down, pos1_][a_, b_] SpinorDottedML[$up][mom_, a_]] := If[MatchQ[pos1, $down], -1, 1] SpinorDottedML[pos1][mom, b]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $up][a_, b_] SpinorUndottedML[$down][mom_, b_]] := SpinorUndottedML[pos1][mom, a]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_, $down][a_, b_] SpinorUndottedML[$up][mom_, b_]] := SpinorUndottedML[pos1][mom, a]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$up, pos1_][a_, b_] SpinorUndottedML[$down][mom_, a_]] := If[MatchQ[pos1, $up], -1, 1] SpinorUndottedML[pos1][mom, b]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$down, pos1_][a_, b_] SpinorUndottedML[$up][mom_, a_]] := If[MatchQ[pos1, $down], -1, 1] SpinorUndottedML[pos1][mom, b]


(* ::Subsection:: *)
(*Epsilon Tensors - Spin*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


EpsilonSpinBox[$up, $up][II_, J_] := 
	TemplateBox[
		{II, J}, "EpsilonSpin",
		DisplayFunction -> (SuperscriptBox["\[Epsilon]", RowBox[{#1, #2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonSpin[$up,$up]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

EpsilonSpinBox[$down, $up][II_, J_] :=
	TemplateBox[
		{II, J}, "EpsilonSpin",
		DisplayFunction -> (SubsuperscriptBox["\[Delta]", RowBox[{#1}], RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonSpin[$down,$up]", "[",RowBox[{#1, ",", #2}], "]"}]&)
	]

EpsilonSpinBox[$down, $down][II_, J_] := 
	TemplateBox[
		{II, J}, "EpsilonSpin",
		DisplayFunction -> (SubscriptBox["\[Epsilon]", RowBox[{#1, #2}]]&),
		InterpretationFunction-> (RowBox[{"EpsilonSpin[$down,$down]", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"ESuu" -> EpsilonSpinBox[$up, $up]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"ESdu" -> EpsilonSpinBox[$down, $up]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"ESdd" -> EpsilonSpinBox[$down, $down]["\[SelectionPlaceholder]", "\[Placeholder]"]
		]
]


(* ::Subsubsection::Closed:: *)
(*Properties*)


EpsilonSpin[$up, $up] /: MakeBoxes[EpsilonSpin[$up, $up][a_, b_], StandardForm| TraditionalForm] := EpsilonSpinBox[$up, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonSpin[$down, $up] /: MakeBoxes[EpsilonSpin[$down, $up][a_, b_],StandardForm | TraditionalForm] := EpsilonSpinBox[$down, $up][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonSpin[$down, $down] /: MakeBoxes[EpsilonSpin[$down, $down][a_,b_], StandardForm | TraditionalForm] := EpsilonSpinBox[$down, $down][DdimVariables`ToLabel[a], DdimVariables`ToLabel[b]]

EpsilonSpin /: EpsilonSpin[$up, $down][a_, b_] := EpsilonSpin[$down, $up][b, a]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $up][a_, b_] EpsilonSpin[$down, pos4_][b_, c_]] := EpsilonSpin[pos1, pos4][a, c]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $down][a_, b_] EpsilonSpin[$up, pos4_][b_, c_]] := EpsilonSpin[pos1, pos4][a, c]

EpsilonSpin /: HoldPattern[EpsilonSpin[$up, pos1_][b_, a_] EpsilonSpin[$down, pos4_][b_, c_]] := If[MatchQ[pos1, $up], -1, 1] EpsilonSpin[pos1, pos4][a, c]

EpsilonSpin /: HoldPattern[EpsilonSpin[$down, pos1_][b_, a_] EpsilonSpin[$up, pos4_][b_, c_]] := If[MatchQ[pos1, $down], -1, 1] EpsilonSpin[pos1, pos4][a, c]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $up][a_, b_] EpsilonSpin[pos4_, $down][c_, b_]] := If[MatchQ[pos1, $up], -1, 1] EpsilonSpin[pos1, pos4][a, c]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $down][a_, b_] EpsilonSpin[pos4_, $up][c_, b_]] := If[MatchQ[pos1, $down], -1, 1] EpsilonSpin[pos1, pos4][a, c]

EpsilonSpin[pos_, pos_][a_, b_] /; \[Not]OrderedQ[{a, b}] := -EpsilonSpin[pos, pos][b, a]

EpsilonSpin[$down, $up][a_, a_] := 2

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $up][II_, J_] SpinorDottedMV[pos3_, $down][mom_, a_, J_]] := SpinorDottedMV[pos3, pos1][mom, a, II]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $down][II_, J_] SpinorDottedMV[pos3_, $up][mom_, a_, J_]] := SpinorDottedMV[pos3, pos1][mom, a, II]
EpsilonSpin /: HoldPattern[EpsilonSpin[$up, pos1_][II_, J_] SpinorDottedMV[pos3_, $down][mom_, a_, II_]] := If[MatchQ[pos1, $up], -1, 1] SpinorDottedMV[pos3, pos1][mom, a, J]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down, pos1_][II_, J_] SpinorDottedMV[pos3_, $up][mom_, a_, II_]] := If[MatchQ[pos1, $down], -1, 1] SpinorDottedMV[pos3, pos1][mom, a, J]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $up][II_, J_] SpinorUndottedMV[pos3_, $down][mom_, a_, J_]] := SpinorUndottedMV[pos3, pos1][mom, a, II]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $down][II_, J_] SpinorUndottedMV[pos3_, $up][mom_, a_, J_]] := SpinorUndottedMV[pos3, pos1][mom, a, II]
EpsilonSpin /: HoldPattern[EpsilonSpin[$up, pos1_][II_, J_] SpinorUndottedMV[pos3_, $down][mom_, a_, II_]] := If[MatchQ[pos1, $up], -1, 1] SpinorUndottedMV[pos3, pos1][mom, a, J]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down, pos1_][II_, J_] SpinorUndottedMV[pos3_, $up][mom_, a_, II_]] := If[MatchQ[pos1, $down], -1, 1] SpinorUndottedMV[pos3, pos1][mom, a, J]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $up][II_, J_] f_[x___, SpinorUndottedMV[$down][mom_, J_], y___]] := f[x, SpinorUndottedMV[pos1][mom, II], y]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $down][II_, J_] f_[x___, SpinorUndottedMV[$up][mom_, J_], y___]] := f[x, SpinorUndottedMV[pos1][mom, II], y]
EpsilonSpin /: HoldPattern[EpsilonSpin[$up, pos1_][II_, J_] f_[x___, SpinorUndottedMV[$down][mom_, II_], y___]] := If[MatchQ[pos1, $up], -1, 1] f[x, SpinorUndottedMV[pos1][mom, J], y]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down, pos1_][II_, J_] f_[x___, SpinorUndottedMV[$up][mom_, II_], y___]] := If[MatchQ[pos1, $down], -1, 1] f[x, SpinorUndottedMV[pos1][mom, J], y]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $up][II_, J_] f_[x___, SpinorDottedMV[$down][mom_, J_], y___]] := f[x, SpinorDottedMV[pos1][mom, II], y]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_, $down][II_, J_] f_[x___, SpinorDottedMV[$up][mom_, J_], y___]] := f[x, SpinorDottedMV[pos1][mom, II], y]
EpsilonSpin /: HoldPattern[EpsilonSpin[$up, pos1_][II_, J_] f_[x___, SpinorDottedMV[$down][mom_, II_], y___]] := If[MatchQ[pos1, $up], -1, 1] f[x, SpinorDottedMV[pos1][mom, J], y]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down, pos1_][II_, J_] f_[x___, SpinorDottedMV[$up][mom_, II_], y___]] := If[MatchQ[pos1, $down], -1, 1] f[x, SpinorDottedMV[pos1][mom, J], y]

EpsilonSpin /: EpsilonSpin[pos1_,po2_][II_,JJ_] SquareB[x___,SpinorDottedMV[pos3_][mom_,KK_],y___] /; MemberQ[{II,JJ},KK] := SquareB[x,(EpsilonSpin[pos1,po2][II,JJ] SpinorDottedMV[pos3][mom,KK]),y]
EpsilonSpin /: EpsilonSpin[pos1_,po2_][II_,JJ_] AngleB[x___,SpinorUndottedMV[pos3_][mom_,KK_],y___] /; MemberQ[{II,JJ},KK] := AngleB[x,(EpsilonSpin[pos1,po2][II,JJ] SpinorUndottedMV[pos3][mom,KK]),y]


(* ::Subsection:: *)
(*Angle brackets*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


AngleBracketBox[a_, b_] := 
	TemplateBox[
		{a, b}, "AngleB", 
		DisplayFunction -> (RowBox[{"\[LeftAngleBracket]", RowBox[{#1, "\[MediumSpace]", #2}], "\[RightAngleBracket]"}]&), 
		InterpretationFunction -> (RowBox[{"AngleB", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[
	EvaluationNotebook[],
		InputAliases -> DeleteDuplicates @ Append[
			InputAliases /. Options[EvaluationNotebook[], InputAliases], 
			"abmm" -> AngleBracketBox[SpinorUndottedMLBox[]["\[SelectionPlaceholder]"], SpinorUndottedMLBox[]["\[Placeholder]"]]
			]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
			InputAliases /. Options[EvaluationNotebook[], InputAliases], 
			"abmMu" -> AngleBracketBox[SpinorUndottedMLBox[]["\[SelectionPlaceholder]"], SpinorUndottedMVBox[$up]["\[Placeholder]", "\[Placeholder]"]]
			]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"abmMd" -> AngleBracketBox[SpinorUndottedMLBox[]["\[SelectionPlaceholder]"], SpinorUndottedMVBox[$down]["\[Placeholder]", "\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"abMuMu" -> AngleBracketBox[SpinorUndottedMVBox[$up]["\[SelectionPlaceholder]", "\[Placeholder]"], SpinorUndottedMVBox[$up]["\[Placeholder]", "\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"abMdMu" -> AngleBracketBox[SpinorUndottedMVBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"], SpinorUndottedMVBox[$up]["\[Placeholder]", "\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"abMdMd" -> AngleBracketBox[SpinorUndottedMVBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"], SpinorUndottedMVBox[$down]["\[Placeholder]", "\[Placeholder]"]]
		]
]


(* ::Subsubsection::Closed:: *)
(*Properties*)


AngleB /: MakeBoxes[AngleB[a_, b_], StandardForm | TraditionalForm] := AngleBracketBox[ToBoxes[a], ToBoxes[b]]

AngleB[a_, b_Plus] := Plus @@ (AngleB[a, #]& /@ (List @@ b))

AngleB[a_Plus, b_] := Plus @@ (AngleB[#, b]& /@ (List @@ a))

AngleB[Times[a_, b__], c_] /; \[Not]MatchQ[a, SpinorUndottedML[][_] | SpinorUndottedMV[_][_, _] | SpinorUndottedMV[][_]] := a * AngleB[Times[b], c]

AngleB[c_, Times[a_, b__]] /; \[Not]MatchQ[a, SpinorUndottedML[][_] | SpinorUndottedMV[_][_, _] | SpinorUndottedMV[][_]] := a * AngleB[c, Times[b]]

AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[$up][i_, II_]] AngleB[b_, SpinorUndottedMV[$down][i_, II_]]] := MassUntilde[i] AngleB[a, b]

AngleB /: HoldPattern[AngleB[SpinorUndottedMV[$up][i_, II_], a_] AngleB[b_, SpinorUndottedMV[$down][i_, II_]]] := -MassUntilde[i] AngleB[a, b]

AngleB /: HoldPattern[AngleB[a_, SpinorUndottedMV[$up][i_, II_]] AngleB[SpinorUndottedMV[$down][i_, II_], b_]] := -MassUntilde[i] AngleB[a, b]

AngleB /: HoldPattern[AngleB[SpinorUndottedMV[$up][i_, II_], a_] AngleB[SpinorUndottedMV[$down][i_, II_], b_]] := MassUntilde[i] AngleB[a, b]

AngleB[a_, b_] /; (a == b) := 0

AngleB[a_, b_] /; \[Not]OrderedQ[{a, b}] := -AngleB[b, a]

AngleB[SpinorUndottedMV[pos1_][a_, II_], SpinorUndottedMV[pos2_][a_, J_]] := EpsilonSpin[pos1, pos2][II, J] * MassUntilde[a]

AngleB[a_Plus, b_] := Plus @@ (AngleB[#, b]& /@ List @@ a)

AngleB[a_, b_Plus] := Plus @@ (AngleB[a, #]& /@ List @@ b)


(* ::Subsection:: *)
(*Square brackets*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


SquareBracketBox[a_, b_] := 
	TemplateBox[
		{a, b}, "SquareB", 
		DisplayFunction -> (RowBox[{"[", RowBox[{#1, "\[MediumSpace]", #2}], "]"}]&), 
		InterpretationFunction -> (RowBox[{"SquareB", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"sbmm" -> SquareBracketBox[SpinorDottedMLBox[]["\[SelectionPlaceholder]"], SpinorDottedMLBox[]["\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"sbmMu" -> SquareBracketBox[SpinorDottedMLBox[]["\[SelectionPlaceholder]"], SpinorDottedMVBox[$up]["\[Placeholder]", "\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"sbmMd" -> SquareBracketBox[SpinorDottedMLBox[]["\[SelectionPlaceholder]"], SpinorDottedMVBox[$down]["\[Placeholder]", "\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"sbMuMu" -> SquareBracketBox[SpinorDottedMVBox[$up]["\[SelectionPlaceholder]", "\[Placeholder]"], SpinorDottedMVBox[$up]["\[Placeholder]", "\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[],
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"sbMdMu" -> SquareBracketBox[SpinorDottedMVBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"], SpinorDottedMVBox[$up]["\[Placeholder]", "\[Placeholder]"]]
		]
]

SetOptions[
	EvaluationNotebook[], 
	InputAliases -> DeleteDuplicates @ Append[
		InputAliases /. Options[EvaluationNotebook[], InputAliases], 
		"sbMdMd" -> SquareBracketBox[SpinorDottedMVBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"], SpinorDottedMVBox[$down]["\[Placeholder]", "\[Placeholder]"]]
		]
]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SquareB /: MakeBoxes[SquareB[a_, b_], StandardForm | TraditionalForm] := SquareBracketBox[ToBoxes[a], ToBoxes[b]]

SquareB[a_, b_Plus] := Plus @@ (SquareB[a, #]& /@ (List @@ b))

SquareB[a_Plus, b_] := Plus @@ (SquareB[#, b]& /@ (List @@ a))

SquareB[Times[a_, b__], c_] /; \[Not]MatchQ[a, SpinorDottedML[][_] | SpinorDottedMV[_][_, _] | SpinorDottedMV[][_]] := a * SquareB[Times[b], c]

SquareB[c_, Times[a_, b__]] /; \[Not]MatchQ[a, SpinorDottedML[][_] | SpinorDottedMV[_][_, _] | SpinorDottedMV[][_]] := a * SquareB[c, Times[b]]

SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[$up][i_, II_]] SquareB[b_, SpinorDottedMV[$down][i_, II_]]] := MassTilde[i] SquareB[a, b]

SquareB /: HoldPattern[SquareB[SpinorDottedMV[$up][i_, II_], a_] SquareB[b_, SpinorDottedMV[$down][i_, II_]]] := -MassTilde[i] SquareB[a, b]

SquareB /: HoldPattern[SquareB[a_, SpinorDottedMV[$up][i_, II_]] SquareB[SpinorDottedMV[$down][i_, II_], b_]] := -MassTilde[i] SquareB[a, b]

SquareB /: HoldPattern[SquareB[SpinorDottedMV[$up][i_, II_], a_] SquareB[SpinorDottedMV[$down][i_, II_], b_]] := MassTilde[i] SquareB[a, b]

SquareB[a_, b_] /; (a == b) := 0

SquareB[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SquareB[b, a]

SquareB[SpinorDottedMV[pos1_][a_, II_], SpinorDottedMV[pos2_][a_, J_]] := EpsilonSpin[pos1, pos2][II, J] * MassTilde[a]

SquareB[a_Plus, b_] := Plus @@ (SquareB[#, b]& /@ List @@ a)

SquareB[a_, b_Plus] := Plus @@ (SquareB[a, #]& /@ List @@ b)


(* ::Subsection:: *)
(*Angle-angle Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


AngleAngleChainBox[a_, b_, c__] := 
	TemplateBox[
		{a, b, c}, "AngleAngleChain",
		DisplayFunction -> (RowBox[{"\[LeftAngleBracket]", #1, "|", ##3, "|", #2, "\[RightAngleBracket]"}]&), 
		InterpretationFunction -> (RowBox[{"AngleAngleChain", "[", RowBox[{#1, ",", "{", TemplateSlotSequence[3, ","], "}", ",", #2}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


AngleAngleChain /: MakeBoxes[AngleAngleChain[a_, c_List, b_], StandardForm | TraditionalForm] := AngleAngleChainBox[ToBoxes[a], ToBoxes[b], Sequence @@ (ToBoxes /@ c)]

AngleAngleChain[a_, c_List, b_Plus] := Plus @@ (AngleAngleChain[a, c, #]& /@ (List @@ b))

AngleAngleChain[a_Plus, c_List, b_] := Plus @@ (AngleAngleChain[#, c, b]& /@ (List @@ a))

AngleAngleChain[Times[a_, b__], c_List, d_] /; \[Not]MatchQ[a, SpinorUndottedML[][_] | SpinorUndottedMV[_][_, _] | SpinorUndottedMV[][_]] := a * AngleAngleChain[Times[b], c, d]

AngleAngleChain[d_, c_, Times[a_, b__]] /; \[Not]MatchQ[a, SpinorUndottedML[][_] | SpinorUndottedMV[_][_, _] | SpinorUndottedMV[][_]] := a * AngleAngleChain[d, c, Times[b]]

AngleAngleChain[a_, b_List, c_] /; \[Not]OrderedQ[{a, c}] := -AngleAngleChain[c, Reverse @ b, a]
	
AngleAngleChain[a_,{A___,Times[x_,y__],B___},b_]/;NumericQ[x]:=x*AngleAngleChain[a,{A,Times[y],B},b]


(* ::Subsection:: *)
(*Square-square Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


SquareSquareChainBox[a_, b_, c__] := 
	TemplateBox[
		{a, b, c}, "SquareSquareChain", 
		DisplayFunction -> (RowBox[{"[", #1, "|", ##3, "|", #2, "]"}]&), 
		InterpretationFunction -> (RowBox[{"SquareSquareChain", "[", RowBox[{#1, ",", "{", TemplateSlotSequence[3, ","], "}", ",", #2}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SquareSquareChain /: MakeBoxes[SquareSquareChain[a_, c_List, b_], StandardForm | TraditionalForm] := SquareSquareChainBox[ToBoxes[a], ToBoxes[b], Sequence @@ (ToBoxes /@ c)]

SquareSquareChain[a_, c_List, b_Plus] := Plus @@ (SquareSquareChain[a, c, #]& /@ (List @@ b))

SquareSquareChain[a_Plus, c_List, b_] := Plus @@ (SquareSquareChain[#, c, b]& /@ (List @@ a))

SquareSquareChain[Times[a_, b__], c_List, d_] /; \[Not]MatchQ[a, SpinorDottedML[][_] | SpinorDottedMV[_][_, _] | SpinorDottedMV[][_]] := a * SquareSquareChain[Times[b], c, d]

SquareSquareChain[d_, c_, Times[a_, b__]] /; \[Not]MatchQ[a, SpinorDottedML[][_] | SpinorDottedMV[_][_, _] | SpinorDottedMV[][_]] := a * SquareSquareChain[d, c, Times[b]]

SquareSquareChain[a_, b_List, c_] /; \[Not]OrderedQ[{a, c}] := -SquareSquareChain[c, Reverse @ b, a]
	
SquareSquareChain[a_,{A___,Times[x_,y__],B___},b_]/;NumericQ[x]:=x*SquareSquareChain[a,{A,Times[y],B},b]


(* ::Subsection:: *)
(*Angle-square Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


AngleSquareChainBox[a_, b_, c__] := 
	TemplateBox[
		{a, b, c}, "AngleSquareChain",
		DisplayFunction -> (RowBox[{"\[LeftAngleBracket]", #1, "|", ##3, "|", #2, "]"}]&), 
		InterpretationFunction -> (RowBox[{"AngleSquareChain", "[", RowBox[{#1, ",", "{", TemplateSlotSequence[3, ","], "}", ",", #2}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


AngleSquareChain /: MakeBoxes[AngleSquareChain[a_, c_List, b_], StandardForm | TraditionalForm] := AngleSquareChainBox[ToBoxes[a], ToBoxes[b], Sequence @@ (ToBoxes /@ c)]

AngleSquareChain[a_, c_List, b_Plus] := Plus @@ (AngleSquareChain[a, c, #]& /@ (List @@ b))

AngleSquareChain[a_Plus, c_List, b_] := Plus @@ (AngleSquareChain[#, c, b]& /@ (List @@ a))

AngleSquareChain[Times[a_, b__], c_List, d_] /; \[Not]MatchQ[a, SpinorUndottedML[][_] | SpinorUndottedMV[_][_, _] | SpinorUndottedMV[][_]] := a * AngleSquareChain[Times[b], c, d]

AngleSquareChain[d_, c_List, Times[a_, b__]] /; \[Not]MatchQ[a, SpinorDottedML[][_] | SpinorDottedMV[_][_, _] | SpinorDottedMV[][_]] := a * AngleSquareChain[d, c, Times[b]]
	
AngleSquareChain[a_,{A___,Times[x_,y__],B___},b_]/;NumericQ[x]:=x*AngleSquareChain[a,{A,Times[y],B},b]


(* ::Subsection:: *)
(*Trace Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


TraceChainBox[c__] := 
	TemplateBox[
		{c}, "TraceChain",
		DisplayFunction -> (RowBox[{SubscriptBox["Tr", "-"], ##}]&), 
		InterpretationFunction -> (RowBox[{"TraceChain", "[", RowBox[{"{", TemplateSlotSequence[1, ","], "}"}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


TraceChain /: MakeBoxes[TraceChain[c_List], StandardForm | TraditionalForm] := TraceChainBox[Sequence @@ (ToBoxes /@ c)]

TraceChain[{A___,Times[x_,y__],B___}]/;NumericQ[x]:=x*TraceChain[{A,Times[y],B}]


(* ::Subsection:: *)
(*Masses*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MassUntildeBox[label_] := 
	TemplateBox[{label}, "MassUntilde", 
		DisplayFunction -> (SubscriptBox["m", RowBox[{#1}]]&), 
		InterpretationFunction -> (RowBox[{"MassUntilde", "[", RowBox[{#1}], "]"}]&)
	]

MassTildeBox[label_] := TemplateBox[{label}, "MassTilde", 
		DisplayFunction -> (SubscriptBox[OverscriptBox["m", "~"], RowBox[{#1}]]&),
		InterpretationFunction -> (RowBox[{"MassTilde", "[", RowBox[{#1}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


MassUntilde /: MakeBoxes[MassUntilde[a_], StandardForm | TraditionalForm] := MassUntildeBox[DdimVariables`ToLabel[a]]

MassTilde /: MakeBoxes[MassTilde[a_], StandardForm | TraditionalForm] := MassTildeBox[DdimVariables`ToLabel[a]]

(*MassTilde /: MassTilde[a_]^A_. MassUntilde[b_]^B_. /; a===b := - Mass[a]^(2*Min[A,B]) * MassTilde[a]^(Max[0,A-B]) * MassUntilde[a]^(Max[0,B-A])*)


(* ::Subsection::Closed:: *)
(*Declare and Undeclare masses*)


$massless={};

DeclareMassless[label_List]:=($massless=DeleteDuplicates@Join[$massless,label]);
DeclareMassless[label_]:=($massless=DeleteDuplicates@Append[$massless,label]);

UndeclareMassless[label_List]:=($massless=Complement[$massless,label]);
UndeclareMassless[label_]:=($massless=DeleteCases[$massless,label]);

ClearMassless[]:=($massless={});


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect @@ 
	DeleteCases[
		Names["NumericalKinematics`*"],
		_?(MatchQ[#,Alternatives @@ (ToString /@ {$massless})]&)
	]

EndPackage[]
