(* ::Package:: *)

BeginPackage["HelicityVariables`"]


(* ::Section:: *)
(*Messages*)


SpinorUndottedML::usage = "..."
SpinorDottedML::usage = "..."
SpinorML::usage = "..."

SpinorUndottedMV::usage = "..."
SpinorDottedMV::usage = "..."
SpinorMV::usage = "..."

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


(* ::Section:: *)
(*Spinor Helicity Variables*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Massless spinors*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


SpinorUndottedMLBox[$up][label_,index_]:=
	TemplateBox[{label,index},"SpinorUndottedML",
		DisplayFunction->(SubsuperscriptBox["\[Lambda]",RowBox[{#1}],RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedML[$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

SpinorUndottedMLBox[$down][label_,index_]:=
	TemplateBox[{label,index},"SpinorUndottedDownML",
		DisplayFunction->(SubscriptBox["\[Lambda]",RowBox[{#1,#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedML[$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

SpinorDottedMLBox[$up][label_,index_]:=
	TemplateBox[{label,index},"SpinorDottedUpML",
		DisplayFunction->(SubsuperscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1}],OverscriptBox[RowBox[{#2}],"."]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedML[$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

SpinorDottedMLBox[$down][label_,index_]:=
	TemplateBox[{label,index},"SpinorDottedDownML",
		DisplayFunction->(SubscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1,OverscriptBox[#2,"."]}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedML[$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

SpinorMLBox[label_]:=
	TemplateBox[{label},"SpinorML",
		DisplayFunction->(RowBox[{#1}]&),
		InterpretationFunction -> (RowBox[{"SpinorML","[",RowBox[{#1}],"]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "suum" -> SpinorUndottedMLBox[$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sudm" -> SpinorUndottedMLBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sdum" -> SpinorDottedMLBox[$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sddm" -> SpinorDottedMLBox[$down]["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SpinorUndottedML[$up] /: MakeBoxes[SpinorUndottedML[$up][a_, b_], StandardForm | TraditionalForm] := SpinorUndottedMLBox[$up][ToBoxes[a], ToBoxes[b]]

SpinorUndottedML[$down] /: MakeBoxes[SpinorUndottedML[$down][a_, b_], StandardForm | TraditionalForm] := SpinorUndottedMLBox[$down][ToBoxes[a], ToBoxes[b]]

SpinorDottedML[$up] /: MakeBoxes[SpinorDottedML[$up][a_, b_], StandardForm | TraditionalForm] := SpinorDottedMLBox[$up][ToBoxes[a], ToBoxes[b]]

SpinorDottedML[$down] /: MakeBoxes[SpinorDottedML[$down][a_, b_], StandardForm | TraditionalForm] := SpinorDottedMLBox[$down][ToBoxes[a], ToBoxes[b]]

SpinorML /: MakeBoxes[SpinorML[a_], StandardForm | TraditionalForm] := SpinorMLBox[ToBoxes[a]]


SpinorUndottedML /: SpinorUndottedML[$up][l1_,a_] SpinorUndottedML[$down][l2_,a_] := AngleB[SpinorML[l1],SpinorML[l2]];

SpinorDottedML /: SpinorDottedML[$up][l1_,a_] SpinorDottedML[$down][l2_,a_] := -SquareB[SpinorML[l1],SpinorML[l2]];


(*SpinorUndottedML[pos_][l1_Plus,a_] := Plus @@ (SpinorUndottedML[pos][#,a] &/@ (List@@l1))

SpinorDottedML[pos_][l1_Plus,a_] := Plus @@ (SpinorDottedML[pos][#,a] &/@ (List@@l1))

SpinorML[l_Plus] := Plus @@ (SpinorML /@ List@@l)*)


(* ::Subsection:: *)
(*Massive spinors*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


SpinorUndottedMVBox[$up,$up][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorUndottedMV",
		DisplayFunction->(SubsuperscriptBox["\[Lambda]",RowBox[{#1}],RowBox[{#2,#3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$up,$up]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]
	
SpinorUndottedMVBox[$up,$down][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorUndottedMV",
		DisplayFunction->(SubsuperscriptBox["\[Lambda]",RowBox[{#1,#3}],RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$up,$down]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]

SpinorUndottedMVBox[$down,$up][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorUndottedMV",
		DisplayFunction->(SubsuperscriptBox["\[Lambda]",RowBox[{#1,#2}],RowBox[{#3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$down,$up]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]
	
SpinorUndottedMVBox[$down,$down][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorUndottedMV",
		DisplayFunction->(SubscriptBox["\[Lambda]",RowBox[{#1,#2,#3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorUndottedMV[$down,$down]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]
	
SpinorDottedMVBox[$up,$up][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorDottedMV",
		DisplayFunction->(SubsuperscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1}],RowBox[{OverscriptBox[RowBox[{#2}],"."],#3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$up,$up]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]
	
SpinorDottedMVBox[$up,$down][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorDottedMV",
		DisplayFunction->(SubsuperscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1,#3}],OverscriptBox[RowBox[{#2}],"."]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$up,$down]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]

SpinorDottedMVBox[$down,$up][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorDottedMV",
		DisplayFunction->(SubsuperscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1,OverscriptBox[RowBox[{#2}],"."]}],RowBox[{#3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$down,$up]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]
	
SpinorDottedMVBox[$down,$down][label_,indexLorentz_,indexSpin_]:=
	TemplateBox[{label,indexLorentz,indexSpin},"SpinorDottedMV",
		DisplayFunction->(SubscriptBox[OverscriptBox["\[Lambda]","~"],RowBox[{#1,OverscriptBox[RowBox[{#2}],"."]," ",#3}]]&),
		InterpretationFunction -> (RowBox[{"SpinorDottedMV[$down,$down]","[",RowBox[{#1,",",#2,",",#3}],"]"}]&)
	]
	
SpinorMVBox[$up][label_,spin_]:=
	TemplateBox[{label,spin},"SpinorMV",
		DisplayFunction->(SuperscriptBox[RowBox[{#1}],RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorMV[$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]
	
SpinorMVBox[$down][label_,spin_]:=
	TemplateBox[{label,spin},"SpinorMV",
		DisplayFunction->(SubscriptBox[RowBox[{#1}],RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"SpinorMV[$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]
	
SpinorMVBox[][label_]:=
	TemplateBox[{label},"SpinorMV",
		DisplayFunction->(StyleBox[#,Bold]&),
		InterpretationFunction -> (RowBox[{"SpinorMV[]","[",RowBox[{#}],"]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Suuu" -> SpinorUndottedMVBox[$up,$up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Suud" -> SpinorUndottedMVBox[$up,$down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sudu" -> SpinorUndottedMVBox[$down,$up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sudd" -> SpinorUndottedMVBox[$down,$down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sduu" -> SpinorDottedMVBox[$up,$up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sdud" -> SpinorDottedMVBox[$up,$down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sddu" -> SpinorDottedMVBox[$down,$up]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Sddd" -> SpinorDottedMVBox[$down,$down]["\[SelectionPlaceholder]", "\[Placeholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SpinorUndottedMV[$up,$up] /: MakeBoxes[SpinorUndottedMV[$up,$up][a_, b_,c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$up,$up][ToBoxes[a], ToBoxes[b],ToBoxes[c]]
SpinorUndottedMV[$up,$down] /: MakeBoxes[SpinorUndottedMV[$up,$down][a_, b_,c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$up,$down][ToBoxes[a], ToBoxes[b],ToBoxes[c]]

SpinorUndottedMV[$down,$up] /: MakeBoxes[SpinorUndottedMV[$down,$up][a_, b_,c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$down,$up][ToBoxes[a], ToBoxes[b],ToBoxes[c]]
SpinorUndottedMV[$down,$down] /: MakeBoxes[SpinorUndottedMV[$down,$down][a_, b_,c_], StandardForm | TraditionalForm] := SpinorUndottedMVBox[$down,$down][ToBoxes[a], ToBoxes[b],ToBoxes[c]]

SpinorDottedMV[$up,$up] /: MakeBoxes[SpinorDottedMV[$up,$up][a_, b_,c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$up,$up][ToBoxes[a], ToBoxes[b],ToBoxes[c]]
SpinorDottedMV[$up,$down] /: MakeBoxes[SpinorDottedMV[$up,$down][a_, b_,c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$up,$down][ToBoxes[a], ToBoxes[b],ToBoxes[c]]

SpinorDottedMV[$down,$up] /: MakeBoxes[SpinorDottedMV[$down,$up][a_, b_,c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$down,$up][ToBoxes[a], ToBoxes[b],ToBoxes[c]]
SpinorDottedMV[$down,$down] /: MakeBoxes[SpinorDottedMV[$down,$down][a_, b_,c_], StandardForm | TraditionalForm] := SpinorDottedMVBox[$down,$down][ToBoxes[a], ToBoxes[b],ToBoxes[c]]

SpinorMV[$up] /: MakeBoxes[SpinorMV[$up][a_,J_], StandardForm | TraditionalForm] := SpinorMVBox[$up][ToBoxes[a],ToBoxes[J]]
SpinorMV[$down] /: MakeBoxes[SpinorMV[$down][a_,J_], StandardForm | TraditionalForm] := SpinorMVBox[$down][ToBoxes[a],ToBoxes[J]]
SpinorMV[] /: MakeBoxes[SpinorMV[][a_], StandardForm | TraditionalForm] := SpinorMVBox[][ToBoxes[a]]


SpinorDottedMV /: SpinorDottedMV[pos1_,$down][i_,a_,I_] SpinorDottedMV[pos2_,$up][i_,b_,I_] := MassTilde[i] EpsilonLorentzDotted[pos1,pos2][a,b]

SpinorUndottedMV /: SpinorUndottedMV[pos1_,$up][i_,a_,I_] SpinorUndottedMV[pos2_,$down][i_,b_,I_] := MassUntilde[i] EpsilonLorentzUndotted[pos1,pos2][a,b]


SpinorUndottedMV /: SpinorUndottedMV[$up,pos_][l1_,a_,J_] SpinorUndottedML[$down][l2_,a_] := - AngleB[SpinorML[l2],SpinorMV[pos][l1,J]];
SpinorUndottedMV /: SpinorUndottedMV[$down,pos_][l1_,a_,J_] SpinorUndottedML[$up][l2_,a_] := AngleB[SpinorML[l2],SpinorMV[pos][l1,J]];
SpinorUndottedMV /: SpinorUndottedMV[$down,pos1_][l1_,a_,J_] SpinorUndottedMV[$up,pos2_][l2_,a_,K_] := AngleB[SpinorMV[pos1][l1,J],SpinorMV[pos2][l2,K]];

SpinorDottedMV /: SpinorDottedMV[$up,pos_][l1_,a_,J_] SpinorDottedML[$down][l2_,a_] := SquareB[SpinorML[l2],SpinorMV[pos][l1,J]];
SpinorDottedMV /: SpinorDottedMV[$down,pos_][l1_,a_,J_] SpinorDottedML[$up][l2_,a_] := - SquareB[SpinorML[l2],SpinorMV[pos][l1,J]];
SpinorDottedMV /: SpinorDottedMV[$down,pos1_][l1_,a_,J_] SpinorDottedMV[$up,pos2_][l2_,a_,K_] := - SquareB[SpinorMV[pos1][l1,J],SpinorMV[pos2][l2,K]];


(*SpinorUndottedMV[pos1_,pos2_][l1_Plus,a_,J_] := Plus @@ (SpinorUndottedML[pos1,pos2][#,a,J] &/@ (List@@l1))

SpinorDottedMV[pos1_,pos2_][l1_Plus,a_,J_] := Plus @@ (SpinorDottedML[pos1,pos2][#,a,J] &/@ (List@@l1))

SpinorMV[pos_][l_Plus,J_] := Plus @@ (SpinorML[pos][#,J] &/@ List@@l)*)


(* ::Subsection:: *)
(*Epsilon Tensors - Lorentz*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


EpsilonLorentzUndottedBox[$up,$up][I_,J_]:=
	TemplateBox[{I,J},"EpsilonLorentzUndotted",
		DisplayFunction->(SuperscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonLorentzUndotted[$up,$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

EpsilonLorentzUndottedBox[$down,$up][I_,J_]:=
	TemplateBox[{I,J},"EpsilonLorentzUndotted",
		DisplayFunction->(SubsuperscriptBox["\[Delta]",RowBox[{#1}],RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonLorentzUndotted[$down,$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]
	
EpsilonLorentzUndottedBox[$down,$down][I_,J_]:=
	TemplateBox[{I,J},"EpsilonLorentzUndotted",
		DisplayFunction->(SubscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonLorentzUndotted[$down,$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]
	
EpsilonLorentzDottedBox[$up,$up][I_,J_]:=
	TemplateBox[{I,J},"EpsilonLorentzDotted",
		DisplayFunction->(SuperscriptBox["\[Epsilon]",RowBox[{OverscriptBox[#1,"."],OverscriptBox[#2,"."]}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonLorentzDotted[$up,$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

EpsilonLorentzDottedBox[$down,$up][I_,J_]:=
	TemplateBox[{I,J},"EpsilonLorentzDotted",
		DisplayFunction->(SubsuperscriptBox["\[Delta]",OverscriptBox[#1,"."],OverscriptBox[#2,"."]]&),
		InterpretationFunction -> (RowBox[{"EpsilonLorentzDotted[$down,$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]
	
EpsilonLorentzDottedBox[$down,$down][I_,J_]:=
	TemplateBox[{I,J},"EpsilonLorentzDotted",
		DisplayFunction->(SubscriptBox["\[Epsilon]",RowBox[{OverscriptBox[#1,"."],OverscriptBox[#2,"."]}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonLorentzDotted[$down,$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ELuuu" -> EpsilonLorentzUndottedBox[$up,$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ELudu" -> EpsilonLorentzUndottedBox[$down,$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ELudd" -> EpsilonLorentzUndottedBox[$down,$down]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ELduu" -> EpsilonLorentzDottedBox[$up,$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ELddu" -> EpsilonLorentzDottedBox[$down,$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ELddd" -> EpsilonLorentzDottedBox[$down,$down]["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


(* ::Text:: *)
(*Graphical properties:*)


EpsilonLorentzUndotted[$up,$up] /: MakeBoxes[EpsilonLorentzUndotted[$up,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzUndottedBox[$up,$up][ToBoxes[a], ToBoxes[b]]
EpsilonLorentzUndotted[$down,$up] /: MakeBoxes[EpsilonLorentzUndotted[$down,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzUndottedBox[$down,$up][ToBoxes[a], ToBoxes[b]]
EpsilonLorentzUndotted[$down,$down] /: MakeBoxes[EpsilonLorentzUndotted[$down,$down][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzUndottedBox[$down,$down][ToBoxes[a], ToBoxes[b]]

EpsilonLorentzDotted[$up,$up] /: MakeBoxes[EpsilonLorentzDotted[$up,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzDottedBox[$up,$up][ToBoxes[a], ToBoxes[b]]
EpsilonLorentzDotted[$down,$up] /: MakeBoxes[EpsilonLorentzDotted[$down,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzDottedBox[$down,$up][ToBoxes[a], ToBoxes[b]]
EpsilonLorentzDotted[$down,$down] /: MakeBoxes[EpsilonLorentzDotted[$down,$down][a_, b_], StandardForm | TraditionalForm] := EpsilonLorentzDottedBox[$down,$down][ToBoxes[a], ToBoxes[b]]


(* ::Text:: *)
(*Undotted Epsilon contractions:*)


EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$up,$down][a_,b_]:=EpsilonLorentzUndotted[$down,$up][b,a]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$up][a_,b_] EpsilonLorentzUndotted[$down,pos4_][b_,c_]] := EpsilonLorentzUndotted[pos1,pos4][a,c]
EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$down][a_,b_] EpsilonLorentzUndotted[$up,pos4_][b_,c_]] := EpsilonLorentzUndotted[pos1,pos4][a,c]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$up,pos1_][b_,a_] EpsilonLorentzUndotted[$down,pos4_][b_,c_]] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]
EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$down,pos1_][b_,a_] EpsilonLorentzUndotted[$up,pos4_][b_,c_]] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$up][a_,b_] EpsilonLorentzUndotted[pos4_,$down][c_,b_]] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]
EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$down][a_,b_] EpsilonLorentzUndotted[pos4_,$up][c_,b_]] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]

EpsilonLorentzUndotted[pos_,pos_][a_,b_] /; \[Not]OrderedQ[{a, b}] := - EpsilonLorentzUndotted[pos,pos][b,a]
EpsilonLorentzUndotted[$down,$up][a_,a_] := 2


(* ::Text:: *)
(*Dotted Epsilon contractions:*)


EpsilonLorentzDotted /: EpsilonLorentzDotted[$up,$down][a_,b_]:=EpsilonLorentzDotted[$down,$up][b,a]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$up][a_,b_] EpsilonLorentzDotted[$down,pos4_][b_,c_]] := EpsilonLorentzDotted[pos1,pos4][a,c]
EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$down][a_,b_] EpsilonLorentzDotted[$up,pos4_][b_,c_]] := EpsilonLorentzDotted[pos1,pos4][a,c]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$up,pos1_][b_,a_] EpsilonLorentzDotted[$down,pos4_][b_,c_]] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]
EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$down,pos1_][b_,a_] EpsilonLorentzDotted[$up,pos4_][b_,c_]] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$up][a_,b_] EpsilonLorentzDotted[pos4_,$down][c_,b_]] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]
EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$down][a_,b_] EpsilonLorentzDotted[pos4_,$up][c_,b_]] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]

EpsilonLorentzDotted[pos_,pos_][a_,b_] /; \[Not]OrderedQ[{a, b}] := - EpsilonLorentzDotted[pos,pos][b,a]
EpsilonLorentzDotted[$down,$up][a_,a_] := 2


(* ::Text:: *)
(*Raise/lower Dotted indices:*)


EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$up][a_,b_] SpinorDottedMV[$down,pos3_][mom_,b_,J_]] :=SpinorDottedMV[pos1,pos3][mom,a,J]
EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$down][a_,b_] SpinorDottedMV[$up,pos3_][mom_,b_,J_]] :=SpinorDottedMV[pos1,pos3][mom,a,J]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$up,pos1_][a_,b_] SpinorDottedMV[$down,pos3_][mom_,a_,I_]] := If[MatchQ[pos1,$up],-1,1]SpinorDottedMV[pos1,pos3][mom,b,I]
EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$down,pos1_][a_,b_] SpinorDottedMV[$up,pos3_][mom_,a_,I_]] := If[MatchQ[pos1,$down],-1,1] SpinorDottedMV[pos1,pos3][mom,b,I]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$up][a_,b_] SpinorUndottedMV[$down,pos3_][mom_,b_,J_]] :=SpinorUndottedMV[pos1,pos3][mom,a,J]
EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$down][a_,b_] SpinorUndottedMV[$up,pos3_][mom_,b_,J_]] :=SpinorUndottedMV[pos1,pos3][mom,a,J]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$up,pos1_][a_,b_] SpinorUndottedMV[$down,pos3_][mom_,a_,I_]] := If[MatchQ[pos1,$up],-1,1]SpinorUndottedMV[pos1,pos3][mom,b,I]
EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$down,pos1_][a_,b_] SpinorUndottedMV[$up,pos3_][mom_,a_,I_]] := If[MatchQ[pos1,$down],-1,1] SpinorUndottedMV[pos1,pos3][mom,b,I]


(* ::Text:: *)
(*Raise/lower Undotted indices:*)


EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$up][a_,b_] SpinorDottedML[$down][mom_,b_]] :=SpinorDottedML[pos1][mom,a]
EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[pos1_,$down][a_,b_] SpinorDottedML[$up][mom_,b_]] :=SpinorDottedML[pos1][mom,a]

EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$up,pos1_][a_,b_] SpinorDottedML[$down][mom_,a_]] := If[MatchQ[pos1,$up],-1,1]SpinorDottedML[pos1][mom,b]
EpsilonLorentzDotted /: HoldPattern[EpsilonLorentzDotted[$down,pos1_][a_,b_] SpinorDottedML[$up][mom_,a_]] := If[MatchQ[pos1,$down],-1,1] SpinorDottedML[pos1][mom,b]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$up][a_,b_] SpinorUndottedML[$down][mom_,b_]] :=SpinorUndottedML[pos1][mom,a]
EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[pos1_,$down][a_,b_] SpinorUndottedML[$up][mom_,b_]] :=SpinorUndottedML[pos1][mom,a]

EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$up,pos1_][a_,b_] SpinorUndottedML[$down][mom_,a_]] := If[MatchQ[pos1,$up],-1,1]SpinorUndottedML[pos1][mom,b]
EpsilonLorentzUndotted /: HoldPattern[EpsilonLorentzUndotted[$down,pos1_][a_,b_] SpinorUndottedML[$up][mom_,a_]] := If[MatchQ[pos1,$down],-1,1] SpinorUndottedML[pos1][mom,b]


(* ::Subsection:: *)
(*Epsilon Tensors - Spin*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


EpsilonSpinBox[$up,$up][I_,J_]:=
	TemplateBox[{I,J},"EpsilonSpin",
		DisplayFunction->(SuperscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonSpin[$up,$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]

EpsilonSpinBox[$down,$up][I_,J_]:=
	TemplateBox[{I,J},"EpsilonSpin",
		DisplayFunction->(SubsuperscriptBox["\[Delta]",RowBox[{#1}],RowBox[{#2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonSpin[$down,$up]","[",RowBox[{#1,",",#2}],"]"}]&)
	]
	
EpsilonSpinBox[$down,$down][I_,J_]:=
	TemplateBox[{I,J},"EpsilonSpin",
		DisplayFunction->(SubscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
		InterpretationFunction -> (RowBox[{"EpsilonSpin[$down,$down]","[",RowBox[{#1,",",#2}],"]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ESuu" -> EpsilonSpinBox[$up,$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ESdu" -> EpsilonSpinBox[$down,$up]["\[SelectionPlaceholder]", "\[Placeholder]"]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ESdd" -> EpsilonSpinBox[$down,$down]["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


EpsilonSpin[$up,$up] /: MakeBoxes[EpsilonSpin[$up,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonSpinBox[$up,$up][ToBoxes[a], ToBoxes[b]]
EpsilonSpin[$down,$up] /: MakeBoxes[EpsilonSpin[$down,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonSpinBox[$down,$up][ToBoxes[a], ToBoxes[b]]
EpsilonSpin[$down,$down] /: MakeBoxes[EpsilonSpin[$down,$down][a_, b_], StandardForm | TraditionalForm] := EpsilonSpinBox[$down,$down][ToBoxes[a], ToBoxes[b]]


EpsilonSpin /: EpsilonSpin[$up,$down][a_,b_]:=EpsilonSpin[$down,$up][b,a]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$up][a_,b_] EpsilonSpin[$down,pos4_][b_,c_]] := EpsilonSpin[pos1,pos4][a,c]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$down][a_,b_] EpsilonSpin[$up,pos4_][b_,c_]] := EpsilonSpin[pos1,pos4][a,c]

EpsilonSpin /: HoldPattern[EpsilonSpin[$up,pos1_][b_,a_] EpsilonSpin[$down,pos4_][b_,c_]] := If[MatchQ[pos1,$up],-1,1]EpsilonSpin[pos1,pos4][a,c]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down,pos1_][b_,a_] EpsilonSpin[$up,pos4_][b_,c_]] := If[MatchQ[pos1,$down],-1,1]EpsilonSpin[pos1,pos4][a,c]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$up][a_,b_] EpsilonSpin[pos4_,$down][c_,b_]] := If[MatchQ[pos1,$up],-1,1]EpsilonSpin[pos1,pos4][a,c]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$down][a_,b_] EpsilonSpin[pos4_,$up][c_,b_]] := If[MatchQ[pos1,$down],-1,1]EpsilonSpin[pos1,pos4][a,c]

EpsilonSpin[pos_,pos_][a_,b_] /; \[Not]OrderedQ[{a, b}] := - EpsilonSpin[pos,pos][b,a]
EpsilonSpin[$down,$up][a_,a_] := 2


EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$up][I_,J_] SpinorDottedMV[pos3_,$down][mom_,a_,J_]] :=SpinorDottedMV[pos3,pos1][mom,a,I]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$down][I_,J_] SpinorDottedMV[pos3_,$up][mom_,a_,J_]] :=SpinorDottedMV[pos3,pos1][mom,a,I]

EpsilonSpin /: HoldPattern[EpsilonSpin[$up,pos1_][I_,J_] SpinorDottedMV[pos3_,$down][mom_,a_,I_]] := If[MatchQ[pos1,$up],-1,1]SpinorDottedMV[pos3,pos1][mom,a,J]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down,pos1_][I_,J_] SpinorDottedMV[pos3_,$up][mom_,a_,I_]] := If[MatchQ[pos1,$down],-1,1] SpinorDottedMV[pos3,pos1][mom,a,J]

EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$up][I_,J_] SpinorUndottedMV[pos3_,$down][mom_,a_,J_]] :=SpinorUndottedMV[pos3,pos1][mom,a,I]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$down][I_,J_] SpinorUndottedMV[pos3_,$up][mom_,a_,J_]] :=SpinorUndottedMV[pos3,pos1][mom,a,I]

EpsilonSpin /: HoldPattern[EpsilonSpin[$up,pos1_][I_,J_] SpinorUndottedMV[pos3_,$down][mom_,a_,I_]] := If[MatchQ[pos1,$up],-1,1]SpinorUndottedMV[pos3,pos1][mom,a,J]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down,pos1_][I_,J_] SpinorUndottedMV[pos3_,$up][mom_,a_,I_]] := If[MatchQ[pos1,$down],-1,1] SpinorUndottedMV[pos3,pos1][mom,a,J]


EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$up][I_,J_] f_[x___,SpinorMV[$down][mom_,J_],y___]] :=f[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: HoldPattern[EpsilonSpin[pos1_,$down][I_,J_] f_[x___,SpinorMV[$up][mom_,J_],y___]] :=f[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: HoldPattern[EpsilonSpin[$up,pos1_][I_,J_] f_[x___,SpinorMV[$down][mom_,I_],y___]] :=If[MatchQ[pos1,$up],-1,1]f[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: HoldPattern[EpsilonSpin[$down,pos1_][I_,J_] f_[x___,SpinorMV[$up][mom_,I_],y___]] :=If[MatchQ[pos1,$down],-1,1]f[x,SpinorMV[pos1][mom,J],y]


(*EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] SquareB[x___,SpinorMV[$down][mom_,J_],y___] :=SquareB[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] SquareB[x___,SpinorMV[$up][mom_,J_],y___] :=SquareB[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] SquareB[x___,SpinorMV[$down][mom_,I_],y___] :=If[MatchQ[pos1,$up],-1,1]SquareB[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] SquareB[x___,SpinorMV[$up][mom_,I_],y___] :=If[MatchQ[pos1,$down],-1,1]SquareB[x,SpinorMV[pos1][mom,J],y]

EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] AngleB[x___,SpinorMV[$down][mom_,J_],y___] :=AngleB[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] AngleB[x___,SpinorMV[$up][mom_,J_],y___] :=AngleB[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] AngleB[x___,SpinorMV[$down][mom_,I_],y___] :=If[MatchQ[pos1,$up],-1,1]AngleB[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] AngleB[x___,SpinorMV[$up][mom_,I_],y___] :=If[MatchQ[pos1,$down],-1,1]AngleB[x,SpinorMV[pos1][mom,J],y]*)


(*EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] AngleAngleChain[x___,SpinorMV[$down][mom_,J_],y___] :=AngleAngleChain[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] AngleAngleChain[x___,SpinorMV[$up][mom_,J_],y___] :=AngleAngleChain[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] AngleAngleChain[x___,SpinorMV[$down][mom_,I_],y___] :=If[MatchQ[pos1,$up],-1,1]AngleAngleChain[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] AngleAngleChain[x___,SpinorMV[$up][mom_,I_],y___] :=If[MatchQ[pos1,$down],-1,1]AngleAngleChain[x,SpinorMV[pos1][mom,J],y]

EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] SquareSquareChain[x___,SpinorMV[$down][mom_,J_],y___] :=SquareSquareChain[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] SquareSquareChain[x___,SpinorMV[$up][mom_,J_],y___] :=SquareSquareChain[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] SquareSquareChain[x___,SpinorMV[$down][mom_,I_],y___] :=If[MatchQ[pos1,$up],-1,1]SquareSquareChain[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] SquareSquareChain[x___,SpinorMV[$up][mom_,I_],y___] :=If[MatchQ[pos1,$down],-1,1]SquareSquareChain[x,SpinorMV[pos1][mom,J],y]

EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] AngleSquareChain[x___,SpinorMV[$down][mom_,J_],y___] :=AngleSquareChain[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] AngleSquareChain[x___,SpinorMV[$up][mom_,J_],y___] :=AngleSquareChain[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] AngleSquareChain[x___,SpinorMV[$down][mom_,I_],y___] :=If[MatchQ[pos1,$up],-1,1]AngleSquareChain[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] AngleSquareChain[x___,SpinorMV[$up][mom_,I_],y___] :=If[MatchQ[pos1,$down],-1,1]AngleSquareChain[x,SpinorMV[pos1][mom,J],y]*)


(* ::Subsection:: *)
(*Angle brackets*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


AngleBracketBox[a_, b_] :=
    TemplateBox[{a, b}, "AngleB",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",RowBox[{#1,"\[MediumSpace]",#2}],"\[RightAngleBracket]"}]&),
        InterpretationFunction -> (RowBox[{"AngleB","[",RowBox[{#1,",",#2}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "abmm" -> AngleBracketBox[SpinorMLBox["\[SelectionPlaceholder]"],SpinorMLBox["\[Placeholder]"]]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "abmMu" -> AngleBracketBox[SpinorMLBox["\[SelectionPlaceholder]"], SpinorMVBox[$up]["\[Placeholder]","\[Placeholder]"]]]]
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "abmMd" -> AngleBracketBox[SpinorMLBox["\[SelectionPlaceholder]"], SpinorMVBox[$down]["\[Placeholder]","\[Placeholder]"]]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "abMuMu" -> AngleBracketBox[SpinorMVBox[$up]["\[SelectionPlaceholder]","\[Placeholder]"], SpinorMVBox[$up]["\[Placeholder]","\[Placeholder]"]]]]    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "abMdMu" -> AngleBracketBox[SpinorMVBox[$down]["\[SelectionPlaceholder]","\[Placeholder]"], SpinorMVBox[$up]["\[Placeholder]","\[Placeholder]"]]]]    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "abMdMd" -> AngleBracketBox[SpinorMVBox[$down]["\[SelectionPlaceholder]","\[Placeholder]"], SpinorMVBox[$down]["\[Placeholder]","\[Placeholder]"]]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


AngleB /: MakeBoxes[AngleB[a_, b_], StandardForm | TraditionalForm] := AngleBracketBox[ToBoxes[a], ToBoxes[b]]

AngleB[a_,b_Plus]:=Plus@@(AngleB[a,#]&/@(List@@b))
AngleB[a_Plus,b_]:=Plus@@(AngleB[#,b]&/@(List@@a))
AngleB[Times[a_,b__],c_]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*AngleB[Times[b],c]
AngleB[c_,Times[a_,b__]]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*AngleB[c,Times[b]]

AngleB /: HoldPattern[AngleB[a_,SpinorMV[$up][i_,I_]] AngleB[b_,SpinorMV[$down][i_,I_]]] := MassUntilde[i] AngleB[a,b]
AngleB /: HoldPattern[AngleB[SpinorMV[$up][i_,I_],a_] AngleB[b_,SpinorMV[$down][i_,I_]]] := -MassUntilde[i] AngleB[a,b]
AngleB /: HoldPattern[AngleB[a_,SpinorMV[$up][i_,I_]] AngleB[SpinorMV[$down][i_,I_],b_]] := -MassUntilde[i] AngleB[a,b]
AngleB /: HoldPattern[AngleB[SpinorMV[$up][i_,I_],a_] AngleB[SpinorMV[$down][i_,I_],b_]] := MassUntilde[i] AngleB[a,b]


AngleB[a_, b_] /; (a == b) := 0
AngleB[a_, b_] /; \[Not]OrderedQ[{a,b}] := -AngleB[b, a]


AngleB[SpinorMV[pos1_][a_,I_],SpinorMV[pos2_][a_,J_]]:=EpsilonSpin[pos1,pos2][I,J]*MassUntilde[a]


AngleB[a_Plus,b_] := Plus@@ (AngleB[#,b] &/@ List@@ a)
AngleB[a_,b_Plus] := Plus@@ (AngleB[a,#] &/@ List@@ b)


AngleB /: HoldPattern[AngleB[a_,SpinorMV[pos1_][i_,I_]]SquareSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],1,-1]*AngleSquareChain[a,Prepend[list,i],b]
AngleB /: HoldPattern[AngleB[SpinorMV[pos1_][i_,I_],a_]SquareSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := (*If[\[Not]MatchQ[pos1,pos2],0(*errore!*),*)If[MatchQ[pos1,$up],-1,1]*AngleSquareChain[a,Prepend[list,i],b](*]*)
AngleB /: HoldPattern[AngleB[a_,SpinorMV[pos1_][i_,I_]]SquareSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],-1,1]*AngleSquareChain[b,Append[list,i],a]
AngleB /: HoldPattern[AngleB[SpinorMV[pos1_][i_,I_],a_]SquareSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],1,-1]*AngleSquareChain[b,Append[list,i],a]


AngleB /: HoldPattern[AngleB[a_,SpinorMV[pos1_][i_,I_]] AngleSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],-1,1]*AngleAngleChain[b,Append[list,i],a]
AngleB /: HoldPattern[AngleB[SpinorMV[pos1_][i_,I_],a_] AngleSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],1,-1]*AngleAngleChain[b,Append[list,i],a]

AngleB /: HoldPattern[AngleB[a_,SpinorMV[pos1_][i_,I_]]AngleSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]MassUntilde[i]AngleSquareChain[a,list,b]
AngleB /: HoldPattern[AngleB[SpinorMV[pos1_][i_,I_],a_]AngleSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]MassUntilde[i]AngleSquareChain[a,list,b]


AngleB /: HoldPattern[AngleB[a_,SpinorMV[pos1_][i_,I_]]AngleAngleChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]MassUntilde[i]AngleAngleChain[a,list,b]
AngleB /: HoldPattern[AngleB[SpinorMV[pos1_][i_,I_],a_]AngleAngleChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]MassUntilde[i]AngleAngleChain[a,list,b]
AngleB /: HoldPattern[AngleB[SpinorMV[pos1_][i_,I_],a_]AngleAngleChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]MassUntilde[i]AngleAngleChain[b,list,a]
AngleB /: HoldPattern[AngleB[a_,SpinorMV[pos1_][i_,I_]]AngleAngleChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]MassUntilde[i]AngleAngleChain[b,list,a]


(* ::Subsection:: *)
(*Square brackets*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


SquareBracketBox[a_, b_] :=
    TemplateBox[{a, b}, "SquareB",
        DisplayFunction -> (RowBox[{"[",RowBox[{#1,"\[MediumSpace]",#2}],"]"}]&),
        InterpretationFunction -> (RowBox[{"SquareB","[",RowBox[{#1,",",#2}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Shortcuts*)


SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sbmm" -> SquareBracketBox[SpinorMLBox["\[SelectionPlaceholder]"], SpinorMLBox["\[Placeholder]"]]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sbmMu" -> SquareBracketBox[SpinorMLBox["\[SelectionPlaceholder]"], SpinorMVBox[$up]["\[Placeholder]","\[Placeholder]"]]]]
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sbmMd" -> SquareBracketBox[SpinorMLBox["\[SelectionPlaceholder]"], SpinorMVBox[$down]["\[Placeholder]","\[Placeholder]"]]]]
    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sbMuMu" -> SquareBracketBox[SpinorMVBox[$up]["\[SelectionPlaceholder]","\[Placeholder]"], SpinorMVBox[$up]["\[Placeholder]","\[Placeholder]"]]]]    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sbMdMu" -> SquareBracketBox[SpinorMVBox[$down]["\[SelectionPlaceholder]","\[Placeholder]"], SpinorMVBox[$up]["\[Placeholder]","\[Placeholder]"]]]]    
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sbMdMd" -> SquareBracketBox[SpinorMVBox[$down]["\[SelectionPlaceholder]","\[Placeholder]"], SpinorMVBox[$down]["\[Placeholder]","\[Placeholder]"]]]]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SquareB /: MakeBoxes[SquareB[a_, b_], StandardForm | TraditionalForm] := SquareBracketBox[ToBoxes[a], ToBoxes[b]]

SquareB[a_,b_Plus]:=Plus@@(SquareB[a,#]&/@(List@@b))
SquareB[a_Plus,b_]:=Plus@@(SquareB[#,b]&/@(List@@a))
SquareB[Times[a_,b__],c_]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*SquareB[Times[b],c]
SquareB[c_,Times[a_,b__]]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*SquareB[c,Times[b]]

SquareB /: HoldPattern[SquareB[a_,SpinorMV[$up][i_,I_]] SquareB[b_,SpinorMV[$down][i_,I_]]] := MassTilde[i] SquareB[a,b]
SquareB /: HoldPattern[SquareB[SpinorMV[$up][i_,I_],a_] SquareB[b_,SpinorMV[$down][i_,I_]]] := -MassTilde[i] SquareB[a,b]
SquareB /: HoldPattern[SquareB[a_,SpinorMV[$up][i_,I_]] SquareB[SpinorMV[$down][i_,I_],b_]] := -MassTilde[i] SquareB[a,b]
SquareB /: HoldPattern[SquareB[SpinorMV[$up][i_,I_],a_] SquareB[SpinorMV[$down][i_,I_],b_]] := MassTilde[i] SquareB[a,b]


SquareB[a_, b_] /; (a == b) := 0
SquareB[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SquareB[b, a]

SquareB[SpinorMV[pos1_][a_,I_],SpinorMV[pos2_][a_,J_]]:=EpsilonSpin[pos1,pos2][I,J]*MassTilde[a]

SquareB[a_Plus,b_] :=Plus@@ (SquareB[#,b] &/@ List@@a)
SquareB[a_,b_Plus] :=Plus@@ (SquareB[a,#] &/@ List@@b)


SquareB /: HoldPattern[AngleB[a_,SpinorMV[pos2_][i_,I_]]SquareB[b_,SpinorMV[pos1_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]*AngleSquareChain[a,{i},b]
SquareB /: HoldPattern[AngleB[SpinorMV[pos2_][i_,I_],a_]SquareB[b_,SpinorMV[pos1_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]*AngleSquareChain[a,{i},b]
SquareB /: HoldPattern[AngleB[SpinorMV[pos2_][i_,I_],a_]SquareB[SpinorMV[pos1_][i_,I_],b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]*AngleSquareChain[a,{i},b]
SquareB /: HoldPattern[AngleB[a_,SpinorMV[pos2_][i_,I_]]SquareB[SpinorMV[pos1_][i_,I_],b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]*AngleSquareChain[a,{i},b]


SquareB /: HoldPattern[SquareB[a_,SpinorMV[pos1_][i_,I_]]AngleAngleChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]*AngleSquareChain[b,Reverse@Prepend[list,i],a]
SquareB /: HoldPattern[SquareB[SpinorMV[pos1_][i_,I_],a_]AngleAngleChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := (*If[\[Not]MatchQ[pos1,pos2],0(*errore!*),*)If[MatchQ[pos1,$down],-1,1]*AngleSquareChain[b,Reverse@Prepend[list,i],a](*]*)
SquareB /: HoldPattern[SquareB[a_,SpinorMV[pos1_][i_,I_]]AngleAngleChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]*AngleSquareChain[b,Append[list,i],a]
SquareB /: HoldPattern[SquareB[SpinorMV[pos1_][i_,I_],a_]AngleAngleChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]*AngleSquareChain[b,Append[list,i],a]


SquareB /: HoldPattern[SquareB[a_,SpinorMV[pos1_][i_,I_]] AngleSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]*SquareSquareChain[a,Prepend[list,i],b]
SquareB /: HoldPattern[SquareB[SpinorMV[pos1_][i_,I_],a_] AngleSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]*SquareSquareChain[a,Prepend[list,i],b]

SquareB /: HoldPattern[SquareB[SpinorMV[pos1_][i_,I_],a_]AngleSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]MassTilde[i]AngleSquareChain[b,list,a]
SquareB /: HoldPattern[SquareB[a_,SpinorMV[pos1_][i_,I_]]AngleSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]MassTilde[i]AngleSquareChain[b,list,a]


SquareB /: HoldPattern[SquareB[a_,SpinorMV[pos1_][i_,I_]]SquareSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]MassTilde[i]SquareSquareChain[a,list,b]
SquareB /: HoldPattern[SquareB[SpinorMV[pos1_][i_,I_],a_]SquareSquareChain[SpinorMV[pos2_][i_,I_],list_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]MassTilde[i]SquareSquareChain[a,list,b]
SquareB /: HoldPattern[SquareB[SpinorMV[pos1_][i_,I_],a_]SquareSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]MassTilde[i]SquareSquareChain[b,list,a]
SquareB /: HoldPattern[SquareB[a_,SpinorMV[pos1_][i_,I_]]SquareSquareChain[b_,list_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]MassTilde[i]SquareSquareChain[b,list,a]


(* ::Subsection:: *)
(*Angle-angle Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


AngleAngleChainBox[a_,b_, c__] :=
    TemplateBox[{a, b,Sequence@@(SubscriptBox["p",#]&/@{c})}, "AngleAngleChain",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",#1,"|",TemplateSlotSequence[{3,3-1+Length[{c}]}],"|",#2,"\[RightAngleBracket]"}]&),
        InterpretationFunction -> (RowBox[{"AngleAngleChain","[",RowBox[{#1,",","{",Sequence@@Delete[Flatten@Table[{i,","},{i,{c}}],-1],"}",",",#2}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Properties*)


AngleAngleChain /: MakeBoxes[AngleAngleChain[a_, c_List,b_], StandardForm | TraditionalForm] := AngleAngleChainBox[ToBoxes[a], ToBoxes[b], Sequence@@(ToBoxes/@c)]


AngleAngleChain[a_,c_List,b_Plus]:=Plus@@(AngleAngleChain[a,c,#]&/@(List@@b))
AngleAngleChain[a_Plus,c_List,b_]:=Plus@@(AngleAngleChain[#,c,b]&/@(List@@a))
AngleAngleChain[Times[a_,b__],c_List,d_]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*AngleAngleChain[Times[b],c,d]
AngleAngleChain[d_,c_,Times[a_,b__]]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*AngleAngleChain[d,c,Times[b]]


AngleAngleChain[a_,b_List,c_] /; \[Not]OrderedQ[{a,c}] := -AngleAngleChain[c,Reverse@b,a]


AngleAngleChain /: HoldPattern[AngleAngleChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]SquareSquareChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],1,-1]*AngleSquareChain[a,Join[list1,Prepend[list2,i]],b]
AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorMV[pos1_][i_,I_],list1_List,a_]SquareSquareChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],-1,1]*AngleSquareChain[a,Join[Reverse@list1,Prepend[list2,i]],b]
AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorMV[pos1_][i_,I_],list1_List,a_]SquareSquareChain[b_,list2_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],1,-1]*AngleSquareChain[a,Join[Reverse@list1,Prepend[Reverse@list2,i]],b]
AngleAngleChain /: HoldPattern[AngleAngleChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]SquareSquareChain[b_,list2_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],-1,1]*AngleSquareChain[a,Join[list1,Prepend[Reverse@list2,i]],b]


AngleAngleChain /: HoldPattern[AngleAngleChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]AngleSquareChain[b_,list2_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],-1,1]*AngleAngleChain[b,Join[list2,Prepend[Reverse@list1,i]],a]
AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorMV[pos1_][i_,I_],list1_List,a_]AngleSquareChain[b_,list2_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],1,-1]*AngleAngleChain[b,Join[list2,Prepend[list1,i]],a]

AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorMV[pos1_][i_,I_],list1_List,a_]AngleSquareChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]MassUntilde[i]AngleSquareChain[a,Join[Reverse@list1,list2],b]
AngleAngleChain /: HoldPattern[AngleAngleChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]AngleSquareChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]MassUntilde[i]AngleSquareChain[a,Join[list1,list2],b]


AngleAngleChain /: HoldPattern[AngleAngleChain[SpinorMV[$down][i_,I_],list1_List,a_]AngleAngleChain[SpinorMV[$up][i_,I_],list2_List,b_]] := - MassUntilde[i]AngleAngleChain[a,Join[Reverse@list1,list2],b]
AngleAngleChain /: HoldPattern[AngleAngleChain[a_,list1_List,SpinorMV[$down][i_,I_]]AngleAngleChain[b_,list2_List,SpinorMV[$up][i_,I_]]] := - MassUntilde[i]AngleAngleChain[a,Join[list1,Reverse@list2],b]

AngleAngleChain /: HoldPattern[AngleAngleChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]AngleAngleChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1] MassUntilde[i]AngleAngleChain[a,Join[list1,list2],b]


(*AngleAngleChain /: AngleAngleChain[SpinorMV[$up][i_,I_],list_List,SpinorMV[$down][i_,I_]] := -MassUntilde[i]TraceChain[RotateRight@list]*)
AngleAngleChain /: AngleAngleChain[SpinorMV[$down][i_,I_],list_List,SpinorMV[$up][i_,I_]] := +MassUntilde[i]TraceChain[RotateRight@list]


(* ::Subsection:: *)
(*Square-square Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


SquareSquareChainBox[a_,b_, c__] :=
    TemplateBox[{a, b,Sequence@@(SubscriptBox["p",#]&/@{c})}, "SquareSquareChain",
        DisplayFunction -> (RowBox[{"[",#1,"|",TemplateSlotSequence[{3,3-1+Length[{c}]}],"|",#2,"]"}]&),
        InterpretationFunction -> (RowBox[{"SquareSquareChain","[",RowBox[{#1,",","{",Sequence@@Delete[Flatten@Table[{i,","},{i,{c}}],-1],"}",",",#2}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Properties*)


SquareSquareChain /: MakeBoxes[SquareSquareChain[a_, c_List,b_], StandardForm | TraditionalForm] := SquareSquareChainBox[ToBoxes[a], ToBoxes[b], Sequence@@(ToBoxes/@c)]


SquareSquareChain[a_,c_List,b_Plus]:=Plus@@(SquareSquareChain[a,c,#]&/@(List@@b))
SquareSquareChain[a_Plus,c_List,b_]:=Plus@@(SquareSquareChain[#,c,b]&/@(List@@a))
SquareSquareChain[Times[a_,b__],c_List,d_]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*SquareSquareChain[Times[b],c,d]
SquareSquareChain[d_,c_,Times[a_,b__]]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*SquareSquareChain[d,c,Times[b]]


SquareSquareChain[a_,b_List,c_] /; \[Not]OrderedQ[{a,c}] := -SquareSquareChain[c,Reverse@b,a]


SquareSquareChain /: HoldPattern[SquareSquareChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]AngleSquareChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1]*SquareSquareChain[a,Join[list1,Prepend[list2,i]],b]
SquareSquareChain /: HoldPattern[SquareSquareChain[SpinorMV[pos1_][i_,I_],list1_List,a_]AngleSquareChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],-1,1]*SquareSquareChain[a,Join[Reverse@list1,Prepend[list2,i]],b]

SquareSquareChain /: HoldPattern[SquareSquareChain[SpinorMV[pos1_][i_,I_],list1_List,a_]AngleSquareChain[b_,list2_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],1,-1]MassTilde[i]AngleSquareChain[b,Join[list2,list1],b]
SquareSquareChain /: HoldPattern[SquareSquareChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]AngleSquareChain[b_,list2_List,SpinorMV[pos2_][i_,I_]]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$up],-1,1]MassTilde[i]AngleSquareChain[b,Join[list2,Reverse@list1],b]


SquareSquareChain /: HoldPattern[SquareSquareChain[SpinorMV[$down][i_,I_],list1_List,a_]SquareSquareChain[SpinorMV[$up][i_,I_],list2_List,b_]] := - MassTilde[i]SquareSquareChain[a,Join[Reverse@list1,list2],b]
SquareSquareChain /: HoldPattern[SquareSquareChain[a_,list1_List,SpinorMV[$down][i_,I_]]SquareSquareChain[b_,list2_List,SpinorMV[$up][i_,I_]]] := - MassTilde[i]SquareSquareChain[a,Join[list1,Reverse@list2],b]

SquareSquareChain /: HoldPattern[SquareSquareChain[a_,list1_List,SpinorMV[pos1_][i_,I_]]SquareSquareChain[SpinorMV[pos2_][i_,I_],list2_List,b_]]/;\[Not]MatchQ[pos1,pos2] := If[MatchQ[pos1,$down],1,-1] MassTilde[i]SquareSquareChain[a,Join[list1,list2],b]


(*SquareSquareChain /: SquareSquareChain[SpinorMV[$up][i_,I_],list_List,SpinorMV[$down][i_,I_]] := -MassTilde[i]TraceChain[list]*)
SquareSquareChain /: SquareSquareChain[SpinorMV[$down][i_,I_],list_List,SpinorMV[$up][i_,I_]] := +MassTilde[i]TraceChain[list]


(* ::Subsection:: *)
(*Angle-square Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


AngleSquareChainBox[a_,b_, c__] :=
    TemplateBox[{a, b,Sequence@@(SubscriptBox["p",#]&/@{c})}, "AngleSquareChain",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",#1,"|",TemplateSlotSequence[{3,3-1+Length[{c}]}],"|",#2,"]"}]&),
        InterpretationFunction -> (RowBox[{"AngleSquareChain","[",RowBox[{#1,",","{",Sequence@@Delete[Flatten@Table[{i,","},{i,{c}}],-1],"}",",",#2}],"]"}]&)]


(* ::Subsubsection:: *)
(*Properties*)


AngleSquareChain /: MakeBoxes[AngleSquareChain[a_, c_List,b_], StandardForm | TraditionalForm] := AngleSquareChainBox[ToBoxes[a], ToBoxes[b], Sequence@@(ToBoxes/@c)]


AngleSquareChain[a_,c_List,b_Plus]:=Plus@@(AngleSquareChain[a,c,#]&/@(List@@b))
AngleSquareChain[a_Plus,c_List,b_]:=Plus@@(AngleSquareChain[#,c,b]&/@(List@@a))
AngleSquareChain[Times[a_,b__],c_List,d_]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*AngleSquareChain[Times[b],c,d]
AngleSquareChain[d_,c_,Times[a_,b__]]/;\[Not]MatchQ[a,SpinorML[_]|SpinorMV[_][_,_]|SpinorMV[][_]]:=a*AngleSquareChain[d,c,Times[b]]


AngleSquareChain /: HoldPattern[AngleSquareChain[SpinorMV[$down][i_,I_],list1_List,a_]AngleSquareChain[SpinorMV[$up][i_,I_],list2_List,b_]] := MassUntilde[i]SquareSquareChain[a,Join[Reverse@list1,list2],b]
AngleSquareChain /: HoldPattern[AngleSquareChain[a_,list1_List,SpinorMV[$down][i_,I_]]AngleSquareChain[b_,list2_List,SpinorMV[$up][i_,I_]]] := MassTilde[i]AngleAngleChain[a,Join[list1,Reverse@list2],b]


AngleSquareChain /: HoldPattern[AngleSquareChain[a_,list1_List,SpinorMV[$down][i_,I_]]AngleSquareChain[SpinorMV[$up][i_,I_],list2_List,b_]] := AngleSquareChain[a,Join[list1,{i},list2],b]
AngleSquareChain /: HoldPattern[AngleSquareChain[a_,list1_List,SpinorMV[$up][i_,I_]]AngleSquareChain[SpinorMV[$down][i_,I_],list2_List,b_]] := -AngleSquareChain[a,Join[list1,{i},list2],b]


AngleSquareChain /: AngleSquareChain[SpinorMV[$down][i_,I_],list_List,SpinorMV[$up][i_,I_]] := -TraceChain[Prepend[list,i]]
AngleSquareChain /: AngleSquareChain[SpinorMV[$up][i_,I_],list_List,SpinorMV[$down][i_,I_]] := TraceChain[Prepend[list,i]]


(* ::Subsection:: *)
(*Trace Chain*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


TraceChainBox[c__] :=
    TemplateBox[{Sequence@@(SubscriptBox["p",#]&/@{c})}, "TraceChain",
        DisplayFunction -> (RowBox[{SubscriptBox["Tr","-"],TemplateSlotSequence[{1,Length[{c}]}]}]&),
        InterpretationFunction -> (RowBox[{"TraceChain","[",RowBox[{"{",Sequence@@Delete[Flatten@Table[{i,","},{i,{c}}],-1],"}"}],"]"}]&)]


(* ::Subsubsection::Closed:: *)
(*Properties*)


TraceChain /: MakeBoxes[TraceChain[c_List], StandardForm | TraditionalForm] := TraceChainBox[Sequence@@(ToBoxes/@c)]


(* ::Subsection:: *)
(*Masses*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


MassUntildeBox[label_]:=
	TemplateBox[{label},"Mass",
		DisplayFunction->(SubscriptBox["m",RowBox[{#1}]]&),
		InterpretationFunction -> (RowBox[{"Mass","[",RowBox[{#1}],"]"}]&)
	]
	
MassTildeBox[label_]:=
	TemplateBox[{label},"Mass",
		DisplayFunction->(SubscriptBox[OverscriptBox["m","~"],RowBox[{#1}]]&),
		InterpretationFunction -> (RowBox[{"Mass","[",RowBox[{#1}],"]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


MassUntilde /: MakeBoxes[MassUntilde[a_], StandardForm | TraditionalForm] := MassUntildeBox[ToBoxes[a]]

MassTilde /: MakeBoxes[MassTilde[a_], StandardForm | TraditionalForm] := MassTildeBox[ToBoxes[a]]

(*MassTilde /: MassTilde[a_]^b_. MassUntilde[a_]^c_. := Mass[a]^(2*Min[b,c]) * MassTilde[a]^(Max[0,b-c]) * MassUntilde[a]^(Max[0,c-b])*)


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["HelicityVariables`*"]


EndPackage[]
