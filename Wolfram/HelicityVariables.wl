(* ::Package:: *)

BeginPackage["HelicityVariables`",{"DdimVariables`"}]


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

$up::usage = "..."
$down::usage = "..."

SquareB::usage = "..."
AngleB::usage = "..."


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

EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$up][a_,b_] EpsilonLorentzUndotted[$down,pos4_][b_,c_] := EpsilonLorentzUndotted[pos1,pos4][a,c]
EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$down][a_,b_] EpsilonLorentzUndotted[$up,pos4_][b_,c_] := EpsilonLorentzUndotted[pos1,pos4][a,c]

EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$up,pos1_][b_,a_] EpsilonLorentzUndotted[$down,pos4_][b_,c_] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]
EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$down,pos1_][b_,a_] EpsilonLorentzUndotted[$up,pos4_][b_,c_] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]

EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$up][a_,b_] EpsilonLorentzUndotted[pos4_,$down][c_,b_] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]
EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$down][a_,b_] EpsilonLorentzUndotted[pos4_,$up][c_,b_] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzUndotted[pos1,pos4][a,c]

EpsilonLorentzUndotted[pos_,pos_][a_,b_] /; \[Not]OrderedQ[{a, b}] := - EpsilonLorentzUndotted[pos,pos][b,a]
EpsilonLorentzUndotted[$down,$up][a_,a_] := 2


(* ::Text:: *)
(*Dotted Epsilon contractions:*)


EpsilonLorentzDotted /: EpsilonLorentzDotted[$up,$down][a_,b_]:=EpsilonLorentzDotted[$down,$up][b,a]

EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$up][a_,b_] EpsilonLorentzDotted[$down,pos4_][b_,c_] := EpsilonLorentzDotted[pos1,pos4][a,c]
EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$down][a_,b_] EpsilonLorentzDotted[$up,pos4_][b_,c_] := EpsilonLorentzDotted[pos1,pos4][a,c]

EpsilonLorentzDotted /: EpsilonLorentzDotted[$up,pos1_][b_,a_] EpsilonLorentzDotted[$down,pos4_][b_,c_] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]
EpsilonLorentzDotted /: EpsilonLorentzDotted[$down,pos1_][b_,a_] EpsilonLorentzDotted[$up,pos4_][b_,c_] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]

EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$up][a_,b_] EpsilonLorentzDotted[pos4_,$down][c_,b_] := If[MatchQ[pos1,$up],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]
EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$down][a_,b_] EpsilonLorentzDotted[pos4_,$up][c_,b_] := If[MatchQ[pos1,$down],-1,1]EpsilonLorentzDotted[pos1,pos4][a,c]

EpsilonLorentzDotted[pos_,pos_][a_,b_] /; \[Not]OrderedQ[{a, b}] := - EpsilonLorentzDotted[pos,pos][b,a]
EpsilonLorentzDotted[$down,$up][a_,a_] := 2


(* ::Text:: *)
(*Raise/lower Dotted indices:*)


EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$up][a_,b_] SpinorDottedMV[$down,pos3_][mom_,b_,J_] :=SpinorDottedMV[pos1,pos3][mom,a,J]
EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$down][a_,b_] SpinorDottedMV[$up,pos3_][mom_,b_,J_] :=SpinorDottedMV[pos1,pos3][mom,a,J]

EpsilonLorentzDotted /: EpsilonLorentzDotted[$up,pos1_][a_,b_] SpinorDottedMV[$down,pos3_][mom_,a_,I_] := If[MatchQ[pos1,$up],-1,1]SpinorDottedMV[pos1,pos3][mom,b,I]
EpsilonLorentzDotted /: EpsilonLorentzDotted[$down,pos1_][a_,b_] SpinorDottedMV[$up,pos3_][mom_,a_,I_] := If[MatchQ[pos1,$down],-1,1] SpinorDottedMV[pos1,pos3][mom,b,I]

EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$up][a_,b_] SpinorUndottedMV[$down,pos3_][mom_,b_,J_] :=SpinorUndottedMV[pos1,pos3][mom,a,J]
EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$down][a_,b_] SpinorUndottedMV[$up,pos3_][mom_,b_,J_] :=SpinorUndottedMV[pos1,pos3][mom,a,J]

EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$up,pos1_][a_,b_] SpinorUndottedMV[$down,pos3_][mom_,a_,I_] := If[MatchQ[pos1,$up],-1,1]SpinorUndottedMV[pos1,pos3][mom,b,I]
EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$down,pos1_][a_,b_] SpinorUndottedMV[$up,pos3_][mom_,a_,I_] := If[MatchQ[pos1,$down],-1,1] SpinorUndottedMV[pos1,pos3][mom,b,I]


(* ::Text:: *)
(*Raise/lower Undotted indices:*)


EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$up][a_,b_] SpinorDottedML[$down][mom_,b_] :=SpinorDottedML[pos1][mom,a]
EpsilonLorentzDotted /: EpsilonLorentzDotted[pos1_,$down][a_,b_] SpinorDottedML[$up][mom_,b_] :=SpinorDottedML[pos1][mom,a]

EpsilonLorentzDotted /: EpsilonLorentzDotted[$up,pos1_][a_,b_] SpinorDottedML[$down][mom_,a_] := If[MatchQ[pos1,$up],-1,1]SpinorDottedML[pos1][mom,b]
EpsilonLorentzDotted /: EpsilonLorentzDotted[$down,pos1_][a_,b_] SpinorDottedML[$up][mom_,a_] := If[MatchQ[pos1,$down],-1,1] SpinorDottedML[pos1][mom,b]

EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$up][a_,b_] SpinorUndottedML[$down][mom_,b_] :=SpinorUndottedML[pos1][mom,a]
EpsilonLorentzUndotted /: EpsilonLorentzUndotted[pos1_,$down][a_,b_] SpinorUndottedML[$up][mom_,b_] :=SpinorUndottedML[pos1][mom,a]

EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$up,pos1_][a_,b_] SpinorUndottedML[$down][mom_,a_] := If[MatchQ[pos1,$up],-1,1]SpinorUndottedML[pos1][mom,b]
EpsilonLorentzUndotted /: EpsilonLorentzUndotted[$down,pos1_][a_,b_] SpinorUndottedML[$up][mom_,a_] := If[MatchQ[pos1,$down],-1,1] SpinorUndottedML[pos1][mom,b]


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


EpsilonSpin[$up,$up] /: MakeBoxes[EpsilonSpin[$up,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonSpinMVBox[$up,$up][ToBoxes[a], ToBoxes[b]]
EpsilonSpin[$down,$up] /: MakeBoxes[EpsilonSpin[$down,$up][a_, b_], StandardForm | TraditionalForm] := EpsilonSpinMVBox[$down,$up][ToBoxes[a], ToBoxes[b]]
EpsilonSpin[$down,$down] /: MakeBoxes[EpsilonSpin[$down,$down][a_, b_], StandardForm | TraditionalForm] := EpsilonSpinMVBox[$down,$down][ToBoxes[a], ToBoxes[b]]


EpsilonSpin /: EpsilonSpin[$up,$down][a_,b_]:=EpsilonSpin[$down,$up][b,a]

EpsilonSpin /: EpsilonSpin[pos1_,$up][a_,b_] EpsilonSpin[$down,pos4_][b_,c_] := EpsilonSpin[pos1,pos4][a,c]
EpsilonSpin /: EpsilonSpin[pos1_,$down][a_,b_] EpsilonSpin[$up,pos4_][b_,c_] := EpsilonSpin[pos1,pos4][a,c]

EpsilonSpin /: EpsilonSpin[$up,pos1_][b_,a_] EpsilonSpin[$down,pos4_][b_,c_] := If[MatchQ[pos1,$up],-1,1]EpsilonSpin[pos1,pos4][a,c]
EpsilonSpin /: EpsilonSpin[$down,pos1_][b_,a_] EpsilonSpin[$up,pos4_][b_,c_] := If[MatchQ[pos1,$down],-1,1]EpsilonSpin[pos1,pos4][a,c]

EpsilonSpin /: EpsilonSpin[pos1_,$up][a_,b_] EpsilonSpin[pos4_,$down][c_,b_] := If[MatchQ[pos1,$up],-1,1]EpsilonSpin[pos1,pos4][a,c]
EpsilonSpin /: EpsilonSpin[pos1_,$down][a_,b_] EpsilonSpin[pos4_,$up][c_,b_] := If[MatchQ[pos1,$down],-1,1]EpsilonSpin[pos1,pos4][a,c]

EpsilonSpin[pos_,pos_][a_,b_] /; \[Not]OrderedQ[{a, b}] := - EpsilonSpin[pos,pos][b,a]
EpsilonSpin[$down,$up][a_,a_] := 2


EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] SpinorDottedMV[pos3_,$down][mom_,a_,J_] :=SpinorDottedMV[pos3,pos1][mom,a,I]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] SpinorDottedMV[pos3_,$up][mom_,a_,J_] :=SpinorDottedMV[pos3,pos1][mom,a,I]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] SpinorDottedMV[pos3_,$down][mom_,a_,I_] := If[MatchQ[pos1,$up],-1,1]SpinorDottedMV[pos3,pos1][mom,a,J]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] SpinorDottedMV[pos3_,$up][mom_,a_,I_] := If[MatchQ[pos1,$down],-1,1] SpinorDottedMV[pos3,pos1][mom,a,J]

EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] SpinorUndottedMV[pos3_,$down][mom_,a_,J_] :=SpinorUndottedMV[pos3,pos1][mom,a,I]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] SpinorUndottedMV[pos3_,$up][mom_,a_,J_] :=SpinorUndottedMV[pos3,pos1][mom,a,I]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] SpinorUndottedMV[pos3_,$down][mom_,a_,I_] := If[MatchQ[pos1,$up],-1,1]SpinorUndottedMV[pos3,pos1][mom,a,J]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] SpinorUndottedMV[pos3_,$up][mom_,a_,I_] := If[MatchQ[pos1,$down],-1,1] SpinorUndottedMV[pos3,pos1][mom,a,J]


EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] SquareB[x___,SpinorMV[$down][mom_,J_],y___] :=SquareB[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] SquareB[x___,SpinorMV[$up][mom_,J_],y___] :=SquareB[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] SquareB[x___,SpinorMV[$down][mom_,I_],y___] :=If[MatchQ[pos1,$up],-1,1]SquareB[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] SquareB[x___,SpinorMV[$up][mom_,I_],y___] :=If[MatchQ[pos1,$down],-1,1]SquareB[x,SpinorMV[pos1][mom,J],y]

EpsilonSpin /: EpsilonSpin[pos1_,$up][I_,J_] AngleB[x___,SpinorMV[$down][mom_,J_],y___] :=AngleB[x,SpinorMV[pos1][mom,I],y]
EpsilonSpin /: EpsilonSpin[pos1_,$down][I_,J_] AngleB[x___,SpinorMV[$up][mom_,J_],y___] :=AngleB[x,SpinorMV[pos1][mom,I],y]

EpsilonSpin /: EpsilonSpin[$up,pos1_][I_,J_] AngleB[x___,SpinorMV[$down][mom_,I_],y___] :=If[MatchQ[pos1,$up],-1,1]AngleB[x,SpinorMV[pos1][mom,J],y]
EpsilonSpin /: EpsilonSpin[$down,pos1_][I_,J_] AngleB[x___,SpinorMV[$up][mom_,I_],y___] :=If[MatchQ[pos1,$down],-1,1]AngleB[x,SpinorMV[pos1][mom,J],y]


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


SquareB[a_, b_] /; (a == b) := 0
SquareB[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SquareB[b, a]

SquareB[SpinorMV[pos1_][a_,I_],SpinorMV[pos2_][a_,J_]]:=EpsilonSpin[pos1,pos2][I,J]*Mass[a]

SquareB[a_Plus,b_] :=Plus@@ (SquareB[#,b] &/@ List@@a)
SquareB[a_,b_Plus] :=Plus@@ (SquareB[a,#] &/@ List@@b)


(* ::Subsection:: *)
(*Angle brackets*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


AngleBracketBox[a_, b_] :=
    TemplateBox[{a, b}, "AngleB",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",RowBox[{#1,"\[MediumSpace]",#2}],"\[RightAngleBracket]"}]&),
        InterpretationFunction -> (RowBox[{"AngleB","[",RowBox[{#1,",",#2}],"]"}]&)]


(* ::Subsubsection:: *)
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


AngleB[a_, b_] /; (a == b) := 0
AngleB[a_, b_] /; \[Not]OrderedQ[{a,b}] := -AngleB[b, a]


AngleB[SpinorMV[pos1_][a_,I_],SpinorMV[pos2_][a_,J_]]:=EpsilonSpin[pos1,pos2][I,J]*Mass[a]


AngleB[a_Plus,b_] := Plus@@ (AngleB[#,b] &/@ List@@ a)
AngleB[a_,b_Plus] := Plus@@ (AngleB[a,#] &/@ List@@ b)


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["HelicityVariables`*"]


EndPackage[]
