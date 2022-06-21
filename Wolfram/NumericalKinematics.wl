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

(* ::Section:: *)

(*Numerical Kinematics*)

(* ::Text:: *)

(*TODOs:*)

(*- Option "Label" in GenerateKinematics*)

(*- Option "MomentumConservation"->False in GenerateKinematics*)

Begin["`Private`"]

(* ::Subsection::Closed:: *)

(*Numerical variables*)

(* ::Subsubsection::Closed:: *)

(*Boxes*)

NSpinorUndottedMLBox[label_] :=
	TemplateBox[{label}, "NSpinorUndottedML", DisplayFunction -> (SubscriptBox[
		"\[Lambda]", RowBox[{#}]]&), InterpretationFunction -> (RowBox[{"NSpinorUndottedML",
		 "[", RowBox[{#}], "]"}]&)]

NSpinorDottedMLBox[label_] :=
	TemplateBox[{label}, "NSpinorDottedML", DisplayFunction -> (SubscriptBox[
		OverscriptBox["\[Lambda]", "~"], RowBox[{#}]]&), InterpretationFunction
		 -> (RowBox[{"NSpinorDottedML", "[", RowBox[{#}], "]"}]&)]

NSpinorUndottedMVBox[label_, indexSpin_] :=
	TemplateBox[{label, indexSpin}, "NSpinorUndottedMV", DisplayFunction
		 -> (SubsuperscriptBox["\[Lambda]", RowBox[{#1}], RowBox[{#2}]]&), InterpretationFunction
		 -> (RowBox[{"NSpinorUndottedMV", "[", RowBox[{#1, ",", #2}], "]"}]&)
		]

NSpinorDottedMVBox[label_, indexSpin_] :=
	TemplateBox[{label, indexSpin}, "NSpinorDottedMV", DisplayFunction ->
		 (SubsuperscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1}], RowBox[
		{#2}]]&), InterpretationFunction -> (RowBox[{"NSpinorDottedMV", "[", 
		RowBox[{#1, ",", #2}], "]"}]&)]

NMomentumBox[label_] :=
	TemplateBox[{label}, "NMomentum", DisplayFunction -> (SubscriptBox["p",
		 RowBox[{#}]]&), InterpretationFunction -> (RowBox[{"NMomentum", "[",
		 RowBox[{#}], "]"}]&)]

(* ::Subsubsection::Closed:: *)

(*Properties*)

NSpinorUndottedML /: MakeBoxes[NSpinorUndottedML[a_], StandardForm | 
	TraditionalForm] :=
	NSpinorUndottedMLBox[ToBoxes[a]]

NSpinorDottedML /: MakeBoxes[NSpinorDottedML[a_], StandardForm | TraditionalForm
	] :=
	NSpinorDottedMLBox[ToBoxes[a]]

NSpinorUndottedMV /: MakeBoxes[NSpinorUndottedMV[a_, b_], StandardForm
	 | TraditionalForm] :=
	NSpinorUndottedMVBox[ToBoxes[a], ToBoxes[b]]

NSpinorDottedMV /: MakeBoxes[NSpinorDottedMV[a_, b_], StandardForm | 
	TraditionalForm] :=
	NSpinorDottedMVBox[ToBoxes[a], ToBoxes[b]]

NMomentum /: MakeBoxes[NMomentum[a_], StandardForm | TraditionalForm] :=
	NMomentumBox[ToBoxes[a]]

(* ::Subsection::Closed:: *)

(*Auxiliary functions*)

MomentumTwistor[p_Integer, d_Integer:4] :=
	RandomInteger[{0, p}, {d}]

MomentumTwistors[n_Integer, p_Integer] :=
	RandomInteger[{0, p}, {n, 4}]

TwoBracket[twi1_List, twi2_List] :=
	twi1[[1]] * twi2[[2]] - twi1[[2]] * twi2[[1]]

OrthogonalTwistor[twi1_List, twi2_List, twi3_List] :=
	Table[
		Sum[
			If[DuplicateFreeQ[{i, j, k, l}],
					Signature[{i, j, k, l}]
					,
					0
				] * twi1[[j]] * twi2[[k]] * twi3[[l]]
			,
			{j, 4}
			,
			{k, 4}
			,
			{l, 4}
		]
		,
		{i, 4}
	]

FourBracket[twi1_List, twi2_List, twi3_List, twi4_List] :=
	Sum[
		If[DuplicateFreeQ[{i, j, k, l}],
				Signature[{i, j, k, l}]
				,
				0
			] * twi1[[i]] * twi2[[j]] * twi3[[k]] * twi4[[l]]
		,
		{i, 4}
		,
		{j, 4}
		,
		{k, 4}
		,
		{l, 4}
	]

DualTwistors[twistors_] :=
	Table[OrthogonalTwistor[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[
		i, #, 1]]], twistors[[Mod[i + 1, #, 1]]]] / (TwoBracket[twistors[[Mod[
		i - 1, #, 1]]], twistors[[Mod[i, #, 1]]]] * TwoBracket[twistors[[Mod[
		i, #, 1]]], twistors[[Mod[i + 1, #, 1]]]]), {i, #}]& @ Length[twistors
		]

PlanarKinematics[twistors_] :=
	Table[FourBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 
		1]]], twistors[[Mod[i + 1, #, 1]]], twistors[[Mod[i + 2, #, 1]]]] / (
		TwoBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 1]]]] * 
		TwoBracket[twistors[[Mod[i + 1, #, 1]]], twistors[[Mod[i + 2, #, 1]]]
		]), {i, #}]& @ Length[twistors]

CheckKinematics[twistors_List] :=
	If[MemberQ[#, 0],
			True
			,
			False
		]& @ Flatten @ (Table[TwoBracket[twistors[[i]], twistors[[j]]], {i,
			 #}, {j, i + 1, #}]& @ Length[twistors])

NEpsilon[a_:1] /; NumberQ[a] :=
	{{0, a}, {-a, 0}}

(* ::Subsection::Closed:: *)

(*Clear Kinematics*)

ClearKinematics[] :=
	(
		ClearAll[NSpinorDottedML, NSpinorUndottedML, NSpinorDottedMV, NSpinorUndottedMV,
			 NMomentum];
		NSpinorUndottedML /: MakeBoxes[NSpinorUndottedML[a_], StandardForm 
			| TraditionalForm] := NumericalKinematics`Private`NSpinorUndottedMLBox[
			ToBoxes[a]];
		NSpinorDottedML /: MakeBoxes[NSpinorDottedML[a_], StandardForm | TraditionalForm
			] := NumericalKinematics`Private`NSpinorDottedMLBox[ToBoxes[a]];
		NSpinorUndottedMV /: MakeBoxes[NSpinorUndottedMV[a_, b_], StandardForm
			 | TraditionalForm] := NumericalKinematics`Private`NSpinorUndottedMVBox[
			ToBoxes[a], ToBoxes[b]];
		NSpinorDottedMV /: MakeBoxes[NSpinorDottedMV[a_, b_], StandardForm 
			| TraditionalForm] := NumericalKinematics`Private`NSpinorDottedMVBox[
			ToBoxes[a], ToBoxes[b]];
		NMomentum /: MakeBoxes[NMomentum[a_], StandardForm | TraditionalForm
			] := NumericalKinematics`Private`NMomentumBox[ToBoxes[a]]
	)

(* ::Subsection:: *)

(*Generate Kinematics*)

Options[GenerateKinematics] = {"Masses" -> {}, "Identical" -> {}, "HighestNumber"
	 -> 5, "Labels" -> "Numeric", "Chirality" -> "Minus"(*or "Plus"*), "Echos"
	 -> True};

GenerateKinematics[n_Integer, OptionsPattern[]] :=
	Module[{twistors, duals, massive = OptionValue["Masses"], x},
		ClearKinematics[];
		twistors = MomentumTwistors[n, OptionValue["HighestNumber"]];
		While[CheckKinematics[twistors], twistors = MomentumTwistors[n, OptionValue[
			"HighestNumber"]]];
		twistors = List /@ twistors;
		If[MatchQ[massive, {}],
			duals = List /@ DualTwistors[Sequence @@@ twistors];
			massive = Table[1, n]
			,(*the fully massless case is the simplest*)
			duals = MapAt[Append[#, MomentumTwistor[OptionValue["HighestNumber"
				]]]&, twistors, List /@ massive];
			While[CheckKinematics[Sequence @@@ duals], duals = MapAt[Append[#,
				 MomentumTwistor[OptionValue["HighestNumber"]]]&, twistors, List /@ massive
				];];
			twistors = duals;(*temporarily, duals are the usual twistors, because we need to insert the additional (massive) ones and check them every time
				*)
			massive = Length /@ duals;
			If[\[Not]MatchQ[OptionValue["Identical"], {}],
				twistors = ReplacePart[twistors, Thread[Rule[#, Table[x[i], {i, Length[
					#]}]]]]& @ (Prepend[{2, 4}, #]& /@ Flatten[Drop[#, 1]& /@ OptionValue[
					"Identical"]]);
				duals =
					If[Length[#] == 2,
							#[[1]]
							,
							0
						]& /@ FoldPairList[TakeDrop, PlanarKinematics[Sequence @@@ twistors
							], massive]; (*the masses*)
				duals = Flatten @ Solve[Equal @@@ Map[duals[[#]]&, Flatten[Subsequences[
					#, {2}]& /@ OptionValue["Identical"], 1], {2}]];
				twistors = twistors /. duals
			];
			duals = FoldPairList[TakeDrop, DualTwistors[Sequence @@@ twistors],
				 massive]
		];
		twistors = Map[Part[#,  ;; 2]&, twistors, {2}];
		duals = Map[Part[#, 3 ;; 4]&, duals, {2}];
		If[CheckKinematics[Sequence @@@ duals] && n > 3,
			GenerateKinematics[n, "Masses" -> OptionValue["Masses"], "Identical"
				 -> OptionValue["Identical"], "HighestNumber" -> OptionValue["HighestNumber"
				]]
			,
			x =
				If[OptionValue["Labels"] == "Numeric",
					Range @ n
					,
					OptionValue["Labels"]
				];
			x =
				Flatten @
					{
						Table[
							If[massive[[i]] == 1,
								{NSpinorUndottedML[x[[i]]]}
								,
								{NSpinorUndottedMV[x[[i]], 1], NSpinorUndottedMV[x[[i]], 2]}
							]
							,
							{i, n}
						]
						,
						Table[
							If[massive[[i]] == 1,
								{NSpinorDottedML[x[[i]]]}
								,
								{NSpinorDottedMV[x[[i]], 1], NSpinorDottedMV[x[[i]], 2]}
							]
							,
							{i, n}
						]
						,
						Table[NMomentum[x[[i]]], {i, n}]
					};
			If[MatchQ[OptionValue["Chirality"], "Plus"],
				{twistors, duals} = {duals, twistors}
			];
			massive = Plus @@@ FoldPairList[TakeDrop, (TensorProduct[Sequence 
				@@ #]& /@ Transpose[{Sequence @@@ twistors, Sequence @@@ duals}]), massive
				];
			twistors = Flatten[twistors, 1];
			duals =
				If[Length[#] == 2,
						Reverse[{-1, 1} * #]
						,
						#
					]& /@ duals;
			Set @@@
				(
					If[OptionValue["Echos"],
							Echo[#]
							,
							#
						]& @ Thread[Rule[x, Flatten[#, 1]& @ {twistors, Flatten[duals, 
							1], massive}]]
				)
		];
	]

(* ::Subsection::Closed:: *)

(*End*)

End[]

(* ::Section:: *)

(*Attributes*)

Protect @@ DeleteCases[Names["NumericalKinematics`*"], _ ? (MatchQ[#,
	 Alternatives @@ (ToString /@ {NSpinorDottedML, NSpinorUndottedML, NSpinorDottedMV,
	 NSpinorUndottedMV, NMomentum})]&)]

EndPackage[]
