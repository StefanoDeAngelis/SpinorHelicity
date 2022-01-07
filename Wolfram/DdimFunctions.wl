(* ::Package:: *)

BeginPackage["DdimFunctions`",{"YoungSymm`"}]


(* ::Section:: *)
(*Messages*)


RelabelDummies::usage = "..."
Relabel::usage = "..."
FromDotIndices::usage = "..."

CombinePolarisations::usage = "..."
DecombinePolarisations::usage = "..."


(* ::Section:: *)
(*D-dimensional Functions*)


Begin["`Private`"]


(* ::Text:: *)
(*TODO:*)
(*-Problem: e.g. latin indices after 26 give errors. We should define some sort of index mod 26 which adds a number to the letter after 26. {a,b...z,a1,b1,...z1,a2,...}*)


(* ::Subsection:: *)
(*Relabel Dummies*)


Options[RelabelDummies] = {"Indices" -> "Greek"}; 

RelabelDummies[OptionsPattern[]][x_Plus] :=
	Plus @@ (RelabelDummies["Indices" -> OptionValue["Indices"]][#]& /@ 
		(List @@ x))

RelabelDummies[OptionsPattern[]][exp_] :=
	Module[{dummies, indices, newdummies},
		dummies = Join[Cases[exp, HoldPattern[DdimVariables`EpsilonPol[_][h__
			]] | HoldPattern[DdimVariables`FieldStr[_][h__]] | HoldPattern[DdimVariables`Riemann[
			_][h__]] :> h, \[Infinity]], Cases[exp, HoldPattern[DdimVariables`Momentum[_][h_
			]] :> h, \[Infinity]]];
		indices = DeleteDuplicates[dummies];
		dummies = Select[Tally[dummies], #[[2]] > 1&][[All, 1]];
		newdummies =
			Table[
				ToExpression @
					FromCharacterCode[
						If[OptionValue["Indices"] == "Greek",
							944 + i
							,
							96 + i
						]
					]
				,
				{i, Length @ indices}
			];
		newdummies = Complement[newdummies, Complement[indices, dummies]][[
			 ;; Length @ dummies]];
		YoungSymm`ReLabel[exp, dummies, newdummies]
	]


(* ::Subsection:: *)
(*Relabel*)


Options[Relabel] = {"Indices" -> "Greek"}; 

Relabel[OptionsPattern[]][x_Plus,n_:0] :=
	Plus @@ (RelabelDummies["Indices" -> OptionValue["Indices"]][#,n]& /@ 
		(List @@ x))

Relabel[OptionsPattern[]][exp_, n_:0] :=
	Module[{dummies, indices, newindices},
		dummies = Join[Cases[exp, HoldPattern[DdimVariables`EpsilonPol[_][h__
			]] | HoldPattern[DdimVariables`FieldStr[_][h__]] | HoldPattern[DdimVariables`Riemann[
			_][h__]] :> h, \[Infinity]], Cases[exp, HoldPattern[DdimVariables`Momentum[_][h_
			]] :> h, \[Infinity]]];
		indices = DeleteDuplicates[dummies];
		dummies = Select[Tally[dummies], #[[2]] > 1&][[All, 1]];
		newindices =
			Table[
				ToExpression @
					FromCharacterCode[
						If[OptionValue["Indices"] == "Greek",
							944 + i + n
							,
							96 + i + n
						]
					]
				,
				{i, Length @ indices}
			];
		indices = SortBy[indices, MemberQ[dummies, #]&];
		YoungSymm`ReLabel[exp, indices, newindices]
	]


(* ::Subsection:: *)
(*From Dot To Indices*)


Options[FromDotIndices] = {"Indices" -> "Greek"}; 

FromDotIndices[OptionsPattern[]][exp_Plus,n_:0] :=
	Plus @@ (FromDotIndices["Indices" -> OptionValue["Indices"]][#,n]& /@ 
		(List @@ exp))

FromDotIndices[OptionsPattern[]][exp_Times, n_:0] :=
	Times @@
		Module[{i = 1 + n},
			i = i + Length @ Join[Cases[Numerator[exp], HoldPattern[DdimVariables`EpsilonPol[
				_][h__]] | HoldPattern[DdimVariables`FieldStr[_][h__]] | HoldPattern[
				DdimVariables`Riemann[_][h__]] :> h, \[Infinity]], Cases[Numerator[exp], HoldPattern[DdimVariables`Momentum[
				_][h_]] :> h, \[Infinity]]];
			If[MatchQ[#, DdimVariables`DotProduct[_, _]],
				If[OptionValue["Indices"] == "Greek",
					#[[1]][ToExpression @ FromCharacterCode[944 + i]] #[[2]][ToExpression
						 @ FromCharacterCode[944 + (i++)]]
					,
					#[[1]][ToExpression @ FromCharacterCode[96 + i]] #[[2]][ToExpression
						 @ FromCharacterCode[96 + (i++)]]
				]
				,
				#
			]& /@ (Flatten @ ReplaceAll[List @@ Relabel["Indices" -> OptionValue[
				"Indices"]][Numerator[exp], n], Power[x_(*?(MatchQ[Head[#],DotProduct]&)*), y_] :> ConstantArray[x, y]])
		]/Denominator[exp]


(* ::Subsection:: *)
(*Combine Polarisation Vectors*)


CombinePolarisations[exp_]:=ReplaceRepeated[exp, {DdimVariables`EpsilonPol[a_][\[Mu]_] DdimVariables`EpsilonPol[a_][\[Nu]_] :> DdimVariables`EpsilonPol[a][\[Mu],\[Nu]],DdimVariables`FieldStr[a_][\[Mu]_,\[Nu]_] DdimVariables`FieldStr[a_][\[Rho]_,\[Sigma]_] :> DdimVariables`Riemann[a][\[Mu],\[Nu],\[Rho],\[Sigma]]}]

DecombinePolarisations[exp_]:=ReplaceRepeated[exp, {DdimVariables`EpsilonPol[a_][\[Mu]_,\[Nu]_]:>DdimVariables`EpsilonPol[a][\[Mu]] DdimVariables`EpsilonPol[a][\[Nu]],DdimVariables`Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_]:>DdimVariables`FieldStr[a][\[Mu],\[Nu]] DdimVariables`FieldStr[a][\[Rho],\[Sigma]]}]


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["DdimFunctions`*"]


EndPackage[]
