(* ::Package:: *)

BeginPackage["DdimFunctions`",{"YoungSymm`"}]


(* ::Section:: *)
(*Messages*)


RelabelDummies::usage = "..."
Relabel::usage = "..."
FromDotIndices::usage = "..."
ToTrace::usage = "..."

SetMasses::usage = "..."
ZeroMasses::usage = "..."
ClearMasses::usage = "..."

CombinePolarisations::usage = "..."
DecombinePolarisations::usage = "..."


(* ::Section:: *)
(*D-dimensional Functions*)


Begin["`Private`"]


(* ::Text:: *)
(*TODO:*)
(*-Problem: e.g. latin indices after 26 give errors. We should define some sort of index mod 26 which adds a number to the letter after 26. {a,b...z,a1,b1,...z1,a2,...}*)


(* ::Subsection::Closed:: *)
(*Relabel Dummies*)


Options[RelabelDummies] = {"Indices" -> "Greek"}; 

RelabelDummies[OptionsPattern[]][x_Plus] :=
	Plus @@ (RelabelDummies["Indices" -> OptionValue["Indices"]][#]& /@ 
		(List @@ x))

RelabelDummies[OptionsPattern[]][exp_] :=
	Module[{dummies, indices, newdummies},
		dummies = (*Join[Cases[exp, HoldPattern[DdimVariables`Momentum[_][h_]
			] :> h, \[Infinity]], Cases[exp, HoldPattern[DdimVariables`EpsilonPol[_][h__]] 
			:> h, \[Infinity]], Cases[exp, HoldPattern[DdimVariables`FieldStr[_][h__]] :> h,
			 \[Infinity]], Cases[exp, HoldPattern[DdimVariables`Riemann[_][h__]] :> h, \[Infinity]]];*)
			Join[Cases[exp, HoldPattern[DdimVariables`EpsilonPol[_][h__]] | HoldPattern[DdimVariables`FieldStr[_][h__]] | HoldPattern[DdimVariables`Riemann[_][h__]] :> h, \[Infinity]], Cases[exp, HoldPattern[DdimVariables`Momentum[_][h_]] :> h, \[Infinity]]];
		
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


(* ::Subsection::Closed:: *)
(*Relabel*)


Options[Relabel] = {"Indices" -> "Greek"}; 

Relabel[OptionsPattern[]][x_Plus, n_:0] :=
	Plus @@ (Relabel["Indices" -> OptionValue["Indices"]][#, n]& /@ (List
		 @@ x))

Relabel[OptionsPattern[]][exp_, n_:0] :=
	Module[{dummies, indices, newindices},
		dummies = (*Join[Cases[exp, HoldPattern[DdimVariables`Momentum[_][h_]
			] :> h, \[Infinity]], Cases[exp, HoldPattern[DdimVariables`EpsilonPol[_][h__]]:> h, \[Infinity]],
			 Cases[exp, HoldPattern[DdimVariables`FieldStr[_][h__]]:> h, \[Infinity]], Cases[exp, 
			HoldPattern[DdimVariables`Riemann[_][h__]] :> h, \[Infinity]]];*)
			   Join[Cases[exp, HoldPattern[DdimVariables`EpsilonPol[_][h__]] | HoldPattern[DdimVariables`FieldStr[_][h__]] | HoldPattern[DdimVariables`Riemann[_][h__]] :> h, \[Infinity]], Cases[exp, HoldPattern[DdimVariables`Momentum[_][h_]] :> h, \[Infinity]]];
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


(* ::Subsection::Closed:: *)
(*ToTrace*)


ToTrace[exp_Plus]:=ToTrace/@exp
ToTrace[Times[a___,b_,c___]]/;MatchQ[b,Power[_,_?(#<0&)]]:=b*ToTrace[Times[a,c]]

ToTrace[exp_]:=
ReplaceRepeated[
ReplaceRepeated[
ReplaceRepeated[
DecombinePolarisations[exp]//Expand,
{
f_[x_][a_]g_[y_][a_]/;MatchQ[f,DdimVariables`EpsilonPol|DdimVariables`Momentum]&&MatchQ[g,DdimVariables`EpsilonPol|DdimVariables`Momentum]:>DdimVariables`DotProduct[f[x],g[y]]
}
],
{
f_[x_][a_]DdimVariables`FieldStr[y_][a_,b_]/;MatchQ[f,DdimVariables`EpsilonPol|DdimVariables`Momentum]:>DdimVariables`FTrace[f[x],{DdimVariables`FieldStr[y]}][b],
f_[x_][b_]DdimVariables`FieldStr[y_][a_,b_]/;MatchQ[f,DdimVariables`EpsilonPol|DdimVariables`Momentum]:>-DdimVariables`FTrace[f[x],{DdimVariables`FieldStr[y]}][a],
DdimVariables`FTrace[x_,y_][a_]DdimVariables`FieldStr[z_][a_,b_]:>DdimVariables`FTrace[x,Append[y,DdimVariables`FieldStr[z]]][b],
DdimVariables`FTrace[x_,y_][b_]DdimVariables`FieldStr[z_][a_,b_]:>-DdimVariables`FTrace[x,Append[y,DdimVariables`FieldStr[z]]][a],
f_[x_][a_]DdimVariables`FTrace[y_,z_List][a_]/;MatchQ[f,DdimVariables`EpsilonPol|DdimVariables`Momentum]:>DdimVariables`FTrace[y,z,f[x]],
DdimVariables`FTrace[x_,z1_List][a_] DdimVariables`FTrace[y_,z2_List][a_]:>(-1)^(Length[z2])DdimVariables`FTrace[x,Join[z1,Reverse@z2],y]
}
],
{
DdimVariables`FieldStr[x_][a_,b_]DdimVariables`FieldStr[y_][b_,c_]:>DdimVariables`FTrace[DdimVariables`FieldStr[x],DdimVariables`FieldStr[y]][a,c],
DdimVariables`FieldStr[x_][a_,c_]DdimVariables`FieldStr[y_][b_,c_]:>-DdimVariables`FTrace[DdimVariables`FieldStr[x],DdimVariables`FieldStr[y]][a,b],
DdimVariables`FTrace[x__][a_,b_]DdimVariables`FieldStr[y_][b_,c_]:>DdimVariables`FTrace[x,DdimVariables`FieldStr[y]][a,c],
DdimVariables`FTrace[x__][a_,c_]DdimVariables`FieldStr[y_][b_,c_]:>-DdimVariables`FTrace[x,DdimVariables`FieldStr[y]][a,b],
DdimVariables`FTrace[x__][a_,b_]DdimVariables`FTrace[y__][b_,c_]:>DdimVariables`FTrace[x,y][a,c],
DdimVariables`FTrace[x__][a_,c_]DdimVariables`FTrace[y__][b_,c_]:>(-1)^(Length[{y}])*DdimVariables`FTrace[x,Sequence@@Reverse@{y}][a,b],
DdimVariables`FTrace[x__][a_,a_]:>DdimVariables`FTrace[{x}]
}
]


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Combine Polarisation Vectors*)


CombinePolarisations[exp_]:=ReplaceRepeated[exp, {DdimVariables`EpsilonPol[a_][\[Mu]_] DdimVariables`EpsilonPol[a_][\[Nu]_] :> DdimVariables`EpsilonPol[a][\[Mu],\[Nu]],DdimVariables`FieldStr[a_][\[Mu]_,\[Nu]_] DdimVariables`FieldStr[a_][\[Rho]_,\[Sigma]_] :> DdimVariables`Riemann[a][\[Mu],\[Nu],\[Rho],\[Sigma]]}]

DecombinePolarisations[exp_]:=ReplaceRepeated[exp, {DdimVariables`EpsilonPol[a_][\[Mu]_,\[Nu]_]:>DdimVariables`EpsilonPol[a][\[Mu]] DdimVariables`EpsilonPol[a][\[Nu]],DdimVariables`Riemann[a_][\[Mu]_,\[Nu]_,\[Rho]_,\[Sigma]_]:>DdimVariables`FieldStr[a][\[Mu],\[Nu]] DdimVariables`FieldStr[a][\[Rho],\[Sigma]]}]


(* ::Subsection:: *)
(*SetMasses*)


SetMasses[masses_List]:=
(
Unprotect[DdimVariables`DotProduct];
Set@@@Transpose[{(DdimVariables`DotProduct[DdimVariables`Momentum[#],DdimVariables`Momentum[#]]&/@masses),(DdimVariables`Mass[#]^2&/@masses)}];
Set@@@Transpose[{(DdimVariables`DotProduct[DdimVariables`Velocity[#],DdimVariables`Velocity[#]]&/@masses),Table[1,Length@masses]}];
Protect[DdimVariables`DotProduct];
)


(* ::Subsection:: *)
(*ZeroMasses*)


ZeroMasses[masses_List]:=(
Unprotect[DdimVariables`Mass];
Set@@@Transpose@{DdimVariables`Mass/@masses,Table[0,Length@masses]};
Protect[DdimVariables`Mass];
)


(* ::Subsection:: *)
(*ClearMasses*)


ClearDownValues[f_]:=DownValues[f]=DeleteCases[DownValues[f],_?(FreeQ[First[#],Pattern]&)]


ClearMasses[]:=(
Unprotect[DdimVariables`Mass,DdimVariables`DotProduct];
ClearDownValues[DdimVariables`Mass];
ClearDownValues[DdimVariables`DotProduct];
Protect[DdimVariables`Mass,DdimVariables`DotProduct];
)


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect@@Names["DdimFunctions`*"]


EndPackage[]
