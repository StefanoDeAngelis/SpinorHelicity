(* ::Package:: *)

BeginPackage["DdimPackage`", {"YoungSymm`","DdimVariables`"}]



(* ::Section:: *)
(*Messages*)


RelabelDummies::usage = "..."
Relabel::usage = "..."

FromDotIndices::usage = "..."
ToTrace::usage = "..."

SetMasses::usage = "..."
FixMasses::usage = "..."
ZeroMasses::usage = "..."
ClearMasses::usage = "..."

DDerivative::usage = "..."

CombinePolarisations::usage = "..."
DecombinePolarisations::usage = "..."


(* ::Section:: *)
(*D-dimensional Functions*)


Begin["`Private`"]


(* ::Text:: *)
(*TODO:*)


(*-Problem: e.g. latin indices after 26 give errors. We should define some sort of index mod 26 which adds a number to the letter after 26. {a,b...z,a1,b1,...z1,a2,...}*)
(*          more generally, we need numbered indices, also as an option*)
(*-Feature: DDerivative with respect to a Momentum/EpsilonPol inside FieldStr and for FTraces*)
(*-Feature: make ToTrace faster with UpValues instead of replacement rules*)
(*-Feature: function which open FieldStr into Momentum and EpsilonPol*)

(* Problem: FromDotIndices has to be rewritten to take into account the changes for the momenta and epsilon polarizations. An additional feature is needed for FTrace.*)


(* ::Subsection::Closed:: *)
(*Relabel Dummies*)


Options[RelabelDummies] = {"Indices" -> "Greek"};

RelabelDummies[OptionsPattern[]][x_Plus] := Plus@@(RelabelDummies["Indices"->OptionValue["Indices"]][#]&/@(List @@ x))

RelabelDummies[OptionsPattern[]][exp_] :=
	Module[{dummies, indices, newdummies},
		dummies = (*Join[Cases[exp, HoldPattern[Momentum[_,h_]] :> h, \[Infinity]], Cases[exp, HoldPattern[EpsilonPol[_,h__]] :> h, \[Infinity]], Cases[exp, HoldPattern[FieldStr[_,h__]] :> h,\[Infinity]], Cases[exp, HoldPattern[Riemann[_,h__]] :> h, \[Infinity]]];*)
			Join[
				Cases[
					exp, 
					HoldPattern[EpsilonPol[_,h__]] | HoldPattern[FieldStr[_,h__]] | HoldPattern[Riemann[_,h__]] :> h,
					\[Infinity]
				],
				Cases[
					exp,
					HoldPattern[Momentum[_,h_]] :> h,
					\[Infinity]
				]
			];
		indices = DeleteDuplicates[dummies];
		dummies = Select[Tally[dummies], #[[2]] > 1&][[All, 1]];
		newdummies =
			Table[
				ToExpression @
					FromCharacterCode[
						If[OptionValue["Indices"] == "Greek",
							944 + i,
							96 + i
						]
					]
				,
				{i, Length @ indices}
			];
		newdummies = Complement[newdummies,Complement[indices, dummies]][[;;Length@dummies]];
		YoungSymm`ReLabel[exp, dummies, newdummies]
	]



(* ::Subsection::Closed:: *)
(*Relabel*)


Options[Relabel] = {"Indices" -> "Greek"};

Relabel[OptionsPattern[]][x_Plus, n_:0] := Plus @@ (Relabel["Indices" -> OptionValue["Indices"]][#, n]& /@ (List @@ x))

Relabel[OptionsPattern[]][exp_, n_:0] :=
	Module[{dummies, indices, newindices},
		dummies =(*Join[Cases[exp, HoldPattern[Momentum[_,h_]] :> h, \[Infinity]], Cases[exp, HoldPattern[EpsilonPol[_,h__]]:> h, \[Infinity]], Cases[exp, HoldPattern[FieldStr[_,h__]]:> h, \[Infinity]], Cases[exp, HoldPattern[Riemann[_,h__]] :> h, \[Infinity]]];*)
			Join[
				Cases[
					exp,
					HoldPattern[EpsilonPol[_,h__]] | HoldPattern[FieldStr[_,h__]] | HoldPattern[Riemann[_,h__]] :> h,
					\[Infinity]
				],
				Cases[
					exp,
					HoldPattern[Momentum[_,h_]] :> h,
					\[Infinity]
				]
			];
		indices = DeleteDuplicates[dummies];
		dummies = Select[Tally[dummies], #[[2]] > 1&][[All, 1]];
		newindices =
			Table[
				ToExpression @
					FromCharacterCode[
						If[
							OptionValue["Indices"] == "Greek",
							944 + i + n,
							96 + i + n
						]
					],
				{i, Length @ indices}
			];
		indices = SortBy[indices, MemberQ[dummies, #]&];
		YoungSymm`ReLabel[exp, indices, newindices]
	]



(* ::Subsection:: *)
(*ToTrace*)


ToTrace[exp_Plus] :=
	ToTrace /@ exp

ToTrace[Times[a___, b_, c___]] /; MatchQ[b, Power[_, _ ? (# < 0&)]] :=
	b * ToTrace[Times[a, c]]

ToTrace[exp_] :=
	ReplaceRepeated[
		ReplaceRepeated[
			ReplaceRepeated[
				DecombinePolarisations[exp] // Expand,
					{f_[x_,a_] g_[y_,a_] /; MatchQ[f, EpsilonPol| Momentum] && MatchQ[g, EpsilonPol | Momentum ] :> DotProduct[f[x], g[y]]}
				],
			{
				f_[x_,a_] FieldStr[y_,a_, b_] /; MatchQ[f, EpsilonPol | Momentum] :> FTrace[f[x], {FieldStr[y]}][b],
				f_[x_,b_] FieldStr[y_,a_, b_] /; MatchQ[f, EpsilonPol| Momentum] :> -FTrace[f[x], {FieldStr[y]}][a],
				FTrace[x_, y_][a_] FieldStr[z_,a_, b_] :> FTrace[x, Append[y, FieldStr[z]]][b],
				FTrace[x_, y_][b_] FieldStr[z_,a_,b_] :> -FTrace[x, Append[y, FieldStr[z]]][a],
				f_[x_,a_] FTrace[y_, z_List][a_] /; MatchQ[f, EpsilonPol| Momentum] :> FTrace[y, z, f[x]],
				FTrace[x_, z1_List][a_] FTrace[y_, z2_List][a_] :> (-1) ^ (Length[z2]) FTrace[x, Join[z1, Reverse @ z2], y]
			}
		],
		{
			FieldStr[x_,a_, b_] FieldStr[y_,b_, c_] :> FTrace[FieldStr[x], FieldStr[y]][a, c],
			FieldStr[x_,a_, c_] FieldStr[y_,b_, c_] :> -FTrace[FieldStr[x], FieldStr[y]][a, b],
			FTrace[x__][a_, b_] FieldStr[y_][b_, c_] :> FTrace[x, FieldStr[y]][a, c],
			FTrace[x__][a_, c_] FieldStr[y_][b_, c_] :> -FTrace[x, FieldStr[y]][a, b],
			FTrace[x__][a_, b_] FTrace[y__][b_, c_] :> FTrace[x, y][a, c],
			FTrace[x__][a_, c_] FTrace[y__][b_, c_] :> (-1) ^ (Length[{y}]) *FTrace[x, Sequence @@ Reverse @ {y}][a, b],
			FTrace[x__][a_, a_] :> FTrace[{x}]
		}
	]



(* ::Subsection::Closed:: *)
(*From Dot To Indices*)


(*Options[FromDotIndices] = {"Indices" -> "Greek"};

FromDotIndices[OptionsPattern[]][exp_Plus, n_:0] := Plus @@ (FromDotIndices["Indices" -> OptionValue["Indices"]][#, n]& /@ (List @@ exp))

FromDotIndices[OptionsPattern[]][exp_Times, n_:0] :=
	Times@@
		Module[{i = 1 + n},
			i = i + Length @
				Join[
					Cases[
						Numerator[exp],
						HoldPattern[EpsilonPol[_,h__]] | HoldPattern[FieldStr[_,h__]] | HoldPattern[Riemann[_,h__]] :> h,
						\[Infinity]
					],
					Cases[
						Numerator[exp], 
						HoldPattern[Momentum[_,h_]] :> h, 
						\[Infinity]
					]
				];
			If[
				MatchQ[
					#,
					DotProduct[_, _]
				],
				If[
					OptionValue["Indices"] == "Greek",
					#[[1]][ToExpression@FromCharacterCode[944 + i]] #[[2]][ToExpression @ FromCharacterCode[944 + (i++)]],
					#[[1]][ToExpression @ FromCharacterCode[96 + i]] #[[2]][ToExpression @ FromCharacterCode[96 + (i++)]]
				],
				#
			]&/@ 
				(Flatten@
					ReplaceAll[
						List @@ Relabel["Indices" -> OptionValue["Indices"]][Numerator[exp], n],
						Power[x_(*?(MatchQ[Head[#],DotProduct]&)*), y_] :> ConstantArray[x, y]
					]
				)
			] / Denominator[exp]
*)


(* ::Subsection::Closed:: *)
(*Combine Polarisation Vectors*)


CombinePolarisations[exp_] :=
	ReplaceRepeated[
		exp,
		{
			EpsilonPol[a_, \[Mu]_] EpsilonPol[a_,\[Nu]_] :> EpsilonPol[a, \[Mu], \[Nu]],
			FieldStr[a_, \[Mu]_, \[Nu]_] FieldStr[a_, \[Rho]_, \[Sigma]_] :> Riemann[a, \[Mu], \[Nu], \[Rho], \[Sigma]]
		}
	]

DecombinePolarisations[exp_] :=
	ReplaceRepeated[
		exp, 
		{
			EpsilonPol[a_, \[Mu]_, \[Nu]_] :>EpsilonPol[a, \[Mu]] EpsilonPol[a, \[Nu]],
			Riemann[a_, \[Mu]_, \[Nu]_, \[Rho]_, \[Sigma]_] :> FieldStr[a, \[Mu], \[Nu]] FieldStr[a, \[Rho], \[Sigma]]
		}
	]



(* ::Subsection::Closed:: *)
(*SetMasses*)


SetMasses[masses_List] :=
	(
		Unprotect[DotProduct];
		Set @@@ Transpose[{(DotProduct[Momentum[#], Momentum[#]]& /@ masses), (Mass[#]^2 &/@masses)}];
		Protect[DotProduct];
	)



(* ::Subsection::Closed:: *)
(*FixMasses*)


FixMasses[particles_List,masses_List] /; Length[particles] == Length[masses] :=
	(
		Unprotect[Mass];
		Set @@@ Transpose @ {Mass /@ particles, masses};
		Protect[Mass];
	)


(* ::Subsection::Closed:: *)
(*ZeroMasses*)


ZeroMasses[masses_List] :=
	(
		Unprotect[Mass];
		Set @@@ Transpose @ {Mass /@ masses, Table[0, Length@masses]};
		Protect[Mass];
	)



(* ::Subsection::Closed:: *)
(*ClearMasses*)


ClearDownValues[f_] :=
	DownValues[f] = DeleteCases[DownValues[f], _ ? (FreeQ[First[#], Pattern
		]&)]

ClearMasses[] :=
	(
		Unprotect[Mass, DotProduct];
		ClearDownValues[Mass];
		ClearDownValues[DotProduct];
		Protect[Mass, DotProduct];
	)



(* ::Subsection:: *)
(*DDerivative*)


(* ::Subsubsection::Closed:: *)
(*Auxiliary function: StripoffIndex*)


StripoffIndex[p_]:=p[[;;1]]


(* ::Subsubsection::Closed:: *)
(*DDerivative*)


(*DDerivative[exp_,,p_?(MatchQ[#,Momentum[_,_]|EpsilonPol[_,_]]&)]:=If[Echo@FreeQ[exp,Echo@Head[p]],0]*)
DDerivative[DotProduct[p1_,p2_],p_?(MatchQ[#,Momentum[_,_]|EpsilonPol[_,_]]&)]:=(If[MatchQ[p1,StripoffIndex[p]],Append[p2,p[[2]]],0]+If[MatchQ[p2,StripoffIndex[p]],Append[p1,p[[2]]],0])
DDerivative[Momentum[x_,a_],Momentum[x_,b_]]:=Metric[a,b]
DDerivative[EpsilonPol[x_,a_],EpsilonPol[x_,b_]]:=Metric[a,b]
DDerivative[sum_Plus,p_?(MatchQ[#,Momentum[_,_]|EpsilonPol[_,_]]&)]:=Plus@@(DDerivative[#,p]&/@List@@sum)
DDerivative[Times[a_,b_],p_?(MatchQ[#,Momentum[_,_]|EpsilonPol[_,_]]&)]:=DDerivative[a,p]*Times[b]+a*DDerivative[b,p]
DDerivative[Power[a_,b_],p_?(MatchQ[#,Momentum[_,_]|EpsilonPol[_,_]]&)]:=b*Power[a,b-1]*DDerivative[a,p]
DDerivative[exp_, p_ ? (MatchQ[#, Momentum[_,_]  | EpsilonPol[_,_]]&)]:=0
(*DDerivative[exp_,,p_?(!MatchQ[#,Momentum[_,_]|EpsilonPol[_,_]]&)] define an error message*)


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*Attributes*)


Protect @@ Names["DdimPackage`*"]

EndPackage[]
