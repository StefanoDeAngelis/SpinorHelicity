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
MassiveParticles::usage = "..."
Identical::usage = "..."
Masses::usage = "..."
InternalTwistors::usage = "..."
HighestNumber::usage = "..."
Chirality::usage = "..."
Echos::usage = "..."
ReturnTwistors::usage = "..."


(* ::Section:: *)
(*Numerical Kinematics*)


(* ::Text:: *)
(*TODOs:*)


(*- Option "Label" in GenerateKinematics*)

(*- Option "MomentumConservation"->False in GenerateKinematics*)

Begin["`Private`"]


(* ::Subsection:: *)
(*Numerical variables*)


(* ::Subsubsection::Closed:: *)
(*Boxes*)


NSpinorUndottedMLBox[label_] :=
	TemplateBox[{label}, "NSpinorUndottedML",
		DisplayFunction -> (SubscriptBox["\[Lambda]", RowBox[{#}]]&),
		InterpretationFunction -> (RowBox[{"NSpinorUndottedML","[", RowBox[{#}], "]"}]&)
	]

NSpinorDottedMLBox[label_] :=
	TemplateBox[{label}, "NSpinorDottedML",
		DisplayFunction -> (SubscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#}]]&), 
		InterpretationFunction -> (RowBox[{"NSpinorDottedML", "[", RowBox[{#}], "]"}]&)
	]

NSpinorUndottedMVBox[label_, indexSpin_] :=
	TemplateBox[{label, indexSpin}, "NSpinorUndottedMV", 
		DisplayFunction -> (SubsuperscriptBox["\[Lambda]", RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"NSpinorUndottedMV", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

NSpinorDottedMVBox[label_, indexSpin_] :=
	TemplateBox[{label, indexSpin}, "NSpinorDottedMV", 
		DisplayFunction -> (SubsuperscriptBox[OverscriptBox["\[Lambda]", "~"], RowBox[{#1}], RowBox[{#2}]]&), 
		InterpretationFunction -> (RowBox[{"NSpinorDottedMV", "[", RowBox[{#1, ",", #2}], "]"}]&)
	]

NMomentumBox[label_] :=
	TemplateBox[{label}, "NMomentum",
		DisplayFunction -> (SubscriptBox["p", RowBox[{#}]]&), 
		InterpretationFunction -> (RowBox[{"NMomentum", "[", RowBox[{#}], "]"}]&)
	]


(* ::Subsubsection::Closed:: *)
(*Properties*)


NSpinorUndottedML /: MakeBoxes[NSpinorUndottedML[a_], StandardForm | TraditionalForm] := NSpinorUndottedMLBox[ToBoxes[a]]

NSpinorDottedML /: MakeBoxes[NSpinorDottedML[a_], StandardForm | TraditionalForm ] := NSpinorDottedMLBox[ToBoxes[a]]

NSpinorUndottedMV /: MakeBoxes[NSpinorUndottedMV[a_, b_], StandardForm | TraditionalForm] := NSpinorUndottedMVBox[ToBoxes[a], ToBoxes[b]]

NSpinorDottedMV /: MakeBoxes[NSpinorDottedMV[a_, b_], StandardForm | TraditionalForm] := NSpinorDottedMVBox[ToBoxes[a], ToBoxes[b]]

NMomentum /: MakeBoxes[NMomentum[a_], StandardForm | TraditionalForm] := NMomentumBox[ToBoxes[a]]


(* ::Subsection::Closed:: *)
(*Auxiliary functions*)


MomentumTwistor[p_Integer, d_Integer:4] := (*RandomInteger[{0, p}, {d}]*)RandomSample[Range[0,p],d]

MomentumTwistors[n_Integer, p_Integer] := (*RandomInteger[{0, p}, {n, 4}]*)Table[MomentumTwistor[p],n]

TwoBracket[twi1_List, twi2_List] := twi1[[1]] * twi2[[2]] - twi1[[2]] * twi2[[1]]

OrthogonalTwistor[twi1_List, twi2_List, twi3_List] :=
	Table[
		Sum[
			If[
				DuplicateFreeQ[{i, j, k, l}],
				Signature[{i, j, k, l}],
				0
			] * twi1[[j]] * twi2[[k]] * twi3[[l]],
			{j, 4},
			{k, 4},
			{l, 4}
		],
		{i, 4}
	]

FourBracket[twi1_List, twi2_List, twi3_List, twi4_List] :=
	Sum[
		If[
			DuplicateFreeQ[{i, j, k, l}],
			Signature[{i, j, k, l}],
			0
		] * twi1[[i]] * twi2[[j]] * twi3[[k]] * twi4[[l]],
		{i, 4},
		{j, 4},
		{k, 4},
		{l, 4}
	]

DualTwistors[twistors_] :=
	Table[
		OrthogonalTwistor[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 1]]], twistors[[Mod[i + 1, #, 1]]]] / (TwoBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 1]]]] * TwoBracket[twistors[[Mod[i, #, 1]]], twistors[[Mod[i + 1, #, 1]]]]),
		{i, #}
	]&@Length[twistors]

PlanarKinematics[twistors_] :=
	Table[
		FourBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #,1]]], twistors[[Mod[i + 1, #, 1]]], twistors[[Mod[i + 2, #, 1]]]] / (TwoBracket[twistors[[Mod[i - 1, #, 1]]], twistors[[Mod[i, #, 1]]]] * TwoBracket[twistors[[Mod[i + 1, #, 1]]], twistors[[Mod[i + 2, #, 1]]]]),
		{i, #}
	]&@Length[twistors]

CheckKinematics[twistors_List] :=
	If[MemberQ[#, 0],
			True,
			False
		]&@Flatten@(Table[TwoBracket[twistors[[i]], twistors[[j]]], {i,#}, {j, i + 1, #}]&@Length[twistors])

NEpsilon[a_:1] /; NumberQ[a] := {{0, a}, {-a, 0}}


(* ::Subsection::Closed:: *)
(*Clear Kinematics*)


ClearKinematics[] :=
	(
		ClearAll[NSpinorDottedML, NSpinorUndottedML, NSpinorDottedMV, NSpinorUndottedMV, NMomentum];
		NSpinorUndottedML /: MakeBoxes[NSpinorUndottedML[a_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorUndottedMLBox[ToBoxes[a]];
		NSpinorDottedML /: MakeBoxes[NSpinorDottedML[a_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorDottedMLBox[ToBoxes[a]];
		NSpinorUndottedMV /: MakeBoxes[NSpinorUndottedMV[a_, b_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorUndottedMVBox[ToBoxes[a], ToBoxes[b]];
		NSpinorDottedMV /: MakeBoxes[NSpinorDottedMV[a_, b_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NSpinorDottedMVBox[ToBoxes[a], ToBoxes[b]];
		NMomentum /: MakeBoxes[NMomentum[a_], StandardForm | TraditionalForm] := NumericalKinematics`Private`NMomentumBox[ToBoxes[a]]
	)



(* ::Subsection:: *)
(*Generate Kinematics*)


Options[GenerateKinematics] = {
   	MassiveParticles -> {},
   	Identical -> {},
   	Masses->{}(*we give rules for fixed masses, the rules must be associated with the first element of the Identical lists*),
   	InternalTwistors -> {}(*for the moment is just for factorisation channels, then we have just one argument. 
   	Prototype: {fixed momentum,{{twistor before},{twistor(s)},{twistor after}}}*),
   	HighestNumber -> 20,
   	Chirality -> "Minus"(*or "Plus"*),
   	Echos -> True,
   	ReturnTwistors -> {}, (*TODO: the function should give an error message if the twistor to be returned is not in the particles list*)
   	FiniteFields -> False
   };

GenerateKinematics[particles_List, opts : OptionsPattern[]] :=
 	Block[
  		{
   			n = Length@particles,
   			massive = Flatten[Position[particles, Alternatives @@ OptionValue[MassiveParticles]]],(*the positions of the massive particles*)
   			identicals = DeleteCases[Flatten[Position[particles, Alternatives @@ #]] & /@ OptionValue[Identical],{}],(*a list of list with the positions of the identical particles*)
   			masses=(Part[#,1]&/@OptionValue[Masses]),
   			massesSquared=OptionValue[Masses],
   			identicalmasses={},
   			twistors,
   			duals,
   			x, (*the numerical variable to be fixed by momenta condition*)
   			y,(*position of the InternalTwistor*)
   			ret (*twistor to be returned*)
   		},
   		
   		If[
   			!(And@@(IntegerQ/@Sqrt[If[OptionValue[FiniteFields],toFF[#],#]&/@(Part[#,2]&/@OptionValue[Masses])])),
   			Message[GenerateKinematics::NonSquareMasses];(*we may want to square the given values*)
   		];
   		
   		If[
   			Or@@(NumericQ/@particles),
   			Message[GenerateKinematics::NumericLabels];
   			Return[$Failed]
   		];
   		
   		ret=massive;
   		If[!MatchQ[identicals,{}],
   			ret=
   				Complement[
   					ret,
   					Flatten[
   						If[
   							MemberQ[
   								#,
   								Alternatives@@
   									Flatten[
   										Position[
   											particles,
   											Alternatives@@
   												If[
   													!MatchQ[OptionValue[InternalTwistors],{}],
   													Append[masses,OptionValue[InternalTwistors][[1]]],
   													masses
   												]
   										]
   									]
   							],
   							#,
   							Delete[#,1]
   						]&/@identicals
   					]
   				]
   		];
   		If[
   			!MatchQ[OptionValue[InternalTwistors],{}],
   			ret=
   				Complement[
   					ret,
   					Flatten@Position[particles,OptionValue[InternalTwistors][[1]]]
   				]
   		];
   		ret=Part[particles,ret];
   		{masses,massesSquared}={Union[masses,ret],Join[massesSquared,Thread[ret->RandomInteger[{1,OptionValue[HighestNumber]},Length@ret]^2]]};
  
  		massive = Table[If[MemberQ[massive, i], 2, 1], {i, n}];(*a list of 2's and 1's corresponding to massive and massless particles, respectively*)
  		
  		identicals=If[Length[#]<2,Nothing,#]&/@identicals (*when we call this function inside CutKinematics, it might be that the subset of identical particles is made of single element*);
  
  		twistors = NumericalKinematics`Private`MomentumTwistors[Total[massive], OptionValue[HighestNumber]];(*2 #(massive) + #(massless) randomly generated momentum twistors*)
  
  		twistors = FoldPairList[TakeDrop, twistors, massive];(*the twistors are grouped into sublists of one or two elements (massless and massive)*)
  
  		ret = (RandomInteger[{1, OptionValue[HighestNumber] - 1}, 2]/OptionValue[HighestNumber]);
  
  		If[
   			\[Not]MatchQ[OptionValue[InternalTwistors], {}], (*the internal twistor appears only at this point*)
   			
   			If[
   				Length[#]==3&&Det[Delete[#,-1]&/@#]==0,
   				Return[$Failed]
   			]&@Partition[Flatten@OptionValue[InternalTwistors][[2]],4];
   			
   			If[
   				MemberQ[masses,OptionValue[InternalTwistors][[1]]]&&
   					(OptionValue[InternalTwistors][[1]]/.massesSquared)!=NumericalKinematics`Private`PlanarKinematics[Partition[Flatten[OptionValue[InternalTwistors][[2]]],4]][[2]]
   					(*check if the mass of the InternalTwistor is the same assigned*),
   				Message[GenerateKinematics::DifferentMasses]
   			];
   			
   			y = Position[particles, OptionValue[InternalTwistors][[1]]][[1, 1]];(*the position of the label of the assigned momentum (one twistor preceed, one or two twistors are associated with the momentum, a final twistor)*)
   			(*In twistor space, a (massless) momentum correspond to a plane, i.e. three twistors*)
   			
   			identicals=If[MemberQ[#,y],RotateLeft[#,Position[#,y][[1,1]]-1],#]&/@identicals; (*if the internal momentum is in a set of identical particles, we choose it to be the first*)
   			
   			twistors = ReplacePart[twistors, {If[y != 1, y - 1, -1], -1} -> ret[[1]]*OptionValue[InternalTwistors][[2, 3]] + (1 - ret[[1]])*OptionValue[InternalTwistors][[2, 2, -1]]];
   			(*If the fixed momentum is the first, then the preceeding twistor must be the last*) (*In general, is the plane which determines the momentum, so we consider a linear combination of the twistors*)
   			twistors = ReplacePart[twistors, {y} -> Reverse@OptionValue[InternalTwistors][[2, 2]]];
   			
   			twistors = ReplacePart[twistors, {If[y != n, y + 1, +1], 1} -> ret[[2]]*OptionValue[InternalTwistors][[2, 1]] + (1 - ret[[2]])*OptionValue[InternalTwistors][[2, 2, 1]]]
   		];
  
  		While[
   			NumericalKinematics`Private`CheckKinematics[Flatten[twistors, 1]],
   			twistors = NumericalKinematics`Private`MomentumTwistors[Total[massive], OptionValue[HighestNumber]];
   			twistors = FoldPairList[TakeDrop, twistors, massive];
   			ret = (RandomInteger[{1, OptionValue[HighestNumber] - 1}, 2]/OptionValue[HighestNumber]);
   			If[\[Not] MatchQ[OptionValue[InternalTwistors], {}],
    				twistors = ReplacePart[twistors, {If[y != 1, y - 1, -1], -1} -> ret[[1]]*OptionValue[InternalTwistors][[2, 3]] + (1 - ret[[1]])*OptionValue[InternalTwistors][[2, 2, -1]]];
    				twistors = ReplacePart[twistors, {y} -> Reverse@OptionValue[InternalTwistors][[2, 2]]];
    				twistors = ReplacePart[twistors, {If[y != n, y + 1, +1], 1} -> ret[[2]]*OptionValue[InternalTwistors][[2, 1]] + (1 - ret[[2]])*OptionValue[InternalTwistors][[2, 2, 1]]];]
   		];(*in case the generated kinematics turns out to be singular, a new one is generated*)
   		
   		If[
   			\[Not]MatchQ[masses, {}]||\[Not]MatchQ[identicals, {}],
   			
   			If[
   				(\[Not]MatchQ[Intersection[#,masses],{}])&&
   					(Length@DeleteDuplicates[Intersection[#,masses]/.massesSquared]!=1) (*the masses have to be the same, if Identical*),
   				Message[GenerateKinematics::InvMasses]
   			]&/@OptionValue[Identical];
   		
   			If[\[Not]SubsetQ[OptionValue[MassiveParticles],masses],Message[GenerateKinematics::NonMassiveMasses]];(*Masses can be assigned only to MassiveParticles*)
   		
   			masses=Flatten[Position[particles, #]&/@masses]; (*the position selection already excludes possible extra assignment (assigned masses which are not in particles)*)
   			
   			If[\[Not]MatchQ[masses, {}],
   			
   				If[\[Not]MatchQ[identicals, {}],
   			
   					identicalmasses=Tuples[{identicals,masses}]; (*if we have assigned masses to one of the particles in an Identical subset, we need to assign the mass to all the particles*)
   					identicalmasses=If[MemberQ[Part[#,1],Part[#,2]],#,Nothing]&/@identicalmasses;
   					identicalmasses=DeleteDuplicates[identicalmasses,MatchQ[#1[[1]],#2[[1]]]&];
   					
   					If[!MatchQ[identicalmasses,{}],
   			
   						identicals=Complement[identicals,Part[#,1]&/@identicalmasses];(*we have three sets: masses for assigned singular masses, identicals for randomly generated mass Identical masses*)
   						masses=Complement[masses,Part[#,2]&/@identicalmasses];(*and identicalmasses for assigned Identical masses*)
   			
   						identicalmasses=MapAt[ReplaceAll[particles[[#]]&,massesSquared],identicalmasses,{All,2}];
   						identicalmasses={#[[1]],ConstantArray[#[[2]],Length@#[[1]]]}&/@identicalmasses;
   						identicalmasses=Transpose/@identicalmasses;
   						identicalmasses=Flatten[identicalmasses,1] (*we obtain a list with the structure: {{position1,mass1},{position2,mass2},...}*)
   					]
   				];
   			
   				masses={#,ReplaceAll[particles[[#]],massesSquared]}&/@masses(*same as the last comment*);
   				
   				(*identicals={#,ConstantArray[RandomInteger[{1,OptionValue[HighestNumber]}](*RandomInteger[{1,OptionValue[HighestNumber]}]/RandomInteger[{1,OptionValue[HighestNumber]}]*),Length@#]}&/@identicals;
   				identicals=Transpose/@identicals;
   				identicals=Flatten[identicals,1];(*we obtain a list with the structure: {{position1,randomMass1},{position2,randomMass2},...}*)*)
   			
   				masses=Join[identicalmasses,masses(*,identicals*)];
   				masses=Transpose[masses];
   				
   				twistors =
    				ReplacePart[
       					twistors,
       					Thread[
        						Rule[
         							#,
         							Table[x[i], {i, ret=Length[#]}](*Now, ret counts the number of variables to be fixed*)
         						]
        					]
       				]&@(
       					ReplaceAll[
      						Prepend[{2, 4}, #] & /@ masses[[1]],(*these are the positions of the assigned masses and identical particles*)
       						{If[y!=1,y-1,n],2,4}->{If[y!=1,y-1,n],1,4}(*if the m-th momentum is identical to the (m-1)-th and it is the internal momentum, we need to accomodate the {1,4},
       						not the {2,4} which is fixed*)
       					]
      					(*we consider the forth element of the second twistor associated to each massive momentum to be fixed by the assigned mass requirement*)
      				),
   				
   				ret=0;
   				masses={{},{}}(*;
   				identicalmasses={}*)
   			];
   				
      		  twistors =
    				  ReplacePart[
    				       twistors,
       					Thread[
        						Rule[
         							#,
         							Table[x[i], {i,ret+1,ret+Length[#]}]
         						]
        					]
       				]&@(
       					ReplaceAll[
      						Prepend[{2, 4}, #] & /@ Flatten[Drop[#, 1] & /@ identicals],(*these are the positions of the identical particles, but the first,
       						to be fed in ReplacePart in position {2,4} for each massive bi-twistor*)
       						{If[y!=1,y-1,n],2,4}->{If[y!=1,y-1,n],1,4}(*if the m-th momentum is identical to the (m-1)-th and it is the internal momentum, we need to accomodate the {1,4},
       						not the {2,4} which is fixed*)
       					]
      					(*the mass of the first particle is automatically generated, the others are fixed accordingly*)
      					(*we consider the forth element of the second twistor associated to each massive momentum to be fixed by the identical mass requirement*)
      				);
      				
      		duals = (*the masses*)
    				If[Length[#]==2,#[[1]],0]&/@(*only the first element of sublists with two elements correspond to a mass*)
    					FoldPairList[TakeDrop,NumericalKinematics`Private`PlanarKinematics[Sequence@@@twistors],massive]; (*all the invariants in terms of twistors*)
    					
    		  identicalmasses=duals[[masses[[1]]]]; (*selecting only assigned masses and identical particles*)
      		
      		duals =
     				Flatten@
     					Solve[
     						Equal@@@
     							Join[
     								MapThread[
     									List,
     									{identicalmasses,masses[[2]]}
     								],
     								Map[
     									duals[[#]] &,
     									Flatten[Subsequences[#, {2}] & /@ identicals, 1],(*gives all subsequences (ordered subsets) containing exactly 2 elements.*)
     									{2}
     								]
     							]
     					];
   			twistors=twistors/.duals;
   			
   		];
   		
   		If[
   			MatchQ[duals,{}], (*it could happen that the kinematic, althought not singular, is problematic: the value of the masses does not depend on x[i] and there is not solution to the previous system.*)
   			(*TODO: This should be traced back to some conditions on the minors of the four twistors determining the masses (see examples below)*)
   			
   			ret=GenerateKinematics[particles, opts];
   			If[\[Not]MatchQ[OptionValue[ReturnTwistors], {}], Return[ret],Return[]]
   		];
   		
   		duals = (*the masses*)
    			If[Length[#]==2,#[[1]],0]&/@(*only the first element of sublists with two elements correspond to a mass*)
    				FoldPairList[TakeDrop,NumericalKinematics`Private`PlanarKinematics[Sequence@@@twistors],massive];
    		If[OptionValue[FiniteFields],duals=toFF@duals];
    	   twistors=Table[If[duals[[i]]==0,twistors[[i]],{twistors[[i,1]],twistors[[i,2]]*Sqrt[duals[[i]]]/(NumericalKinematics`Private`TwoBracket[Sequence@@twistors[[i]]])}],{i,Length@duals}];
  
  		duals=FoldPairList[TakeDrop, NumericalKinematics`Private`DualTwistors[Sequence@@@twistors], massive];(*dual twistors*)
  
  		If[
   			\[Not]MatchQ[OptionValue[ReturnTwistors],{}],
   			ret=List[OptionValue[ReturnTwistors][[1]],{twistors[[If[#!=1,#-1,-1],-1]],twistors[[#]],twistors[[If[#!=n,#+1,+1],1]]}]&@Flatten[Position[particles, OptionValue[ReturnTwistors][[1]]]][[1]],
   			{}
   		];(*it works only for one twistor to return*)
   		
   		If[
   			n==3&&MatchQ[OptionValue[MassiveParticles],{}]&&MatchQ[OptionValue[Chirality],"Plus"],
   			{twistors, duals} = {duals, twistors}
   		];
  		 
  		 twistors = Map[Part[#, ;;2]&, twistors, {2}];
  		 duals = Map[Part[#, 3;;4]&, duals, {2}];
  
  		If[
   			(NumericalKinematics`Private`CheckKinematics[Sequence@@@twistors] || NumericalKinematics`Private`CheckKinematics[Sequence@@@duals]) && n>3,
   			
   			GenerateKinematics[particles, opts],
   
   			x = particles;
   			x =
    				{
     					Table[If[massive[[i]] == 1, {NSpinorUndottedML[x[[i]]]}, {NSpinorUndottedMV[x[[i]], 1], NSpinorUndottedMV[x[[i]], 2]}], {i, n}],
     					Table[If[massive[[i]] == 1, {NSpinorDottedML[x[[i]]]}, {NSpinorDottedMV[x[[i]], 1], NSpinorDottedMV[x[[i]], 2]}], {i, n}],
     					Table[NMomentum[x[[i]]], {i, n}]
     				};
   
   			massive=
    				Plus@@@(*two massless momenta give a massive one*)
     					FoldPairList[
      						TakeDrop,
      						(TensorProduct[Sequence@@#]&/@Transpose[{Sequence@@@twistors, Sequence@@@duals(*flattens the distinction between massive and massless*)}]),(*the momenta*)
      						massive
      					];
   
   			If[ (*excluding the momentum associated to the InternalTwistor from the set*)
    				\[Not]MatchQ[OptionValue[InternalTwistors], {}],
    				x = Delete[#, y] & /@ x;
    				twistors = Delete[twistors, y];
    				duals = Delete[duals, y];
    				massive = Delete[massive, y];
    			];
   
   			x=Flatten@x;
   
   			twistors = Flatten[twistors, 1](*stop distinguishing between massive and massless spinors*);
   
   			duals = If[Length[#]==2, Reverse[{-1, 1}*#], #]&/@duals;(*little group indices for dotted spinors*)
   			
   			If[
   				OptionValue[FiniteFields],
   				
   				{twistors,duals,massive,ret}=toFF@{twistors,duals,massive,ret}
   			];
   
   			Set@@@(
   				If[OptionValue[Echos], Echo[#], #]&@
   					Thread[
   						Rule[
   							x,
   							Flatten[#, 1]&@{twistors, Flatten[duals, 1], massive}
   						]
   					]
   			);
   
   			If[\[Not]MatchQ[OptionValue[ReturnTwistors], {}], Return[ret]]
   
   		]
  
  	]
  	
 GenerateKinematics::InvMasses="'Masses' has to be assigned to the first elements in the sublists of 'Identicals' and assigned 'Masses' must be identical.";
 GenerateKinematics::NonSquareMasses="Assigned 'Masses' have to be perfect squares.";
 GenerateKinematics::NumericLabels="The labels cannot be numeric.";
 GenerateKinematics::NonMassiveMasses="'Masses' can be only be assigned to 'MassiveParticles'";
 GenerateKinematics::DifferentMasses="The mass of the 'InternalTwistors' is different from the one assigned in 'Masses'";


(* ::Subsection::Closed:: *)
(*End*)


End[]



(* ::Section:: *)
(*Attributes*)


Protect@@
	DeleteCases[
		Names["NumericalKinematics`*"],
		_?(MatchQ[#,Alternatives @@ (ToString /@ {NSpinorDottedML, NSpinorUndottedML, NSpinorDottedMV, NSpinorUndottedMV, NMomentum})]&)
	]

EndPackage[]
