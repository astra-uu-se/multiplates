%    The contents of this file are subject to the Mozilla Public License
%    Version  2.0  (the "License"); you may not use this file except in
%    compliance with the License. You may obtain a copy of the License at:
%
%    http://www.mozilla.org/MPL/
%
%    Software  distributed  under  the License is distributed on an "AS
%    IS"  basis,  WITHOUT  WARRANTY  OF  ANY  KIND,  either  express or
%    implied.
%
% Purpose: GENERATE MINIZINC DATAFILES FOR THE EVALUATION OF SIX SIMPLIFIED MODELS
% Author : Ramiz Gindullin, Uppsala University

:- use_module(library(lists)).
:- use_module(library(samsort)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(timeout)).

% You can run it with following commands:
% [generate_multiplate_model], generate_all_dzns(1), halt.
% [generate_multiplate_model], generate_all_dzns(2), halt.
% [generate_multiplate_model].
% [generate_multiplate_model], top, halt.

top :- generate_all_dzns(1).


generate_all_dzns(Flag) :-
    findall([NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber],
            (member(NumTargetPlates,  [2,5,10,15]),
             member(SizeTargetPlates, [2,4,6,8]),
             member(NumSourcePlates,  [2,5,10]),
             no_contradictory_options(NumTargetPlates, SizeTargetPlates, NumSourcePlates),
             setrand(200),
             ExNumber in 0..9, indomain(ExNumber),
            %NumTargetPlates = 4, SizeTargetPlates = 3, NumSourcePlates = 5, ExNumber = 0,
             generate_dzn(Flag, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber)
            ),
            AllInfo),
    write_info(AllInfo, Flag),
    nl.

% In case there is a need for it in the future
no_contradictory_options(_NumTargetPlates, _SizeTargetPlates, _NumSourcePlates) :-
    true.

write_info(AllInfo, Flag) :-
    (Flag = 1 ->
         OutputFileName = 'multiplates_models_evaluation'
    ;
         OutputFileName = 'multiplates_models_evaluation_2_only'
    ),
    atoms_concat([OutputFileName,'.csv'], FileName),
    open(FileName, write, SOut),
    format(SOut, 'Name, NumTargetPlates, SizeTargetPlates, NumSourcePlates, NumCombinations~n', []),
    (foreach([NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber],
             AllInfo),
     param([Flag, SOut])
    do
     write_dzn_file_info(Flag, SOut, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber)
    ),
    close(SOut).
write_dzn_file_info(Flag, SOut, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber) :-
    !,
    generate_file_name(Flag, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber, _DznName, DznFile),
    NumCombinations is NumTargetPlates * SizeTargetPlates,
    format(SOut, '~w,~w,~w,~w,~w~n', [DznFile, NumTargetPlates, SizeTargetPlates, NumSourcePlates, NumCombinations]),
    true.

generate_dzn(Flag, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber) :-
    generate_file_name(Flag, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber, DznName, DznFile),

    write(name(DznName)),nl,
    % 1. Generate and solve model
    generate_dzn(Flag, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber, DznName, AllVars),

    % 2. Write model to the file
    open(DznFile, write, SOut),
    write_dzn(SOut, DznName, AllVars),
    close(SOut),
    
    write(written),nl,
    true.

generate_file_name(Flag, NumTargetPlates, SizeTargetPlates, NumSourcePlates, ExNumber, DznName, DznFile) :-
    (Flag = 1 ->
         PrefixName = 'multiplate_dzn_files/'
    ;
         PrefixName = 'multiplate_dzn_files_2_only/'
    ),
    (foreach(X,  [NumTargetPlates,  SizeTargetPlates,  NumSourcePlates,  ExNumber]),
     foreach(XA, [NumTargetPlatesA, SizeTargetPlatesA, NumSourcePlatesA, ExNumberA])
    do
     to_atom(X, XA)
    ),
    atoms_concat(['multiplate_t',NumTargetPlatesA, '_w', SizeTargetPlatesA, '_s', NumSourcePlatesA, '_', ExNumberA],
                 DznName),
    atom_concat(PrefixName, DznName, PrefixNameFunctor),
    atom_concat(PrefixNameFunctor, '.dzn', DznFile).

generate_dzn(1, NumTargetPlates, SizeTargetPlates, NumSourcePlates, _ExNumber, _DznName, AllVars) :-
    !,
    NumCombinations is NumTargetPlates * SizeTargetPlates,

    length(CombinationVars, NumCombinations),

    (foreach(CombinationVarsLine, CombinationVars), param(NumSourcePlates)
    do
     length(CombinationVarsLine, NumSourcePlates)
    ),

    % ctr 1 - each row at least 1
    (foreach(CombinationVarsLine, CombinationVars), param(NumSourcePlates)
    do
     sum(CombinationVarsLine, #>=, 1)
    ),
	
    % ctr 2 - each column at least 1 IIF NumCombinations >= NumSourcePlates
    (for(I, 1, NumSourcePlates), param(CombinationVars)
    do
     post_ctrs_on_each_source_plate(I, CombinationVars)
    ),
	
    % ctr 3 - total sum at least random number
    TotalSumMin is max(NumCombinations, NumSourcePlates),
    TotalSumMax is 2 * NumCombinations * NumSourcePlates div 3, % up to 66% is set to true
    random(TotalSumMin, TotalSumMax, TotalSum),
    flatten(CombinationVars, CombinationVarsFlat),
    sum(CombinationVarsFlat, #>=, TotalSum),

    % add randomness to the solution
    TotalProb is NumCombinations * NumSourcePlates,
    (foreach(Var, CombinationVarsFlat), param([TotalSum, TotalProb])
    do
     (maybe(TotalSum, TotalProb) -> Var is 1 ; true)
    ), 

    label_plate_variables(CombinationVarsFlat),
    
    AllVars = [NumTargetPlates, SizeTargetPlates, NumSourcePlates, NumCombinations, CombinationVars],
    !.
% same as before but in this case we assume that we only test pairs of materials.
generate_dzn(_, NumTargetPlates, SizeTargetPlates, NumSourcePlates, _ExNumber, _DznName, AllVars) :-
    NumCombinations is NumTargetPlates * SizeTargetPlates,

    length(CombinationVars, NumCombinations),

    (foreach(CombinationVarsLine, CombinationVars), param(NumSourcePlates)
    do
     length(CombinationVarsLine, NumSourcePlates)
    ),

    repeat,
    % step 1 - each combination uses 2 materials, each material is provided by a source plate
    length(CombinationSourcesPairs, NumCombinations),
    (foreach([CombinationSource1, CombinationSource2], CombinationSourcesPairs), param(NumSourcePlates)
    do
     random(1, NumSourcePlates, CombinationSource1),
     random(1, NumSourcePlates, CombinationSource2)
    ),

    % step 2 - make sure that each source plate is used at least once
    (for(I, 1, NumSourcePlates), param(CombinationSourcePairs)
    do
     test_source_plate_in_combinations(CombinationSourcePairs, I)
    ),

    % step 3 - convert to Booleans
    (foreach([CombinationSource1, CombinationSource2], CombinationSourcesPairs),
     foreach(CombinationVarsLine, CombinationVars),
     param(NumSourcePlates)
    do
     (for(I, 1, NumSourcePlates),
      foreach(Var, CombinationVarsLine),
      param([CombinationSource1, CombinationSource2])
     do
      (I = CombinationSource1 -> Var is 1 ;
       I = CombinationSource2 -> Var is 1 ;
                                 Var is 0 )
     )
    ),
    AllVars = [NumTargetPlates, SizeTargetPlates, NumSourcePlates, NumCombinations, CombinationVars],
    !.

post_ctrs_on_each_source_plate(I, CombinationVars) :-
    (foreach(CombinationVarsLine, CombinationVars),
     foreach(CombinationVarI,     CombinationVarsI),
     param(I)
    do
     CombinationVarI in 0..1,
     nth1(I, CombinationVarsLine, CombinationVarI)
    ),
    sum(CombinationVarsI, #>=, 1).

test_source_plate_in_combinations([[I, _]|_], I) :- !.
test_source_plate_in_combinations([[_, I]|_], I) :- !.
test_source_plate_in_combinations([_|R], I) :- !,
    test_source_plate_in_combinations(R, I).

label_plate_variables(Vars) :-
    labeling([], Vars).

flatten(List, FlatList) :-
    flatten(List, FlatList, []).

flatten([], List1, List2):- !, List1 = List2.
flatten([List|R], X, Z) :-
    is_list(List), !,
    flatten(List, X, Y),
    flatten(R, Y, Z).
flatten([E|R], [E|X], Z) :-
    flatten(R, X, Z).

write_dzn(SOut, _DznName, AllVars) :-
    AllVars = [NumTargetPlates, SizeTargetPlates, NumSourcePlates, NumCombinations, CombinationVars],

    format(SOut, 'num_source_plates = ~w;~n~n',  [NumSourcePlates]),
    format(SOut, 'num_target_plates = ~w;~n',    [NumTargetPlates]),
    format(SOut, 'size_target_plates = ~w;~n~n', [SizeTargetPlates]),
    format(SOut, 'num_combinations = ~w;~n~n', [NumCombinations]),

    format(SOut, 'combinations_source_plates =~narray2d(1..num_combinations, 1..num_source_plates,~n[~n', []),

    (foreach(CombinationVarsLine, CombinationVars), param(SOut)
    do
     (foreach(Var, CombinationVarsLine), param(SOut)
     do
      (Var = 0 ->
           format(SOut, ' false,', [])
      ;
       Var = 1 ->
           format(SOut, '  true,', [])
      ;
           write(error_var(Var)),nl,halt
      )
     ),
     format(SOut, '~n', [])
    ),
    
    format(SOut, ']);', []),
    true.


% following predicates are taken from https://github.com/cquimper/MapSeekerScheduling/blob/main/utility.pl :
atoms_concat([Atom|R], Final) :-
    to_atom(Atom, AtomC),
    atoms_concat(R, AtomC, Final).

atoms_concat([], Final, Final) :- !.
atoms_concat([Atom|R], Prev, Final) :-
    to_atom(Atom, AtomC),
    atom_concat(Prev, AtomC, Cur),
    atoms_concat(R, Cur, Final).

% convert to atoms
to_atom(X,X):-
        atom(X),
        !.
to_atom(X,Y):-
        number(X),
        number_codes(X,L),
        atom_codes(Y,L).

write_list(List) :-
    (foreach(X, List) do write(X), nl).