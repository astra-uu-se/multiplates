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
% Purpose: MODEL MULTIPLATE II-A
% Author : Ramiz Gindullin, Uppsala University

include "globals.mzn";

int: num_source_plates;

int: num_target_plates;
int: size_target_plates;

int: num_combinations;

array[1..num_combinations, 1..num_source_plates] of bool: combinations_source_plates;
set of 1..num_source_plates: used_source_plates =
       {sp | sp in 1..num_source_plates
             where exists([combinations_source_plates[cb,sp] | cb in 1..num_combinations])};
      
% Constraint model starts here
array[1..num_combinations] of var 1..num_target_plates: target_plate_placement::no_output;

constraint (size_target_plates * num_target_plates = num_combinations) ->
           global_cardinality(target_plate_placement,
                              [tp                 | tp in  1..num_target_plates],
                              [size_target_plates | tp in  1..num_target_plates]);
constraint (size_target_plates * num_target_plates > num_combinations) ->
           global_cardinality(target_plate_placement,
                              [tp                 | tp in  1..num_target_plates],
                              [0                  | tp in  1..num_target_plates],
                              [size_target_plates | tp in  1..num_target_plates]);

%array[1..num_target_plates] of var set of 1..num_source_plates: source_target_connections;
/*constraint forall(tp in 1..num_target_plates)
                 (source_target_connections[tp] =
                  {sp | sp in used_source_plates
                   where exists([combinations_source_plates[cb,sp] /\ (target_plate_placement[cb] = tp) | cb in 1..num_combinations])}
                 );*/
array[1..num_source_plates, 1..num_target_plates] of var bool: source_target_connections;
constraint forall(sp in used_source_plates, tp in 1..num_target_plates)
                 (source_target_connections[sp,tp] =
                  exists([combinations_source_plates[cb,sp] /\ (target_plate_placement[cb] = tp) | cb in 1..num_combinations])
                 );

array[1..num_target_plates] of var 0..num_source_plates: num_connections_per_target_plate;
constraint num_connections_per_target_plate = [sum(sp in 1..num_source_plates)
                                                  (source_target_connections[sp,tp])
                                               | tp in 1..num_target_plates];
array[1..num_target_plates - 1] of var 0..num_source_plates: connection_steps;
constraint forall(tp in 1..num_target_plates - 1)
                 ((connection_steps[tp] >= 1)
                  ->
                  (source_target_connections[connection_steps[tp],tp])
                 );
constraint forall(tp in 1..num_target_plates - 1)
                 ((connection_steps[tp] >= 1)
                  ->
                  (source_target_connections[connection_steps[tp],tp + 1])
                 );
constraint forall(tp in 1..num_target_plates - 1)
           (forall(sp in 1..num_source_plates)(source_target_connections[sp,tp] * source_target_connections[sp,tp + 1] = 0)
            ->
            connection_steps[tp] = 0
           );
constraint forall(tp in 1..num_target_plates - 2
                  where connection_steps[tp] >= 1
                        /\ num_connections_per_target_plate[tp + 1] > 1)
                 (connection_steps[tp + 1] != connection_steps[tp]);

var 0..num_target_plates - 1: count_potential_connections::no_output;
constraint count_potential_connections = sum(tp in 1..num_target_plates - 1)(connection_steps[tp] >= 1);

solve minimize sum(source_target_connections) - count_potential_connections;

%COMMON OUTPUT
output [show(target_plate_placement)];
output ["\n"];
output [show(source_target_connections)];
output ["\n"];
output ["Nb of arcs: \(sum(num_connections_per_target_plate) - count_potential_connections)\n"];
output ["Connections sequence: \(connection_steps)\n"];