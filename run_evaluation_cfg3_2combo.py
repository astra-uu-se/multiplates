#    The contents of this file are subject to the Mozilla Public License
#    Version  2.0  (the "License"); you may not use this file except in
#    compliance with the License. You may obtain a copy of the License at:
#
#    http://www.mozilla.org/MPL/
#
#    Software  distributed  under  the License is distributed on an "AS
#    IS"  basis,  WITHOUT  WARRANTY  OF  ANY  KIND,  either  express or
#    implied.
#
# Purpose: EXECUTE THE EVALUATION AND RECORD ITS RESULTS (WITH A SELECTED SOLVER)
# Author : Ramiz Gindullin, Uppsala University

import run_evaluation

run_evaluation.run_config('gurobi_config', 'multiplates_models_evaluation_2_only.csv', 'multiplates_models_evaluation_two_only_')
