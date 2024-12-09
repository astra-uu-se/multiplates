# Multiplates
This repository contains materials to reproduce the evaluation section of the submitted paper "Minimising Source-Plate Swaps for Robotised Compound Dispensing in Microplates"

The materials include:

1. Script to generate synthetic datasets
2. Two synthetic datasets, `multiplate_dzn_files.zip` and `multiplate_dzn_files_2_only.zip`:

	1. `multiplate_dzn_files.zip` is not used for the article. The number of connected source plates to a given compound combination is unrestricted
	2. `multiplate_dzn_files_2_only.zip` is used for the evaluation in the article. The number of connected source plates to a given compound combination is limited by a maximum of 2
3. Scripts and configuration files to run computational experiments
4. Dataframes, logs and a Python Notebook with aggregated results


# Data generation
The generated dataset is contained in files  `multiplate_dzn_files.zip` and `multiplates_models_evaluation.csv` (Dataset 1), `multiplate_dzn_files_2_only.zip` and `multiplates_models_evaluation_2_only.csv` (Dataset 2). Uncompress them and move to the next step of the evaluation.

If you wish to generate datasets anew then:

1. Open a bash shell and position yourself in the directory containing the file `generate_multiplate_model.pl`
2. Run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following commands:

	1. `| ?- [generate_multiplate_model], generate_all_dzns(1), halt.` to generate Dataset 1
	2. `| ?- [generate_multiplate_model], generate_all_dzns(2), halt.` to generate Dataset 2

Each dataset generation takes a few minutes.

Create the folder `multiplate_dzn_files` and/or `multiplate_dzn_files_2_only` if not created prior.


# Executing computational experiments

To execute the computational experiments first install and configure Python (version 3.7.0 or later), MiniZinc (version 2.8.0 or later) and Gurobi (version 11.0.3 or later). Make sure that the versions of installed solvers, Chuffed, CP-SAT and Gurobi, are matched the versions written in the configuration files `chuffed_config.mpc`, `cpsat_config.mpc` and `gurobi_config.mpc` respectively.

Then:

1. Open a bash shell and position yourself in the directory containing files `run_evaluation.py`, `run_evaluation_cfg1.py`, `run_evaluation_cfg2.py`, `run_evaluation_cfg3.py`, `run_evaluation_cfg1_2combo.py`, `run_evaluation_cfg2_2combo.py`, `run_evaluation_cfg3_2combo.py`
2. Execute commands, sequentially or in parallel:
	1. `time python3 run_evaluation_cfg1.py` to test Dataset 1 on Chuffed (1 thread) (NOT REQUIRED FOR THE ARTICLE)
	2. `time python3 run_evaluation_cfg2.py` to test Dataset 1 on CP-SAT (4 threads) (NOT REQUIRED FOR THE ARTICLE)
	3. `time python3 run_evaluation_cfg3.py` to test Dataset 1 on Gurobi (4 threads) (NOT REQUIRED FOR THE ARTICLE)
	4. `time python3 run_evaluation_cfg1_2combo.py` to test Dataset 2 on Chuffed (1 thread)
	5. `time python3 run_evaluation_cfg2_2combo.py` to test Dataset 2 on CP-SAT (4 threads)
	6. `time python3 run_evaluation_cfg3_2combo.py` to test Dataset 2 on Gurobi (4 threads)

# Aggregating the results

Execution of computational experiments will result in the creation of files `multiplates_models_evaluation_complete_chuffed_config.csv`, `multiplates_models_evaluation_complete_cpsat_config.csv`, `multiplates_models_evaluation_complete_gurobi_config.csv`,  `multiplates_models_evaluation_two_only_chuffed_config.csv`, `multiplates_models_evaluation_two_only_cpsat_config.csv`, `multiplates_models_evaluation_two_only_gurobi_config.csv`.

Open the Python Notebook `evaluate_multiplates.ipynb` in, either, Jupyter or Google Collab. Rerun or modify the code if needed.
