# Multiplates
This repository contains materials to reproduce the evaluation section of the submitted paper "Minimising Source-Plate Swaps for Robotised Compound Dispensing in Microplates"

The materials include:

1. Script to generate synthetic datasets
2. Two synthetic datasets, `multiplate_dzn_files.zip` and `multiplate_dzn_files_2_only.zip`:

	1. `multiplate_dzn_files.zip` is not used for the article. The number of connected source plates to a given compound combination is unrestricted
	2. `multiplate_dzn_files_2_only.zip` is used for the evaluation in the article. The number of connected source plates to a given compound combination is limited by a maximum of 2


# Generating the dataset for the evaluation
The generated dataset is contained in files  `multiplate_dzn_files.zip` and `multiplate_dzn_files_2_only.zip`. Uncompress them and move to the next step of the evaluation.

If you wish to generate datasets anew then run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following commands:

1. `| ?- [generate_multiplate_model], generate_all_dzns(1), halt.` to create the dataset `multiplate_dzn_files`
2. `| ?- [generate_multiplate_model], generate_all_dzns(2), halt.` to create the dataset `multiplate_dzn_files_2_only`

Create the folder `multiplate_dzn_files` and/or `multiplate_dzn_files_2_only` if not created prior.
