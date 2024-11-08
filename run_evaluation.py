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
# Purpose: EXECUTE THE EVALUATION AND RECORD ITS RESULTS
# Author : Ramiz Gindullin, Uppsala University

import os.path
import subprocess

# functions:
def run_cmd(minizinc_path, project_path, timeout_set, solver_config_name, model_file_name, data_file):
    solver_config = project_path + solver_config_name + '.mpc'
    model_file = project_path + 'combo_prototype_simplified_' + model_file_name + '.mzn'
    cmd = minizinc_path + ' --param-file-no-push ' + solver_config + ' ' + model_file + ' ' + data_file
    process = subprocess.Popen([cmd], shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    while True:
        try:
            retval = process.wait(timeout = timeout_set + 10)
            #retval = process.wait(timeout = 2)
            output, _ = process.communicate()
            output = output.decode('utf-8').strip()
            process.kill()
            break
        except subprocess.TimeoutExpired:
            process.kill()
            print(solver_config_name + ', ' + model_file_name + ', ' + data_file + ': process killed. Restarting')
    return output

def extract_data(data_text, timeout):
    #print(data_text) # disable - for testing purposes only
    data_text_lines = data_text.rsplit('\n')
    is_timeout = False
    is_time = False
    is_total_time = False
    # default values
    solution_time = '-1'
    solution = '-1'
    total_time = str(timeout)
    # see if default values can be overwritten
    for line in data_text_lines:
        if line == '% Time limit exceeded!':
            is_timeout = True
        elif line == '=====UNKNOWN=====':
            is_timeout = True
        elif line[:16] == '% time elapsed: ':
            if is_time:
                total_time = line[16:-2]
                is_total_time = True
            else:
                solution_time = line[16:-2]
                is_time = True
        elif line[:12] == 'Nb of arcs: ':
            solution = line[12:]
    if not is_timeout and not is_total_time:
        is_timeout = True
    if solution == '-1':
        is_timeout = True
        solution_time = '-1'
    if is_timeout:
        timeout_str = 'true'
    else:
        timeout_str = 'false'
    return solution_time + ',' + timeout_str + ',' + total_time + ',' + solution


def run_config(config):
    # list relevant paths:
    minizinc_path = '/Applications/MiniZincIDE.app/Contents/Resources/minizinc'
    project_path = '/Users/ramgi410/Documents-Local/eclipse_workspace/generate_multi_plates_examples/'
    data_file_list = project_path + 'multiplates_models_evaluation.csv'
    timeout_set = 300 # set accordingly with configs

    # relevant lists:
    model_list = ['I_A',
                  'I_B',
                  'II_A',
                  'II_B',
                  'III_A',
                  'III_B']
    model_list_test = ['I_A']

    # main program
    os.path.exists(data_file_list)
    lines = open(data_file_list,'r').readlines()
    f = open(project_path + 'multiplates_models_evaluation_complete_' + config + '.csv', 'w')
    log = open(project_path + 'multiplates_models_evaluation_' + config + '.log', 'w')

    f.write('Solver,Model,' + lines[0][:-1] + ',SolutionTime,IsTimeout,TotalTime, NbSwaps\n')
    log.write(config + ':\n')
    
    for line in lines[1:]:
        for model in model_list:
            cmd_to_str = run_cmd(minizinc_path, project_path, timeout_set,
                                 config, model, line.rsplit(',')[0])
            log.write(cmd_to_str + '\n\n')
            str_to_write = extract_data(cmd_to_str, timeout_set)
            log.write(config + ',' + model + ',' + line.rsplit(',')[0] + ': ' +
                      str_to_write + '\n\n\n\n\n')
            print(line.rsplit(',')[0] + ': ' + str_to_write + ',' + model)
            f.write(config + ',' + model + ','+ line[:-1] + ',' + str_to_write + '\n')

    f.close()
    log.close()
    print('finished')
        
