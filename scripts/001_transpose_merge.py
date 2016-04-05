#!/usr/bin/env Python

import os.path

path_to_data_directory = "../data_default_single"

# --------------------------------------------------------------
# Description:
# Transpose and merge individual mutant data files into a single
# file.
# --------------------------------------------------------------

# --------------------------------------------------------------
# Calling sequence:
# python 001_transpose_merge.py
# 
# Note: Working directory for the python call has to be the
#       './scripts/' directory.
# --------------------------------------------------------------

# All files could also include invisible files starting with
# a '.' in the filename.
data_directory_files = os.listdir(path_to_data_directory)

# Only count files starting with '0000' (:identifies the raw mutant
# data files).
number_of_files = len([i for i in data_directory_files if i.startswith("0000")])

combined_file  = "../data_processed/merge_single_" + str(number_of_files)
combined_file += ".dat"

combined_file = open(combined_file, 'w')

# Add header.
combined_file.write("Mutant F1 F2 F3 F4 F5 F6 F7 F8 F9 F10 F11 F12\n")

# Process files.
for d in os.listdir("../data_default_single/"):
    if d.startswith("0000"):
        file_name = os.path.splitext(d)[0]
        mutant_name = file_name.split('-')[1]

        tmp_file = open('../data_default_single/' + d, 'r')
        tmp_data = tmp_file.readlines()

        # Transpose column data.
        tmp_data = ' '.join(tmp_data)
        tmp_data = mutant_name + ' ' + tmp_data
        tmp_data = tmp_data.split('\n')
        tmp_data = ' '.join(tmp_data)
        tmp_data = tmp_data + '\n'
        
        combined_file.write(tmp_data)

combined_file.close()
