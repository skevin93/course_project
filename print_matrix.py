#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np

def read_file(file_name:str) -> dict:
    f_obj = open(file_name,"r")

    line = f_obj.readline()
    tmp_dict = {}

    while line:
        line = line.rstrip()
        line = line.split(":")
        if len(line) == 2:
            curr_key = line[0].strip()
            tmp_dict[curr_key] = []
            
            line = line[1].split()
            m = int(line[0])
            if len(line) == 2:
                m = int(line[1])

            for _ in range(m):
                line = f_obj.readline().split()
                tmp_dict[curr_key].append(list(map(float, line)))

        line = f_obj.readline()
    
    return tmp_dict

def print_info(file_name):
    output = read_file(file_name)

    f, axs = plt.subplots(1,2,figsize=(10,10))

    for i, key in enumerate(output.keys()):
        array = output[key]

        plt.subplot(1,2,i+1)
        plt.title(key)
        plt.matshow(array, fignum=False)#, cmap="Blues")
        plt.colorbar()


    plt.show()
print(__name__)

if __name__ == "__main__":
    print_info("output.out")
