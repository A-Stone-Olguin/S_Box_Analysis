from sage.all import *
from sage.crypto.sbox import *
from sage.crypto.sboxes import sboxes
import numpy as np
import pickle
import pandas as pd
import os
import sys

current_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(current_dir)

# Modified from https://github.com/abrari/block-cipher-testing
# Works only on square s-boxes
def sbox_bic(sbox):

    maxCorr = 0
    m = len(sbox)
    for i in range(m):
        ei = 2**i 
        for j in range(m):
            for k in range(m):
                if j != k:
                    avalanche_vec_j = np.zeros(2**m, dtype=float)
                    avalanche_vec_k = np.zeros(2**m, dtype=float)

                    # Each possible input
                    for X in range(2**m):
                        ej = 2**j 
                        ek = 2**k 

                        dei = sbox[X] ^ sbox[X ^ ei]
                        dej = (dei & ej) >> j
                        dek = (dei & ek) >> k 

                        avalanche_vec_j[X] = dej 
                        avalanche_vec_k[X] = dek
                    corr = abs(np.corrcoef(avalanche_vec_j, avalanche_vec_k)[0,1])
                
                    if maxCorr < corr:
                        maxCorr = corr
    return maxCorr

# Modified version to get the average SAC of an sbox from:
#       https://github.com/abrari/block-cipher-testing
def avg_sac(sbox):
    m = len(sbox)
    sac_mat = np.array([np.zeros(m, dtype=float) for _ in range(m)])

    for i in range(m):
        ei = 2**i 
        for j in range(m):
            ej = 2**j 
            for X in range(2**m):
                dei = sbox[X] ^ sbox[X ^ ei]
                sac_mat[i][j] += (dei & ej) >> j 
    
    output_len = 2**m
    for i in range(m):
        for j in range(m):
            sac_mat[i][j] /= output_len

    sum = 0 
    for i in range(m):
        sum += np.sum(sac_mat[i])
    avg = sum / (m**2)
    return avg 

# Adding the 8 sboxes from the paper
def sboxes_8():
    sboxes = {}
    with open("./chipwhisperer_minimal/generate_c/sboxes_8.txt", "r") as f:
        for line in f.readlines():
            values = line.split(", ")
            values[-1] = values[-1][:4]  # Remove trailing \n 
            sboxes[f"S{values[0]}"] = [int(i, 16) for i in values[1:]]
    return sboxes

def create_row(sbox):
    row = {}
    row["box"] = [num for num in sbox] 
    row["inverse"] = [num for num in sbox.inverse()] if sbox.is_permutation() else []
    row["nonlinearity"] = int(sbox.nonlinearity())
    row["linear_probability"] = float(sbox.maximal_linear_bias_relative())
    row["differential_probability"] = float(sbox.maximal_difference_probability())
    row["boomerang_uniformity"] = int(sbox.boomerang_uniformity()) if sbox.is_permutation() else -1
    row["diff_branch"] = int(sbox.differential_branch_number())
    row["linear_branch"] = int(sbox.linear_branch_number())
    row["linearity"] = int(sbox.linearity())
    row["bic"] = sbox_bic(sbox) 
    row["sac"] = avg_sac(sbox) 
    return row 

def main():
    pickle_filename = f"{current_dir}/chipwhisperer_minimal/generate_c/sboxes_info.pkl"
    df_dict = {}

    for name, sbox in sboxes.items():
        if len(sbox) == 8:
            print("Calculating info:", name)
            df_dict[name] = create_row(sbox)

    # Add the 8 sboxes
    sb_8 = sboxes_8()
    print("doing sboxes_8():")
    for name in sb_8.keys():
        sbox = SBox(*sb_8[name])
        print("Calculating info:", name)
        df_dict[name] = create_row(sbox)

    print("\nFinished calculating sbox info!\n")
    df = pd.DataFrame.from_dict(df_dict).T
    with open(pickle_filename, "wb") as f:     
        pickle.dump(df, f)
    return

if __name__ == "__main__":
    main()