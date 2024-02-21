import numpy as np
from tqdm import trange

HW = [bin(n).count("1") for n in range(0,256)]

def aes_internal(sbox, input_data, key, sboxcomp=None):
    if sboxcomp:
        return sbox[sboxcomp[input_data ^ key]]
    else:
        return sbox[input_data ^ key]

def calculate_diffs(sbox, textin_array, trace_array, guess, byteindex=0, bitnum=0):
    """Perform simple DPA on two traces, using textin and trace arrays"""
    one_list = []
    zero_list = []
    numtraces = np.shape(trace_array)[0]

    for trace_index in range(numtraces):
        hypothetical_leakage = aes_internal(sbox, guess, textin_array[trace_index][byteindex])

        #Mask off the requested bit
        if hypothetical_leakage & (1<<bitnum):
            one_list.append(trace_array[trace_index])
        else:
            zero_list.append(trace_array[trace_index])

    one_avg = np.asarray(one_list).mean(axis=0)
    zero_avg = np.asarray(zero_list).mean(axis=0)
    return abs(one_avg - zero_avg)


def dpa_run(sbox, textin_array, trace_array):
    key_guess = []
    # known_key = [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c]

    # full_diffs_list = []
    for subkey in trange(0, 16, desc="Attacking Subkey", leave=False):
        max_diffs = [0]*256
        full_diffs = [0]*256
        for guess in range(0, 256):
            full_diff_trace = calculate_diffs(sbox, textin_array, trace_array, guess, subkey)
            max_diffs[guess] = np.max(full_diff_trace)
            full_diffs[guess] = full_diff_trace
            
        #Get argument sort, as each index is the actual key guess.
        # full_diffs_list.append(full_diffs[:])
        sorted_args = np.argsort(max_diffs)[::-1]
        
        #Keep most likely
        key_guess.append(sorted_args[0])
    return key_guess
    
def cpa_run(sbox, textin_array, trace_array):
    # Dr. O'Flynn's CPA stat functions
    def mean(X):
        return np.sum(X, axis=0)/len(X)
    def std_dev(X, X_bar):
        return np.sqrt(np.sum((X-X_bar)**2, axis=0))
    def cov(X, X_bar, Y, Y_bar):
        return np.sum((X-X_bar)*(Y-Y_bar), axis=0)


    t_bar = mean(trace_array) 
    o_t = std_dev(trace_array, t_bar)

    # cparefs = [0] * 16
    key_guess = [0] * 16
    for bnum in trange(0, 16, desc="Attacking subkey", leave=False):
        maxcpa = [0] * 256
        for kguess in range(0, 256):
            hws = np.array([[HW[aes_internal(sbox, textin[bnum], kguess)] for textin in textin_array]]).transpose()
            hws_bar = mean(hws)
            o_hws = std_dev(hws, hws_bar)
            covariance = cov(trace_array, t_bar, hws, hws_bar)
            correlation = covariance/(o_t*o_hws)
            maxcpa[kguess] = max(abs(correlation))
        # cparefs[bnum] = max(maxcpa)
        key_guess[bnum] = np.argmax(maxcpa)
    return key_guess

def tvla_run():
    #TODO Finish this
    return
