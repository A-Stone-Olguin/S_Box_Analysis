from tqdm import trange
from chipwhisperer_minimal.helper_functions import *
from chipwhisperer_minimal.sca_attacks import *

METRIC_HIGH = 250
TOTAL_RUNS = 10
    

def num_traces(sbox_name, sbox, platform = "CWNANO", attack_method="DPA", N_lo=0, N_hi = 2*METRIC_HIGH, setup_result = None):
    # If we found our threshold, return our number of traces
    # Since we halve our result each time, this will terminate in `log_2 METRIC_HIGH` iterations
    if abs(N_hi - N_lo) <= 1:
        if not setup_result:
            print("Invalid bounds given! Need to make sure that METRIC_HIGH >1")
            return -1
        else:
            setup_result[0].dis()
            return N_hi
    known_key = [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c]
    
    num_correct_breaks = 0
    hexname = f"simpleserial-aes-{platform}.hex"

    if not setup_result:
        # Make the firmware for the sbox and setup the device
        make_firmware(sbox_name, platform, c_target = 'TINYAES128C', scope_t = 'OPENADC', sbox2 = False)
        setup_result = setup_scope_prog(platform)

        print("Programming target")
        cw.program_target(setup_result[0], setup_result[1], f"{current_dir}/firmware/simpleserial-aes/{hexname}")
        # scope.dis()

    bisection = (N_lo + N_hi)//2

    for _ in trange(0,TOTAL_RUNS, desc=f"Num traces for {sbox_name} with N of {bisection} ", leave=False):
        textin_array, trace_array = gather_n_traces(setup_result, N=bisection, hexname=hexname)
        
        if attack_method.upper() == "DPA":
            key_guess = dpa_run(sbox, textin_array, trace_array)
        elif attack_method.upper() == "CPA":
            key_guess = cpa_run(sbox, textin_array, trace_array)
        else:
            print("Invalid attack_method provided. Please use either 'CPA' or 'DPA'.")
            return 
    
        if known_key == key_guess:
            num_correct_breaks+=1

    # If we broke in 90% of the tests, we consider this a good break, check for a lower break
    if num_correct_breaks >= .9*TOTAL_RUNS:
        return num_traces(sbox_name, sbox, platform, attack_method, N_lo=N_lo, N_hi=bisection, setup_result=setup_result)
    # Our original upper bound wasn't high enough
    elif bisection == METRIC_HIGH/2:
        print(F"No break with upper bound of {METRIC_HIGH}/2!")
        return -1
    # We didn't get a break, see if there is one with more traces
    else:
        return num_traces(sbox_name, sbox, platform, attack_method, N_lo=bisection, N_hi=N_hi, setup_result=setup_result)
    
# Function for metric of TVLA
def tvla():
    return