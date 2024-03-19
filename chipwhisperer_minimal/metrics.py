from tqdm import trange
from chipwhisperer_minimal.helper_functions import *
from chipwhisperer_minimal.sca_attacks import *
import time

TOTAL_RUNS = 30
TTEST_THRESHOLD = 4.5

# metric_params = {
#     "METRIC_HIGH" : int,
#     "attack_function" : function,
#     "TOTAL_RUNS" : int,
# }

# Create DPA and CPA structs
dpa_metrics = {
    "METRIC_HIGH" : 4000,
    "attack_function" : dpa_run,
    "TOTAL_RUNS" : 10,
}

cpa_metrics = {
    "METRIC_HIGH" : 300,
    "attack_function" : cpa_run,
    "TOTAL_RUNS" : 30,
}

tvla_metrics = {
    "METRIC_HIGH" : 500,
    "attack_function" : tvla_run,
    "TOTAL_RUNS" : 30,
}
    

def num_traces(sbox_name, sbox, platform = "CWNANO", metrics = dpa_metrics):
    # Program the CW device
    make_firmware(sbox_name, platform, c_target = 'TINYAES128C', scope_t = 'OPENADC', sbox2 = False, aes_mode="ECB")

    #Pause for a second to generate the firmware
    time.sleep(1)
    setup_result = setup_scope_prog(platform)

    print("Programming target")
    hexname = f"simpleserial-aes-{platform}.hex"
    cw.program_target(setup_result[0], setup_result[1], f"{current_dir}/firmware/simpleserial-aes/{hexname}")


    # If no metric high given, define it
    hi = metrics["METRIC_HIGH"]
    first_hi = hi
    lo = 0
    known_key = [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c]

    while (abs(hi - lo) > 1):
        midpoint = (hi + lo)//2 
        counter = 0
        for i in trange(0, metrics["TOTAL_RUNS"], desc=f"Calculating {sbox_name} using {midpoint} traces", leave=False):
            textin_array, trace_array = gather_n_traces(setup_result, N=midpoint)

            key_guess = metrics["attack_function"](sbox, textin_array, trace_array)
            if key_guess == known_key:
                counter += 1
        # print(counter)
        if counter >= .9 * metrics["TOTAL_RUNS"]:
            hi = midpoint
        elif hi == first_hi:
            print(F"No break with upper bound of {first_hi}!")
            setup_result[0].dis()
            return -1
        else:
            lo = midpoint
        # print(f"After: lo, hi = {lo}, {hi}")

    # Disconnects the CW device
    setup_result[0].dis()
    return hi


# Function for metric of TVLA
def tvla(sbox_name, platform = "CWNANO", metrics = tvla_metrics, aes_mode=None):
    # Make the firmware for the sbox and setup the device
    make_firmware(sbox_name, platform, c_target = 'TINYAES128C', scope_t = 'OPENADC', sbox2 = False, aes_mode=aes_mode)

    #Pause for a second to generate the firmware
    time.sleep(1)
    setup_result = setup_scope_prog(platform)

    print("Programming target")
    hexname = f"simpleserial-aes-{platform}.hex"
    cw.program_target(setup_result[0], setup_result[1], f"{current_dir}/firmware/simpleserial-aes/{hexname}")

    percentage_leaks = [] 

    for i in trange(0, TOTAL_RUNS, desc=f"TVLA on {sbox_name}", leave=False):
        fixed_t, random_t = tvla_gather_n_traces(setup_result, N=metrics["METRIC_HIGH"])
        percentage_leak = metrics["attack_function"](fixed_t, random_t, TTEST_THRESHOLD)

        percentage_leaks.append(percentage_leak)

    setup_result[0].dis()
    # Return the average percent leaks
    return np.mean(percentage_leaks)

## current_result = sum of 60 values/60 = sum([0,...,0, r_1, ..., r_30])/60

## current_result * 60 = sum of 30 actual values = sum([r_1, .., r_30])

# current_result * 2 * 30 = sum([r_1, .., r_30])
# current_result * 2 = sum([r_1, .., r_30])/30

## actual_result = current_result * 2 = sum of 30 acutal values/30