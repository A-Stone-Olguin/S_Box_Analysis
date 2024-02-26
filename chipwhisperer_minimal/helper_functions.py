import chipwhisperer as cw
import os
from math import *
import pickle
import subprocess
import sys
import time
import numpy as np
from tqdm import trange

current_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(current_dir)
from generate_c.generate_c_files import generate_c_files

# Sets up a cw device
def setup_scope_prog(PLATFORM="CWNANO"):
    try:
        if not scope.connectStatus:
            scope.con()
    except NameError:
        scope = cw.scope()

    target_type = cw.targets.SimpleSerial

    try:
        target = cw.target(scope, target_type)
    except:
        print(
            "INFO: Caught exception on reconnecting to target - attempting to reconnect to scope first."
        )
        print(
            "INFO: This is a work-around when USB has died without Python knowing. Ignore errors above this line."
        )
        scope = cw.scope()
        target = cw.target(scope, target_type)

    print("INFO: Found ChipWhisperer😍")


    if "STM" in PLATFORM or PLATFORM == "CWLITEARM" or PLATFORM == "CWNANO":
        prog = cw.programmers.STM32FProgrammer
    else:
        prog = None
    
    time.sleep(0.05)
    scope.default_setup()
    scope.adc.samples = 2500
    
    return (scope, prog, target)

def gather_n_traces(setup_result, N=100):
    scope, prog, target = setup_result

    ktp = cw.ktp.Basic()
    trace_array = []
    textin_array = []

    key, text = ktp.next()
    target.set_key(key)

    for _ in trange(N, desc=f"Gathering {N} traces", leave=False):
        scope.arm()

        target.simpleserial_write('p', text)

        ret = scope.capture()
        if ret:
            print("Target timed out!")
            continue

        response = target.simpleserial_read('r', 16)
        
        trace_array.append(scope.get_last_trace())
        textin_array.append(text)

        key, text = ktp.next()

    return textin_array, trace_array

def tvla_gather_n_traces(setup_result, N=100):
    scope, prog, target = setup_result

    ktp = cw.ktp.TVLATTest()
    ktp.init(N)
    group1 = []
    group2 = []

    fixed_text = bytearray([0xDA,0x39,0xA3,0xEE,0x5E,0x6B,0x4B,0x0D,0x32,0x55,0xBF,0xEF,0x95,0x60,0x18,0x90])
    key, text = ktp.next()

    for _ in trange(2 * N, desc=f"Gathering {N*2} traces", leave=False):
        key, text = ktp.next()
        trace = cw.capture_trace(scope, target, text, key)
        if trace is None:
            print("No trace found!")
            continue 
        if trace.textin == fixed_text:
            group1.append(trace.wave)
        else:
            group2.append(trace.wave)
        key, text = ktp.next()
    return (group1, group2)

def reset_target(scope):
    scope.io.nrst = 'low'
    time.sleep(0.05)
    scope.io.nrst = 'high_z'
    time.sleep(0.05)



# Generates the "Complement" of an s-box
# A complement s-box c[i,j] is defined in comparison to the original s-box s[i,j] as:
#   HW(c[i,j] + s[i,j]) = n
#   HD(c[i,j], s[i,j])  = n
def generate_n_complement(sbox, n):
    bits = int(ceil(log(len(sbox))))
    if n < bits/2:
        print("There is no complement with this HW/HD constraint")
        return 
    ones_vector = [1 for _ in range(bits)]
    # Generate the n complement
    for i in range(bits-n):
        ones_vector[2*i] = 0
    # Cast to an int
    val = 0
    for bit in ones_vector:
        val = (val << 1) | bit
    
    comp_sbox = [0] * len(sbox)
    for i in range(len(sbox)):
        comp_sbox[i] = sbox[i] ^ val
    
    return comp_sbox

# def program_device()

def make_firmware(name_sbox, platform = 'CWNANO', c_target = 'TINYAES128C', scope_t = 'OPENADC', sbox2 = False, aes_mode=None):
    
    pickle_file = f"{current_dir}/generate_c/sboxes_info.pkl"
    with open(pickle_file, "rb") as f:
        sbox_info = pickle.load(f).T.to_dict()
    # Generate c file
    generate_c_files(name_sbox)
    subprocess.run(["make", f"PLATFORM={platform}", f"CRYPTO_TARGET={c_target}", f"SBOX2=0", f"AES_MODE={aes_mode}"],  cwd=f"{current_dir}/firmware/simpleserial-aes")
    return

