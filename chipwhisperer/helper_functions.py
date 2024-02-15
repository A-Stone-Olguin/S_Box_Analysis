import chipwhisperer as cw
import os
from math import *
import pickle
import subprocess
import sys



current_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(current_dir)
from generate_c.generate_c_files import generate_c_files

def gather_n_traces(N=100, hexname="flick_em.hex"):
    current_dir = os.path.dirname(os.path.realpath(__file__))

    scope.default_setup()

    cw.program_target(scope, prog, f"{current_dir}/firmware/simpleserial-aes/{hexname}")

    trace_array = []
    for _ in range(N):
        scope.arm()

        ret = scope.capture()
        if ret:
            print("Target timed out!")
            continue

        trace_array.append(scope.get_last_trace())


    scope.dis()
    return trace_array

def setup(PLATFORM="CWNANO"):
    try:
        if not scope.connectStatus:
            scope.con()
    except NameError:
        scope = cw.scope()

    target_type = cw.targets.SimpleSerial

    try:
        cw.target(scope, target_type)
    except:
        print(
            "INFO: Caught exception on reconnecting to target - attempting to reconnect to scope first."
        )
        print(
            "INFO: This is a work-around when USB has died without Python knowing. Ignore errors above this line."
        )
        scope = cw.scope()
        cw.target(scope, target_type)

    if "STM" in PLATFORM or PLATFORM == "CWLITEARM" or PLATFORM == "CWNANO":
        prog = cw.programmers.STM32FProgrammer
    else:
        prog = None

    
    print("INFO: Found ChipWhisperer😍")
    return

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

def make_firmware(name_sbox, platform = 'CWNANO', c_target = 'TINYAES128C', scope_t = 'OPENADC', sbox2 = False):
    
    pickle_file = f"{current_dir}/generate_c/sboxes_info.pkl"
    with open(pickle_file, "rb") as f:
        sbox_info = pickle.load(f).T.to_dict()
    # Generate c file
    generate_c_files(name_sbox)

    subprocess.Popen(["make", f"PLATFORM={platform}", f"CRYPTO_TARGET={c_target}", f"SBOX2=0"],  cwd=f"{current_dir}/firmware/simpleserial-aes")
    return



def aes_internal(sbox, input_data, key, sboxcomp=None):
    if sboxcomp:
        return sbox[sboxcomp[input_data ^ key]]
    else:
        return sbox[input_data ^ key]


        
    
    


