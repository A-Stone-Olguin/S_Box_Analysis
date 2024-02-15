import numpy as np
import pickle
import pandas as pd
import math
import os

# Takes an s-box and converts it to an array in c-code, expressed as a string
def c_array(sbox, inv=False):
    N = math.sqrt(len(sbox))
    if not N.is_integer():
        print("Invalid dimensions! Not a square sbox")
    
    formatted_elements = ["0x{:02X}".format(i).lower() for i in sbox]
    hex_vals = ["{:01X}".format(i) for i in range(int(N))]
    arr_type = "rsbox" if inv else "sbox"

    string = f"static const uint8_t {arr_type}[{len(sbox)}] = {{\n"
    string += "// " + "     ".join("%s" %x for x in hex_vals) + "\n"
    for i, hex_str in enumerate(formatted_elements):
        if i % N == 0 and i != 0:
            string += f" // {hex_vals[int(i/N)-1]}\n"

        if i == len(formatted_elements)-1:
            string += hex_str
        else:
            string += hex_str + ", "
    string += f"   // {hex_vals[int(i/N)]}\n}};"
    return string

# Generate a single c file
def generate_c_file(sbox_info, prelude, postlude, complement, current_dir):
    box = sbox_info["box"]
    inv = sbox_info["inverse"]

    if len(inv) == 0:
        with open(f"{current_dir}/aes_postlude.c.part", "r") as f:
            c_inv = "".join(f.readlines())
    else:
        c_inv = c_array(inv, True)
    
    aes_list = [prelude, c_array(box), complement, c_inv, postlude]
    return "\n".join(aes_list)


# Generate c-files. If no parameter given, generate all of them. Otherwise, generate just one.
def generate_c_files(name=None):
    current_dir = os.path.dirname(os.path.realpath(__file__))


    with open(f"{current_dir}/aes_prelude.c.part", "r") as f:
        prelude = "".join(f.readlines())
    with open(f"{current_dir}/aes_postlude.c.part", "r") as f:
        postlude = "".join(f.readlines())
    with open(f"{current_dir}/aes_complement.c.part", "r") as f:
        complement = "".join(f.readlines())

    with open(f"{current_dir}/sboxes_info.pkl", "rb") as f:
        sbox_df = pickle.load(f)

    sbox_dict = sbox_df.T.to_dict()

    if name:
        # Check if name exists:
        if name in sbox_dict.keys():
            with open(f"{current_dir}/../firmware/crypto/tiny-AES128-C/aes.c", "w") as f:
                f.write(generate_c_file(sbox_dict[name], prelude, postlude, complement, current_dir))

        else:
            print("No such key exists:", name)
            return

    else:
        for name in sbox_dict.keys():
            with open(f"{current_dir}/{name}.c", "w") as f:
                f.write(generate_c_file(sbox_dict[name], prelude, postlude, complement, current_dir))


    return 

if __name__ == "__main__":
    generate_c_files()