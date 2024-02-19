from chipwhisperer_minimal.helper_functions import *

make_firmware("AES", platform = 'CWNANO', c_target = 'TINYAES128C', scope_t = 'OPENADC', sbox2 = False)
setup_result = setup_scope_prog("CWNANO")
res = gather_n_traces(setup_result, N=100, hexname="simpleserial-aes-CWNANO.hex")
print(res)