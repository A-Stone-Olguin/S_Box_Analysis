from chipwhisperer.helper_functions import *

make_firmware("AES", platform = 'CWNANO', c_target = 'TINYAES128C', scope_t = 'OPENADC', sbox2 = False)