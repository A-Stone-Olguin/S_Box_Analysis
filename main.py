import pickle
from chipwhisperer_minimal.metrics import num_traces, tvla

def main():
    try: 
        with open("./chipwhisperer_minimal/generate_c/sboxes_info.pkl", "rb") as f:
            sboxes_df = pickle.load(f)
    except FileNotFoundError:
        print("No pickled sbox info found, generating by running sboxes_info.py")
        import sboxes_info
        sboxes_info.main()
        with open("./chipwhisperer_minimal/generate_c/sboxes_info.pkl", "rb") as f:
            sboxes_df = pickle.load(f)

    sboxes_dict = sboxes_df.T.to_dict()
    device = "CWNANO"
    attack_method = "TVLA"
    if attack_method.upper() == "TVLA":
        filename=f"./results/avg_leaks_{attack_method.lower()}_{device.lower()}.txt"
    else:
        filename=f"./results/n_traces_{attack_method.lower()}_{device.lower()}.txt"

    for sbox_name in sboxes_dict.keys():
        sbox = sboxes_dict[sbox_name]["box"]
        if attack_method.upper() == "TVLA":
            avg_percent_leaks = tvla(sbox_name, device)
            with open(filename, "a") as f:
                print(sbox_name, avg_percent_leaks, file=f)

        elif attack_method.upper() == "CPA" or attack_method.upper() == "DPA": 
            N_threshold = num_traces(sbox_name, sbox, device, attack_method)
            with open(filename, "a") as f:
                print(sbox_name, N_threshold, file=f)
        else:
            print("Invalid attack method given! Please input a valid attack method: `CPA`, `DPA` or `TVLA`.")
            continue
    print("All done!")

if __name__ == "__main__":
    main()
