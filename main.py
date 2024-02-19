import pickle
from chipwhisperer_minimal.metrics import num_traces

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

    with open("./results/n_traces_cpa.txt", "w") as f:
        for sbox_name in sboxes_dict.keys():
            sbox = sboxes_dict[sbox_name]["box"]
            N_threshold = num_traces(sbox_name, sbox, "CWNANO", "CPA")
            print(sbox_name, N_threshold, file=f)
    print("All done!")

if __name__ == "__main__":
    main()
