# S_Box_Analysis
Repository to analyze S-boxes and determine what characteristics influence their side-channel resistance.
This repository demonstrates the work done for my Master's Thesis.

## About this repository
This repository aims to utilize side-channel analysis metrics to determine what S-boxes are more resistant to power-based side-channel attacks.
The encryption protocol that will be used is the Advanced Encryption Standard, but the S-box used within it will be varied.
Metrics involve using Differential Power Analysis, Correlation Power Analysis, and Test Vector Leakage Assessment.

The Side-channel analysis will be performed using the ChipWhisperer hardware, namely the ChipWhisperer Nano and the ChipWhisperer Lite devices.

## Getting Started
A few installations must take place to utilize this repository properly.
* [SageMath](https://doc.sagemath.org/html/en/installation/index.html) to ensure that the analysis of S-boxes can be computed.
* [ChipWhisperer](https://chipwhisperer.readthedocs.io/en/latest/#install) to use the same hardware used for this project.

TODO add more as python gets created