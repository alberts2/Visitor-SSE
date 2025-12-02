#!/usr/bin/env python3
import sys
import json
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np

# make plots and compute errors between visitor model and approximate model

json1_fn = sys.argv[1] # full model
json2_fn = sys.argv[2] # approx model

# For restricting comparison to exponential phase. S0 = 10^7
max_I_thresh = 50000
if len(sys.argv) > 4:
    max_I_thresh = float(sys.argv[3])

out_prefix = json1_fn
if len(sys.argv) > 4:
    out_prefix = sys.argv[4]


with open(json1_fn, 'r') as f:
    json1 = json.load(f)
with open(json2_fn, 'r') as f:
    json2 = json.load(f)

if len(json1) == 2:
    json1 = json1['trajectories'][0]
if len(json2) == 2:
    json2 = json2['trajectories'][0]

# S, I, and R shape is (k, k, 1, N)
# A shape is (k,N)
S1 = np.array(json1['Susceptible']) 
S2 = np.array(json2['Susceptible'])
I1 = np.array(json1['Contagious'])
I2 = np.array(json2['Contagious'])
R1 = np.array(json1['Recovered'])
R2 = np.array(json2['Recovered'])
A1 = np.array(json1['SampleCount'])
A2 = np.array(json2['SampleCount'])

# Only check approximation in exponential phase
max_I_thresh_idx = np.max([x for x in range(I1.shape[3]) \
                           if (np.sum(I1[:,:,0,x]) + np.sum(R1[:,:,0,x])) < max_I_thresh])

time_steps = S1.shape[-1]
locations = S1.shape[0]  # Assuming k x k locations, k is S1.shape[0]

with PdfPages(out_prefix + ".compare_approx_full.pdf") as pdf:
    # check that time pts are identical
#    plt.figure()
#    plt.plot(json1['t'], json2['t'])
#    plt.plot(json1['t'],json1['t'], linestyle = '--', color = "red")
#    plt.ylabel("Approx sim Time pts")
#    plt.xlabel("Full model sim Time pts")
#    pdf.savefig()
#    plt.close()

    # Overlay plot Contagious for each pair of locations (i, j)
    for i in range(locations):
        for j in range(locations):
            APE = 100 * np.abs((I2[i, j, 0, max_I_thresh_idx] - 
                                        I1[i, j, 0, max_I_thresh_idx])/ 
                                       I1[i, j, 0, max_I_thresh_idx])
            print("APE loc " +str(i) + " " + str(j) + " : " +  str(np.round(APE, decimals = 4)))

            plt.figure()
            plt.plot(json1['t'][0:max_I_thresh_idx], I1[i, j, 0, 0:max_I_thresh_idx], 
                     linestyle='-', color='blue', label='Full Model')
            plt.plot(json2['t'][0:max_I_thresh_idx], I2[i, j, 0, 0:max_I_thresh_idx], 
                     linestyle='--', color='red', label='Approximate Model')
            plt.title(f'Contagious from Location {i} in location {j}')
            plt.xlabel('Time')
            plt.ylabel('Number of Contagious')
            plt.legend()
            pdf.savefig()
            plt.close()

with PdfPages(out_prefix + ".num_samples.pdf") as pdf:
    # check that time pts are identical

    # Overlay plot Contagious for each pair of locations (i, j)
    for i in range(locations):
        plt.figure()
        plt.plot(json1['t'][0:max_I_thresh_idx], A1[i, 0:max_I_thresh_idx], 
                    linestyle='-', color='blue', label='Full Model')
        plt.plot(json1['t'][0:max_I_thresh_idx], A2[i, 0:max_I_thresh_idx], 
                    linestyle='--', color='red', label='Approximate Model')
        plt.title(f'Sampled in Location {i}')
        plt.xlabel('Time')
        plt.ylabel('Sample Count')
        plt.legend()
        pdf.savefig()
        plt.close()
