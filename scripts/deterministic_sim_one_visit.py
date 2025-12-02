#!/usr/bin/env python3
import masterpy
import scipy as sp
import sys
import os
import subprocess
import json
import numpy as np

# get arguments
out_path = './sir_visit_approx'
prefix = 'out'
idx = 0
remove_big_files = False
if len(sys.argv) >= 2:
    out_path     = sys.argv[1]
if len(sys.argv) >= 3:
    prefix       = sys.argv[2]
if len(sys.argv) >= 4:
    idx          = int(sys.argv[3])
if len(sys.argv) == 5:
    remove_big_files = sys.argv[4] == "True"


# model setup
args = {
    'dir'                : out_path,        # dir for simulations
    'prefix'             : prefix,          # project name(s)
	'model_type'         : 'sir',           # model type defines states & events
    # model variant defines rates
    'model_variant'      : ['EqualRates',
                            'noTree',
                            # 'ImmunityLoss',
                            # 'Exposed',
                            # 'RecoverApproximation',
                            'Visitor',  # [Migration, Visitor, VisitorApproximation]
                            'Deterministic'],           # [Stochastic, Deterministic]
    'num_char'           : 1,               # number of evolutionary characters 
    'num_states'         : 2,               # number of states per character
    'num_hidden_char'    : 1,               # number of hidden states
    'num_exposed_cat'    : 1,               # number of infected Exposed stages (>1)
    'stop_time'          : None,            # time to stop simulation 
    'min_num_taxa'       : 2,             # min number of taxa for valid sim
    'max_num_taxa'       : 2000,             # max number of taxa for valid sim
    'prop_extant_sampled' : 0.1,             # Expected proportion of lineages at stop_time to be sampled in tree
    'max_num_unsampled_lineages' : 250,      # max_num_taxa * max_num_unsampled_lineages == stopping condition
    'rv_fn'              : {                # distributions for model params
        'R0'                  : sp.stats.uniform.rvs,
        'Recover'             : sp.stats.loguniform.rvs,
        'Sample'              : sp.stats.loguniform.rvs,
        'ProgressInfected'    : sp.stats.loguniform.rvs,
        'VisitDepart'         : sp.stats.loguniform.rvs,
        'VisitReturn'         : sp.stats.loguniform.rvs,
        'S0'                  : sp.stats.uniform.rvs,
        'R2S'                 : sp.stats.loguniform.rvs,
        'Stop_time'           : sp.stats.uniform.rvs,
        'nSampled_tips'       : sp.stats.randint.rvs,
        'Time_of_interest'    : sp.stats.expon.rvs
    },
    'rv_arg'                : {                # loc/scale/shape for param dists
        'R0'                : { 'loc' : 5.0,    'scale' : 5.0  }, 
        'Recover'           : { 'a' : 10**-1,   'b' : 10**0    },     # 1 to 10 days, rate of 0.1 to 1
        'Sample'            : { 'a' : 5 * 10**-2,   'b' : 10**-1   },     # 10 to 100 days, rate of 0.01 to 0.1
        'ProgressInfected'  : { 'a' : 10**-1,   'b' : 10**0    },     # 1 to 10 days,  rate of 0.1 to 1.0
        'VisitDepart'       : { 'a' : 10**-2,   'b' : 10**-1   },     # 100 to 1000 days, rate of 0.01 to 0.1
        'VisitReturn'       : { 'a' : 10**-1,   'b' : 10**0    },     # 1 to 10 days, rate of 0.1 to 1
        'S0'                : { 'loc' : 100000000., 'scale' : 0 }, # 50000 to 250000 ind. in population
        'R2S'               : { 'loc' : 0.0001, 'scale' : 0.0099 },   # 10000 to 100 days, rate of 0.0001 to 0.01
        'Stop_time'         : { 'loc' : 3.0,    'scale' : 1.0  },     # 5 to 5 days, rate of 0
        'nSampled_tips'     : { 'low' : 999.0,  'high' : 1000.   },    # subsample samples
        'Time_of_interest'  : { 'loc' : 0,    'scale' : 30}
    }
}

# filesystem paths
tmp_fn       = out_path + "/" + prefix + '.' + str(idx)
xml_fn       = tmp_fn + '.xml'
param_mtx_fn = tmp_fn + '.param_col.csv'
param_vec_fn = tmp_fn + '.labels.csv'
phy_nex_fn   = tmp_fn + '.nex.tre'
phy_nwk_fn   = tmp_fn + '.tre'
dat_nex_fn   = tmp_fn + '.dat.nex'
dat_json_fn  = tmp_fn + '.json'

# make sim dir for output
os.makedirs(out_path, exist_ok=True)

# load model
my_model = masterpy.load(args)

# NOTE: .set_model is called in the constructor.
# only using here to set the seed for validation:
my_model.set_model(idx)

# make XML
xml_str = my_model.make_xml(idx)

# save xml output
masterpy.write_to_file(xml_str, xml_fn)

# call BEAST
x = subprocess.run(['beast', xml_fn], capture_output=False)

# include sim stats such as prevalence at time pt of interest and 
# cumulative number of samples up to present
sim_stats = my_model.get_json_stats(dat_json_fn)

# gather all data for labels files
params_and_popstats = {**my_model.params, **sim_stats}
params_and_popstats = masterpy.log_params(params_and_popstats, 
                                          ['R0', 'Sample', 'Recover', 'VisitDepart',
                                           'VisitReturn','Infect'])
param_mtx_str, param_vec_str = masterpy.param_dict_to_str(params_and_popstats)

# make label file
masterpy.write_to_file(param_mtx_str, param_mtx_fn)
masterpy.write_to_file(param_vec_str, param_vec_fn)

# delete json file
if remove_big_files:
    os.remove(dat_json_fn)

quit()
