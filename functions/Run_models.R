#Run model script

pacman::p_load(tidyverse,
               brms,
               parallel
               )

source("functions/cbs_model_runners.R")


#Run models
#Mass
run_my_cbs_models(data= mm.g, name="greater_mass", part = "mass", seed=6)
run_my_cbs_models(data= mm.l, name="lesser_mass", part = "mass", seed=6)

run_my_cbs_models(data= mm.g, name="greater_wing", part = "wing.chord", seed=6)
run_my_cbs_models(data= mm.l, name="lesser_wing", part = "wing.chord", seed=6)

run_my_cbs_models(data= mm.g, name="greater_tarsus", part = "tarsus", seed=6)
run_my_cbs_models(data= mm.l, name="lesser_tarsus", part = "tarsus", seed=6)

run_my_cbs_models(data= mm.g, name="greater_wt_index", part = "wt.index", seed=6)
run_my_cbs_models(data= mm.l, name="lesser_wt_index", part = "wt.index", seed=6)

