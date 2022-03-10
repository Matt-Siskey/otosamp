###############################################
### AMAK Data & Control File Writer
### Matt Siskey
### February 2021
###############################################

writeCtl_POP <- function(dat,y){
  cat(
    "Model_1	# model_name\n",
    "goa_pop_2020.dat	# data_file\n",
    "1961	# styr_rec_est\n",
    "2017	# endyr_rec_est\n",
    "2	# ph_Fdev\n",
    "1	# ph_avg_F\n",
    "2	# ph_recdev\n",
    "3	# ph_fish_sel\n",
    "3	# ph_srv1_sel\n",
    "-3	# ph_srv2_sel\n",
    "0.0614	# mprior\n",
    "0.1	# cvmprior\n",
    "2	# ph_m\n",
    "1.7	# sigrprior\n",
    "0.2	# cvsigrprior\n",
    "2	# ph_sigr\n",
    "1.15	# q_srv1prior\n",
    "0.447213595	# cvq_srv1prior\n",
    "3	# ph_q_srv1\n",
    "1	# q_srv2prior\n",
    "0.447213595	# cvq_srv2prior\n",
    "-3	# ph_q_srv2\n",
    "1977	# yr_catchwt\n",
    "50	# wt_ssqcatch\n",
    "50	# wt_ssq_catch2\n",
    "0	# wt_cpue\n",
    "1	# wt_srv1\n",
    "0	# wt_srv2\n",
    "1	# wt_fish_age\n",
    "1	# wt_srv1_age\n",
    "1	# wt_fish_size\n",
    "0	# wt_srv1_size\n",
    "0	# wt_srv2_size\n",
    "1	# wt_rec_var\n",
    "0.1	# wt_fmort_reg\n",
    "1	# wt_avg_sel\n",
    "3	# initial_LMR\n",
    "0.911884102089418	# yieldratio\n",
    CompDist,"# CompDist (1=multinomial, 2=dirichlet-multinomial)\n",
    log_theta_prior,"# log_theta_srv1_age_prior (rec: 0.5)\n",
    cv_theta_prior,"# cv_theta_srv1_age_prior (rec: 0.05)\n",
    ph_theta,"# ph_theta_srv1_age (rec: 2; set to negative value to turn off estimation of theta)\n",
    file=dat,sep=" ")
}

# file=dat,append=TRUE,sep=" ")
