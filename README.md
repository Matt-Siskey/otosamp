# otosamp
pipeline_push: Code for grabbing RACEBase data, running bootstrap estimator on age data, calculating input sample sizes & age comps, feeding replicated data products to assessment models, running those models & saving outputs

pipeline_output: Code for processig output from bootstrap estimator (number of otoliths, number of tows recorded across sampling rate scenarios), running GLMMs on processed output from bootstrap estimator (otos/tow & tows across treatments), processing output from stock assessment model runs, running Levene's test on OFL distributions, and plotting.

Cost & Revenue Analysis: Code for cost function relating FTE salary, ageing time to number of otoliths sampled across treatments (total costs per treatment); code to conducnt p-star analysis using among-bootstrap uncertainty in OFL, ABC/OFL buffer to generate new realizations of ABC across sampling rate treatments, and hence new realizations of revenue based on recent industry-reported revenues.

Seeds: Creates seed lists for bootstrap estimator sampling.
