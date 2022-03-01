library(targets)
library(upulR) # personal R package for creating Table-1

# Define custom functions and other global objects -----------------------------
source("R/functions.R")
source("R/helper_functions.R")


base_cov <- c("Age","Sex","L0_inc", "Y0_any","L0_den", "L0_mari","L0_srh")
l1_cov <- c("L1_inc", "L1_den", "L1_mari", "L1_srh")
expo <- c("A0_teeth","A1_teeth")
out <- "Y2"


d0 <-  NULL

d1 <-  function(data, trt) {
  (data[[trt]]==1)*data[[trt]]+ (data[[trt]]!=1)* 1
}

d2 <-  function(data, trt) {
  (data[[trt]]==2)*data[[trt]]+ (data[[trt]]!=2)* 2
}

d3 <-  function(data, trt) {
  (data[[trt]]==3)*data[[trt]]+ (data[[trt]]!=3)* 3
}

d4 <-  function(data, trt) {
  (data[[trt]]==4)*data[[trt]]+ (data[[trt]]!=4)* 4
}





# Set target-specific options such as packages-----------------------------------
tar_option_set(packages = c("tidyverse", "haven",
                            "Gmisc", "htmlTable",
                            "flextable",
                            "lmtp"))


# Starting the list of targets--------------------------------------------------
list(
  tar_target(df_file,
             "data/selected",
             format = "file")
  ,
  # Working data -------------------------------------------------------------
  tar_target(working_df,
             readRDS(file=df_file))
  ,

  # create a dataset for descriptive analysis---------------------------------
  tar_target(descriptive_data,
             get_descriptive_data(working_df))

  ,

  # plot distribution of missing covariates ----------------------------------
  tar_target(mis_by_outcome,
             plot_missing(descriptive_data,
                          by_var = "Y2",
                          x_lab = "Social participation in 2016 (Outcome)") %>%
               ggplot2::ggsave(filename = "figures/missing_outcome.svg",
                               width = 12,height = 10 ),
             format= "file")

  ,
  tar_target(mis_by_exposure,
             plot_missing(descriptive_data,
                          by_var = "A0_teeth",
                          x_lab = "Number of teeth at baseline") %>%
               ggplot2::ggsave(filename = "figures/missing_exposure.svg",
                               width = 12,height = 10 ),
             format= "file")

  ,
  # Flow of participants (Add connecting arrows using Inkscape)
  tar_target(sample_flowchart,
             flow_chart(df= descriptive_data ,
                        expo,out,base_cov,l1_cov))
  ,

  # Table 1 --------------------------------------------------------------------
  tar_target(tab1_data,
             get_tab1_data(descriptive_data),
             format= "rds")
  ,

  tar_target(table_1,
             create_table1(df = tab1_data,
                           headvar = out,
                           rowvars = c(expo[1],base_cov),
                           headvar_na_level = "Censored",
                           file_name = "tables/table_1",
                           header = "Social participation in 2016"))
  ,

  # get a tmle ready data set-----------------------------------------------------------
  # dummify all categorical variables/ all variables as numeric

  tar_target(tmle_data,
             get_tmle_data(descriptive_data),
             format= "rds")
  ,


  # Set-up TMLE --------------------------------------------------------------
  tar_target(a, expo)  # time varying exposure (2010 & 2013)
  ,

  tar_target(y, out)   # Outcome (2016)
  ,
  # Time-invariant covariates
  tar_target(w, colnames(tmle_data %>% select(Age,Sex,contains("Y0"))))

  ,
  # time-varying covariates
  tar_target(tv, list(colnames(tmle_data %>% select(contains("L0"))),
                      colnames(tmle_data %>% select(contains("L1")))))
  ,

  tar_target(cens, c("c1","c2"))
  ,

  tar_target(sl_lib, c("SL.glm", "SL.xgboost", "SL.nnet"))

  ,

  tar_target(params,
             list(trt = a,
                  outcome = y ,
                  baseline = w ,
                  time_vary=tv,
                  outcome_type = "binomial",
                  cens = cens,
                  k=0
                  # ,
                  # learners_outcome = sl_lib,
                  # learners_trt = sl_lib
             ))
  ,

  # Define shift functions to shift exposure----------------------------------


  # Run TMLE --------------------------------------------------

  tar_target(tmle_results,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data,
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(tmle_results_with_SL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data,
                                             learners_outcome = sl_lib,
                                             learners_trt = sl_lib,
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(results_df,
             get_contrast(results_list= tmle_results ,
                          ref_d= 1, # d0 is the observed you can change into any d#
                          d_max=4,
                          type= "or"))
  ,

  tar_target(results_df_with_SL,
             get_contrast(results_list= tmle_results_with_SL ,
                          ref_d= 1, # d0 is the observed you can change into any d#
                          d_max=4,
                          type= "or"))
  # ,




)
