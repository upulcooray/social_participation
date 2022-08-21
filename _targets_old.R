library(targets)
library(upulR) # personal R package for creating Table-1

library(future)
library(future.callr)



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
                            "flextable", "EValue",
                            "lmtp","mice", "upulR"))


plan(callr)

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

  tar_target(imp_data,
             get_mice_data(descriptive_data ,
                           mice_cars= c(expo[1],base_cov),
                           imp_only_vars= c(expo[2],l1_cov, out),
                           m=5))

  ,





  # plot distribution of missing covariates ----------------------------------
  tar_target(mis_by_outcome2,
             plot_missing(descriptive_data,
                          by_var = "Y2",
                          x_lab = "Social participation in 2016 (Outcome)") %>%
               ggplot2::ggsave(filename = "figures/missing_outcome.svg",
                               width = 12,height = 10 ),
             format= "file")

  ,
  tar_target(mis_by_exposure2,
             plot_missing(descriptive_data,
                          by_var = "A0_teeth",
                          x_lab = "Number of teeth at baseline") %>%
               ggplot2::ggsave(filename = "figures/missing_exposure.svg",
                               width = 12,height = 10 ),
             format= "file")

  ,
  # Flow of participants (Add connecting arrows using Inkscape)
  tar_target(sample_flowchart2,
             flow_chart_imp(df= descriptive_data ,
                        expo,out,base_cov,l1_cov))
  ,

  # Table 1 --------------------------------------------------------------------
  tar_target(tab1_data2,
             get_tab1_data(descriptive_data),
             format= "rds")
  ,

  tar_target(table_1_2,
             upulR::create_table1(df = tab1_data2,
                           headvar = out,
                           rowvars = c(expo[1],base_cov),
                           headvar_na_level = "Censored",
                           file_name = "tables/table_1",
                           header = "Social participation in 2016"))
  ,


  tar_target(dropouts_comparison2,
             get_dropout_comparison(df=tab1_data2,
                                    rowvars= c(expo[1],base_cov)))

  ,


  # get a tmle ready data set---------------------------------------------------
  # dummify all categorical variables/ all variables as numeric

  tar_target(tmle_data2,
             get_tmle_data(imp_data),
             format= "rds")
  ,


  # Set-up TMLE ----------------------------------------------------------------
  tar_target(a, expo)  # time varying exposure (2010 & 2013)
  ,

  tar_target(y, out)   # Outcome (2016)
  ,
  # Time-invariant covariates
  tar_target(w, colnames(tmle_data2 %>% select(Age,Sex,contains("Y0"))))

  ,
  # time-varying covariates
  tar_target(tv, list(colnames(tmle_data2 %>% select(contains("L0"))),
                      colnames(tmle_data2 %>% select(contains("L1")))))
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

  tar_target(nested_data_df,
             tribble(~d,~m,~data,~params,~params_sl,
                     d,m,tmle_data2,params,params_sl) %>%
               unnest(cols = d) %>%
               unnest(cols = m) %>%
               mutate(data=map2(data,m, imp_filter)),

             format= "rds")


  ,


  tar_target(tmle_d0,
             run_lmtp_imp_data(data= tmle_data2,
                               params= params,
                               d="d0"))




  ,

  tar_target(tmle_res_m1_noSL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==1),
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(tmle_res_m1_SL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==1),
                                             learners_outcome = sl_lib,
                                             learners_trt = sl_lib,
                                             shift= eval(as.symbol(x))))
                      )))
  ,
  tar_target(tmle_res_m2_noSL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==2),
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(tmle_res_m2_SL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==2),
                                             learners_outcome = sl_lib,
                                             learners_trt = sl_lib,
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(tmle_res_m3_noSL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==3),
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(tmle_res_m3_SL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==3),
                                             learners_outcome = sl_lib,
                                             learners_trt = sl_lib,
                                             shift= eval(as.symbol(x))))
                      )))
  ,
  tar_target(tmle_res_m4_noSL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==4),
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(tmle_res_m4_SL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==4),
                                             learners_outcome = sl_lib,
                                             learners_trt = sl_lib,
                                             shift= eval(as.symbol(x))))
                      )))
  ,
  tar_target(tmle_res_m5_noSL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==5),
                                             shift= eval(as.symbol(x))))
                      )))
  ,

  tar_target(tmle_res_m5_SL,
             lapply(paste0("d",0:4) ,
                    function (x) do.call(run_lmtp,
                              c(params, list(data= tmle_data2 %>% filter(.imp==5),
                                             learners_outcome = sl_lib,
                                             learners_trt = sl_lib,
                                             shift= eval(as.symbol(x))))
                      )))
  ,


  # contrast & pool ref=d0, est= "sl" -----

  tar_target(res_d0_sl,
             get_pooled_results(tmle_res_m1_SL,
                                tmle_res_m2_SL,
                                tmle_res_m3_SL,
                                tmle_res_m4_SL,
                                tmle_res_m5_SL,
                                est= "sl",
                                ref_d = 0L))

  ,

  # contrast & pool ref=d1, est= "sl" -----

  tar_target(res_d1_sl,
             get_pooled_results(tmle_res_m1_SL,
                                tmle_res_m2_SL,
                                tmle_res_m3_SL,
                                tmle_res_m4_SL,
                                tmle_res_m5_SL,
                                est= "sl",
                                ref_d = 1L))

  ,


  # contrast & pool ref=d0, est= "glm" -----

  tar_target(res_d0_glm,
             get_pooled_results(tmle_res_m1_noSL,
                                tmle_res_m2_noSL,
                                tmle_res_m3_noSL,
                                tmle_res_m4_noSL,
                                tmle_res_m5_noSL,
                                est= "glm",
                                ref_d = 0L))

  ,

  # contrast & pool ref=d1, est= "glm" -----

  tar_target(res_d1_glm,
             get_pooled_results(tmle_res_m1_noSL,
                                tmle_res_m2_noSL,
                                tmle_res_m3_noSL,
                                tmle_res_m4_noSL,
                                tmle_res_m5_noSL,
                                est= "glm",
                                ref_d = 1L))

  ,


  # combine pooled results -----

  tar_target(pooled_estimates,
             rbind(res_d0_sl,res_d1_sl,
                   res_d0_glm,res_d1_glm))
  ,


  tar_target(table_2,
             get_table2(pooled_estimates),
             format= "file")
  ,

  tar_target(figure_2,
             get_figure2(pooled_estimates),
             format = "file")



)
