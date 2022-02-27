library(targets)

# Define custom functions and other global objects -----------------------------
source("R/functions.R")
source("code/helper_functions.R")

# Set target-specific options such as package-----------------------------------
tar_option_set(packages = c("tidyverse",
                            "flextable",
                            "haven",
                            "lmtp"

))

base_cov <- c("Y0_any", "L0_inc", "L0_den", "L0_mari","L0_srh","Age","Sex")
l1_cov <- c("L1_inc", "L1_den", "L1_mari", "L1_srh")
expo <- c("A0_teeth","A1_teeth")
out <- "Y2"



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
                 ggplot2::ggsave(filename = "plots/missing_outcome.svg"),
               format= "file")

    ,
    tar_target(mis_by_exposure,
               plot_missing(descriptive_data,
                            by_var = "A0_teeth",
                            x_lab = "Number of teeth at baseline") %>%
                 ggplot2::ggsave(filename = "plots/missing_exposure.svg"),
               format= "file")

    ,
    # Flow of participants using R/flowchart.R
    tar_target(sample_flowchart,
             flow_chart(df,
                          expo,
                          out,
                          base_cov,
                          l1_cov),
               format= "file")
    ,



    # Create analytic data -----------------------------------------------------
    tar_target(analytic_data,
               get_analytic_data(descriptive_data),
               format= "rds")
    ,
    # Table 1 ------------------------------------------------------------------
    tar_target(table_1,
               create_table1(analytic_data))
    ,


    # Run mice & get dummies fir cat_vars---------------------------------------
    tar_target(mi_data,
               run_mice(data = analytic_data,
                        cat_vars= c('Y0_any'),
                        m = 5) ,
               format = "rds")

    ,

    # Set-up TMLE --------------------------------------------------------------
    tar_target(a, c("A0_teeth", "A1_teeth"))
    ,
    tar_target(y, "Y2_any")
    ,
    tar_target(w, c("Age", "Sex",
                    "Y0_any_2","Y0_any_3","Y0_any_4","Y0_any_5","Y0_any_6"))
    ,
    tar_target(tv, list(c("L0_inc",
                          "L0_iadl",
                          "L0_mari"),
                        c("L1_inc",
                          "L1_iadl",
                          'L1_mari')))
    ,
    tar_target(sl_lib, c("SL.glm", "SL.xgboost"))
    ,

    tar_target(params,
               list(trt = a,
                    outcome = y ,
                    baseline = w ,
                    time_vary=tv,
                    outcome_type = "binomial",
                    k=0,
                    learners_outcome = sl_lib,
                    learners_trt = sl_lib
               ))
    ,

    # Define shift functions to shift exposure----------------------------------

    tar_target(d1,
               function(data, trt) {
                 (data[[trt]]==1)*data[[trt]]+
                   (data[[trt]]!=1)* 1})
    ,
    tar_target(d2,
               function(data, trt) {
                 (data[[trt]]==2)*data[[trt]]+
                   (data[[trt]]!=2)* 2})
    ,
    tar_target(d3,
               function(data, trt) {
                 (data[[trt]]==3)*data[[trt]]+
                   (data[[trt]]!=3)* 3})
    ,
    tar_target(d4,
               function(data, trt) {
                 (data[[trt]]==4)*data[[trt]]+
                   (data[[trt]]!=4)* 4})
    ,
    tar_target(d5,
               function(data, trt) {
                 (data[[trt]]- 1 >= 1) * (data[[trt]] -1) +
                   (data[[trt]] - 1 < 1) * data[[trt]] })

  ,

  tar_target(table2,  # a .docx file will be created in the working folder
             create_table1(formula=
                             as.formula(A1_lbp ~      # header variable
                                          A0_lbp
                             ),
                           df=tmle_df,
                           n_cat= 5,   # number of categories in the head variable
                           file_name= "table_2",
                           header = "Changes in low back pain over two years"),
             format = 'file')

  ,


  # Set-up TMLE ----------------------------------------------------------------
  tar_target(a, c("A0_lbp", "A1_lbp"))
  ,
  tar_target(y, paste0("y", 1:2))
  ,
  tar_target(w, tmle_df %>% select(starts_with("W0")) %>% names())
  ,
  tar_target(l0, tmle_df %>% select(starts_with("L0")) %>% names())
  ,
  tar_target(l1, tmle_df %>% select(starts_with("L1")) %>% names())
  ,
  tar_target(tv, list(l0,l1))
  ,
  tar_target(cens, c("c1","c2"))
  ,
  tar_target(sl_lib, c("SL.glm","SL.xgboost", "SL.nnet"))
  ,

  tar_target(parms,
             list(trt = a,
                  outcome = y ,
                  baseline = w ,
                  time_vary=tv,
                  outcome_type = "survival",
                  cens = cens,
                  k = 0
                  ,
                  learners_outcome = sl_lib,
                  learners_trt = sl_lib
             ))
  ,

  # Run TMLE --------------------------------------------------

  tar_target(tmle_results_d0_d3,
             lapply(paste0("d", 0:3) , function (x) do.call(
               run_lmtp, c(parms, list(data= tmle_df,
                                       shift= eval(as.symbol(x))))
             )))
  ,
  tar_target(tmle_results_d4_d8,
             lapply(paste0("d", 4:8) , function (x) do.call(
               run_lmtp, c(parms, list(data= tmle_df,
                                       shift= eval(as.symbol(x))))
             )))
  ,
  tar_target(tmle_results_d9_d12,
             lapply(paste0("d", 9:12) , function (x) do.call(
               run_lmtp, c(parms, list(data= tmle_df,
                                       shift= eval(as.symbol(x))))
             )))
  ,

  tar_target(tmle_results_d13_d16,
             lapply(paste0("d", 13:16) , function (x) do.call(
               run_lmtp, c(parms, list(data= tmle_df,
                                       shift= eval(as.symbol(x))))
             )))
  ,

  tar_target(tmle_results_list, c(
    tmle_results_d0_d3, tmle_results_d4_d8,
    tmle_results_d9_d12, tmle_results_d13_d16
  ))

  ,


  tar_target(results_df,
             get_contrast(results_list= tmle_results_list ,
                          ref_d= 0, # d0 is the observed you can change into any d#
                          d_max=16,
                          type= "rr"))
  ,

  tar_target(results_df2,
             get_contrast(results_list= tmle_results_list ,
                          ref_d= 1, # d0 is the observed you can change into any d#
                          d_max=16,
                          type= "rr"))

)
