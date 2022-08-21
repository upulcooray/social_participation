library(targets)
library(tarchetypes)
library(tidyverse)
library(upulR) # personal R package for creating Table-1

library(future)
library(future.callr)



# Define custom functions and other global objects -----------------------------
source("R/functions_v2.R")
source("R/helper_functions.R")



base_cov <- c("Age","Sex","L0_inc", "Y0_any","L0_den", "L0_mari","L0_srh")
l1_cov <- c("L1_inc", "L1_den", "L1_mari", "L1_srh")
expo <- c("A0_teeth","A1_teeth")
out <- "Y2"


d0 <-  NULL

d1 <-  function(data, trt) {  # what if edentulous were 1-9

  out <- list()
  a= data[[trt]]
  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('1','2','3')) {
      out[[i]] <- as.character(a[i])
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) - 1
    }
  }
  factor(unlist(out),levels = c(1:4),  ordered = TRUE)
}


d1.2 <-  function(data, trt) {  # what if edentulous were 1-9 & 1-9 were 10-19

  out <- list()
  a= data[[trt]]
  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('1','2')) {
      out[[i]] <- as.character(a[i])
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) - 1
    }
  }
  factor(unlist(out),levels = c(1:4),  ordered = TRUE)
}


d2 <-  function(data, trt) {  # what if edentulous were 1-9 & 1-9 were 10-19 & 10-19 were >20

  out <- list()
  a= data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('1')) {
      out[[i]] <- as.character(a[i])

    } else {
      out[[i]] <- as.numeric(as.character(a[i])) - 1
    }
  }
  factor(unlist(out), levels = c(1:4), ordered = TRUE)

  }


d3 <-  function(data, trt) {  # What if all were >20

  out <- list()
  a= data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('1')) {
      out[[i]] <- as.character(a[i])

    # }else if (as.character(a[i]=="3")){
    #   as.numeric(as.character(a[i])) - 1
    } else {
      out[[i]] <- 1
    }
  }
  factor(unlist(out), levels = c(1:4),  ordered = TRUE)

}


d3.1 <-  function(data, trt) {  # What if >20 were 10-19

  out <- list()
  a= data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('2','3','4')) {
      out[[i]] <- as.character(a[i])

    # }else if (as.character(a[i]=="3")){
    #   as.numeric(as.character(a[i])) - 1
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) + 1
    }
  }
  factor(unlist(out),levels = c(1:4), ordered = TRUE)

}


d3.2 <-  function(data, trt) {  # What if  >20 were 10-19 & 10-19 were 1-9

  out <- list()
  a= data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('3','4')) {
      out[[i]] <- as.character(a[i])

    # }else if (as.character(a[i]=="3")){
    #   as.numeric(as.character(a[i])) - 1
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) + 1
    }
  }
  factor(unlist(out),levels = c(1:4), ordered = TRUE)

}

d3.3 <-  function(data, trt) {  # What if  >20 were 10-19 & 10-19 were 1-9 & 1-9 were edentulous

  out <- list()
  a= data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('4')) {
      out[[i]] <- as.character(a[i])

    # }else if (as.character(a[i]=="3")){
    #   as.numeric(as.character(a[i])) - 1
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) + 1
    }
  }
  factor(unlist(out),levels = c(1:4), ordered = TRUE)

}

d4 <-  function(data, trt) {  # if eden and 1-9 peple become 10-19

  out <- list()
  a= data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('3','4')) {
      out[[i]] <- as.character(a[i])

    # }else if (as.character(a[i]=="3")){
    #   as.numeric(as.character(a[i])) - 1
    } else {
      out[[i]] <- 3
    }
  }
  factor(unlist(out),levels = c(1:4), ordered = TRUE)

}

d5 <-  function(data, trt) {  # What if all were edentulous

  out <- list()
  a= data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c('4')) {
      out[[i]] <- as.character(a[i])

    # }else if (as.character(a[i]=="3")){
    #   as.numeric(as.character(a[i])) - 1
    } else {
      out[[i]] <- 4
    }
  }
  factor(unlist(out),levels = c(1:4), ordered = TRUE)

  }




# Set target-specific options such as packages-----------------------------------
tar_option_set(packages = c("tidyverse", "haven",
                            "Gmisc", "htmlTable",
                            "flextable", "EValue",
                            "lmtp","mice", "upulR",
                            "compareGroups"))


plan(callr)
set.seed(198511110)

# Starting the list of targets--------------------------------------------------
list(
  tar_target(df_file,
             "data/selected.csv",
             format = "file")
  ,
  # Working data -------------------------------------------------------------
  tar_target(working_df,
             read_csv(file=df_file))
  ,

  # create a dataset for descriptive analysis---------------------------------
  tar_target(descriptive_data,
             get_descriptive_data(working_df))

  ,

  tar_target(imp_data,
             get_mice_data(descriptive_data , m=5))

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
             flow_chart(df= descriptive_data))
  ,

# Table 1 --------------------------------------------------------------------

  tar_target(table1,
             get_table1(df=imp_data,
                        y_var="Y2",
                        file_name="tables/table1.csv"),
             format= "file")
  ,


  tar_target(dropouts_comp,
             get_dropout_comparison(df=descriptive_data),
             format = "file")

  ,


  # get a tmle ready data set---------------------------------------------------
  # dummify all categorical variables/ all variables as numeric

  tar_target(tmle_data,
             get_tmle_data(imp_data)
             ,

             format= "rds")
  ,


  # Set-up TMLE ----------------------------------------------------------------
  tar_target(a,  c("A0_teeth","A1_teeth"))  # time varying exposure (2010 & 2013)
  ,

  tar_target(y, "Y2")   # Outcome (2016)
  ,
  # Time-invariant covariates
  tar_target(w, colnames(tmle_data %>% select(contains("w_"))))

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
                  intervention_type ="mtp",
                  cens = cens
                  # ,
                  # learners_outcome = sl_lib,
                  # learners_trt = sl_lib
             ))
  ,

  tar_target(sl_lib2, c("SL.glm","SL.gam",
                        "SL.xgboost"
                        # ,
                        # "SL.randomForest"
  ))

  ,



  tar_target(params_sl,
             params %>%
               modifyList(list(learners_outcome = sl_lib2,
                               learners_trt = sl_lib2))

               )

  ,
#
#   # Run TMLE --------------------------------------------------
#
#   tar_target(d, paste0("d",0:5))
#
#   ,
#
  tar_target(m, 1:5)

  ,

  tar_target(imps, cbind(imp=m) %>% as.data.frame())

  ,
#
# # with super learner -------
#
  tar_target(tmle_d0_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d0",
                                                           params = params_sl))))

  ,


  tar_target(tmle_d1_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d1",
                                                           params = params_sl))))

  ,


  tar_target(tmle_d2_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d2",
                                                           params = params_sl))))

  ,


  tar_target(tmle_d3_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d3",
                                                           params = params_sl))))

  ,


  tar_target(tmle_d4_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d4",
                                                           params = params_sl))))

  ,

  tar_target(tmle_d5_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d5",
                                                           params = params_sl))))
  ,

  tar_target(tmle_d1_2_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d1.2",
                                                           params = params_sl))))
  ,

  tar_target(tmle_d3_1_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d3.1",
                                                           params = params_sl))))
  ,

  tar_target(tmle_d3_2_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d3.2",
                                                           params = params_sl))))
  ,

  tar_target(tmle_d3_3_sl,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d3.3",
                                                           params = params_sl))))

  # ,
#
#   # without super learner ------
#
#   tar_target(tmle_d0,
#              imps %>%
#                mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data2 ,
#                                                            m= .x,
#                                                            d= "d0",
#                                                            params = params))))
#
#   ,
#
#   tar_target(tmle_d1,
#              imps %>%
#                mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data2 ,
#                                                            m= .x,
#                                                            d= "d1",
#                                                            params = params))))
#
#   ,
#   tar_target(tmle_d2,
#              imps %>%
#                mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data2 ,
#                                                            m= .x,
#                                                            d= "d2",
#                                                            params = params))))
#
#   ,
#
#   tar_target(tmle_d3,
#              imps %>%
#                mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data2 ,
#                                                            m= .x,
#                                                            d= "d3",
#                                                            params = params))))
#
#   ,
#
#   tar_target(tmle_d4,
#              imps %>%
#                mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data2 ,
#                                                            m= .x,
#                                                            d= "d4",
#                                                            params = params))))
#
#   ,
#
#   tar_target(tmle_d5,
#              imps %>%
#                mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data2 ,
#                                                            m= .x,
#                                                            d= "d5",
#                                                            params = params))))
#
  ,

tar_target(results_sl,

           cbind(imp= imps$imp,
                 d0= tmle_d0_sl$tmle,
                 d1= tmle_d1_sl$tmle,
                 d1_2= tmle_d1_2_sl$tmle,
                 d2= tmle_d2_sl$tmle,
                 d3_1= tmle_d3_1_sl$tmle,
                 d3_2= tmle_d3_2_sl$tmle,
                 d3_3= tmle_d3_3_sl$tmle,
                 d3= tmle_d3_sl$tmle,
                 d4= tmle_d4_sl$tmle,
                 d5= tmle_d5_sl$tmle) %>%
             as_tibble() %>%
             unnest(imp) %>%
             mutate(
               d0_vs_d1= map2(d1,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               d0_vs_d1_2= map2(d1_2,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               d0_vs_d2= map2(d2,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               d0_vs_d3_1= map2(d3_1,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               d0_vs_d3_2= map2(d3_2,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               d0_vs_d3_3= map2(d3_3,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               d0_vs_d3= map2(d3,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               # d0_vs_d4= map2(d4,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
               d0_vs_d5= map2(d5,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")))
           %>%
             dplyr::select(imp,contains("vs")) %>%
             pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
             mutate(results= map(results,~.$vals)) %>%
             unnest(cols = results) %>%
             pool_estimates()
)
,

tar_target(table2_data,
           get_table2_data(results_sl))

,

tar_target(table_2,
           get_table2(table2_data),
           format= "file")
,

tar_target(figure_3,
           plot_or(table2_data),
           format= "file")
# ,
#
#
# tar_quarto(quar, path = "paper/main_text.qmd")


# ,
#
# tar_target(contrast_results_df2,
#            nested_results_df2 %>%group_by(m,d) %>%
#
#              pivot_wider(names_from = d, values_from = results ) %>%
#              ungroup() %>%
#              mutate(
#                d0_vs_d1= map2(d1,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
#                d0_vs_d2= map2(d2,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
#                d0_vs_d3= map2(d3,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
#                d0_vs_d4= map2(d4,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
#                d0_vs_d5= map2(d5,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
#                # d0_vs_d6= map2(d6,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
#                # d0_vs_d7= map2(d7,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
#              )
#            %>%
#              dplyr::select(m,contains("vs")) %>%
#              pivot_longer(!m, names_to = "contrast", values_to = "results") %>%
#              mutate(results= map(results,~.$vals)) %>%
#              unnest(cols = results),
#            format= "rds")
# ,
#
# tar_target(results_main_2,
#            contrast_results_df2 %>%
#              pool_estimates(mi=10) %>%
#              round_uc(),
#            format= "rds")
#
# ,
#
#
#
# tar_target(tmle_res_m1_noSL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==1),
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
#
# tar_target(tmle_res_m1_SL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==1),
#                                                       learners_outcome = sl_lib,
#                                                       learners_trt = sl_lib,
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
# tar_target(tmle_res_m2_noSL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==2),
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
#
# tar_target(tmle_res_m2_SL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==2),
#                                                       learners_outcome = sl_lib,
#                                                       learners_trt = sl_lib,
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
#
# tar_target(tmle_res_m3_noSL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==3),
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
#
# tar_target(tmle_res_m3_SL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==3),
#                                                       learners_outcome = sl_lib,
#                                                       learners_trt = sl_lib,
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
# tar_target(tmle_res_m4_noSL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==4),
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
#
# tar_target(tmle_res_m4_SL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==4),
#                                                       learners_outcome = sl_lib,
#                                                       learners_trt = sl_lib,
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
# tar_target(tmle_res_m5_noSL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==5),
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
#
# tar_target(tmle_res_m5_SL,
#            lapply(paste0("d",0:4) ,
#                   function (x) do.call(run_lmtp,
#                                        c(params, list(data= tmle_data2 %>% filter(.imp==5),
#                                                       learners_outcome = sl_lib,
#                                                       learners_trt = sl_lib,
#                                                       shift= eval(as.symbol(x))))
#                   )))
# ,
#
#
# # contrast & pool ref=d0, est= "sl" -----
#
# tar_target(res_d0_sl,
#            get_pooled_results(tmle_res_m1_SL,
#                               tmle_res_m2_SL,
#                               tmle_res_m3_SL,
#                               tmle_res_m4_SL,
#                               tmle_res_m5_SL,
#                               est= "sl",
#                               ref_d = 0L))
#
# ,
#
# # contrast & pool ref=d1, est= "sl" -----
#
# tar_target(res_d1_sl,
#            get_pooled_results(tmle_res_m1_SL,
#                               tmle_res_m2_SL,
#                               tmle_res_m3_SL,
#                               tmle_res_m4_SL,
#                               tmle_res_m5_SL,
#                               est= "sl",
#                               ref_d = 1L))
#
# ,
#
#
# # contrast & pool ref=d0, est= "glm" -----
#
# tar_target(res_d0_glm,
#            get_pooled_results(tmle_res_m1_noSL,
#                               tmle_res_m2_noSL,
#                               tmle_res_m3_noSL,
#                               tmle_res_m4_noSL,
#                               tmle_res_m5_noSL,
#                               est= "glm",
#                               ref_d = 0L))
#
# ,
#
# # contrast & pool ref=d1, est= "glm" -----
#
# tar_target(res_d1_glm,
#            get_pooled_results(tmle_res_m1_noSL,
#                               tmle_res_m2_noSL,
#                               tmle_res_m3_noSL,
#                               tmle_res_m4_noSL,
#                               tmle_res_m5_noSL,
#                               est= "glm",
#                               ref_d = 1L))
#
# ,
#
#
# # combine pooled results -----
#
# tar_target(pooled_estimates,
#            rbind(res_d0_sl,res_d1_sl,
#                  res_d0_glm,res_d1_glm))
# ,
#
#
# tar_target(table_2,
#            get_table2(pooled_estimates),
#            format= "file")
# ,
#
# tar_target(figure_2,
#            get_figure2(pooled_estimates),
#            format = "file")
#
#

)
