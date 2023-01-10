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
                            "compareGroups")
               )


plan(callr)
set.seed(198511110)

# Starting the list of targets--------------------------------------------------
list(
  tar_target(df_file,
             read_dta("~/Desktop/JAGES_data/10-13-16panel_with_datsuraku.dta"),
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

tar_target(res,
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
             unnest(imp)
)

,

tar_target(results_sl,
            res %>%
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

tar_target(marginal_estimates,

           res %>%
             pool_marginal() %>%
             filter(Scenario!="d4") %>%
             mutate(Scenario= c("Observed", paste("Scenario",1:8, sep = " "))) %>%
             quick_table(file = "appendix_table2"))

,


tar_target(figure_3,
           plot_or(table2_data),
           format= "file")
,

tar_quarto(quar, path = "paper/main_text.qmd")




)
