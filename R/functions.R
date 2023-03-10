
get_descriptive_data <- function(data){

 # iadl2 <- names(data %>% dplyr::select(dplyr::starts_with("iadl2")))

 df<-  data %>%  dplyr::mutate_if(is.numeric,function(x) dplyr::na_if(x,-9999)) %>%
    # dplyr::mutate_at(dplyr::vars(contains("teeth")),as.factor) %>%
    dplyr::mutate(A0_teeth = teeth10clean(teeth4_10),
                  A1_teeth = teeth13_16(teeth4_13),
                  Y0_any = pmin(cmnt6sg10, cmnt6hb10, cmnt6re10,cmnt6sp10,cmnt6vl10,
                                na.rm = T),
                  Y1_any = pmin(cmnt6sg13, cmnt6hb13, cmnt6re13,cmnt6sp13,cmnt6vl13,
                                na.rm = T),
                  Y2_any = pmin(cmnt6sg16, cmnt6hb16, cmnt6re16,cmnt6sp16,cmnt6vl16,
                                na.rm = T),
                  Y2 = dplyr::if_else(Y2_any<= 4,1,0),
                  mebr2nb10 = dplyr::if_else(mebr2nb10==0,1,mebr2nb10),
                  L0_inc = eqincome(hhine_10)/sqrt(mebr2nb10),
                  L1_inc = eqincome(hhine_13)/sqrt(mebr2nb13),
                  L0_den = dplyr::if_else(dent4_10==1,0,1),
                  L1_den = dplyr::if_else(dent4_13==1,0,1)) %>%

   # dplyr::mutate_at(dplyr::vars(dplyr::all_of(iadl2)),
   #                  function(x) dplyr::if_else(x==2,0,1)) %>%

   dplyr::mutate(
     # L0_iadl = rowSums(dplyr::select(.,iadl2bt10:iadl2ty10)),
     # L1_iadl = rowSums(dplyr::select(.,iadl2bt13:iadl2ty13)),
     L0_mari = dplyr::if_else(mari5st10>1,0, mari5st10),
     L1_mari = dplyr::if_else(mari5st13>1,0, mari5st13),
     L0_srh = srh_4_10,
     L1_srh = srh_4_13,
     C1 = part13 %>% as.numeric(),
     C2 = part16 %>% as.numeric(),
     Age = age_ysl10,
     Sex = sex_2_10) %>%
   dplyr::mutate(
     C1 = dplyr::case_when(C1==6 & C2==5~ 5,
                           C1==6 & (C2==4 | C2==3) ~ 4,
                           TRUE ~ C1),
     C2 = dplyr::case_when(C1==5 ~5,
                           C1==3 & C2==1 ~ 3,
                           C1==4 & C2==1 ~ 4,
                           C1==3 & C2==6 ~ 3,
                           C1==6 & C2==1 ~ 6,
                           TRUE~ C2),
     c1 = dplyr::if_else(C1==1,1,0),
     c2 = dplyr::if_else(C2==1,1,0),
     Sex = dplyr::if_else(Sex==1,1,0)) %>%

   dplyr::select(A0_teeth:tidyr::last_col()) %>%
   select(-Y1_any)

  base_vars <- df %>% dplyr::select(Age,Sex,dplyr::contains("0_")) %>% colnames()
  L1_vars <- df %>% dplyr::select(dplyr::contains("1_")) %>% colnames()

 df2 <- df %>%
   dplyr::mutate_at(dplyr::vars(dplyr::all_of(L1_vars)),
                    ~ dplyr::if_else(c1==0 & !is.na(.),-99,.)) %>%
   # dplyr::mutate(Y2= dplyr::if_else(c1==0 | c2==0 ,-88,Y2)) %>%
    dplyr::select(c(base_vars,"c1","C1",L1_vars,"C2","c2","Y2"))

 df2


}


get_mice_data <- function(df, mice_cars, imp_only_vars, ...){

  # get predictor matrix----

  allVars <- names(df)

  ## names of variables with missingness
  missVars <- names(df)[colSums(is.na(df)) > 0]

  ## mice predictorMatrix
  predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
  rownames(predictorMatrix) <- allVars
  colnames(predictorMatrix) <- allVars

  imputerMatrix <- predictorMatrix
  imputerMatrix[,mice_cars] <- 1

  imputedMatrix <- predictorMatrix
  imputedMatrix[,unique(c(mice_cars,imp_only_vars))] <- 1

  ## Keep correct imputer-imputed pairs only
  predictorMatrix <- imputerMatrix * imputedMatrix
  ## Diagonals must be zeros (a variable cannot impute itself)
  diag(predictorMatrix) <- 0

  set.seed(19851111)

  m <- mice::mice(data = df,
                  predictorMatrix = predictorMatrix,
                  ...)

  imp_df <- mice::complete(m,"long")

  return(imp_df)


}

is_binary <- function(x) {
  x0 <- na.omit(x) %>% as.character()
  length(unique(x0)) %in% 1:2 & all(x%in% 0:2)
}

is_cat <- function(x) {
  x0 <- na.omit(x)
  (length(unique(x0)) > 2 & length(unique(x0)) < 7)
}


# Labelling data ----
get_labelled_data <- function(df){

  num_vars <- df %>% na.omit() %>%
    dplyr::select_if(function(x) length(unique(x))> 6) %>% colnames()

  levels <- list(teeth= c(">= 20 teeth"= "4",
                          "10-19 teeth"= "3",
                          "1-9 teeth"= "2",
                          "Edentate"= "1"),

                 denture= c("Do not wear dentures" = "0",
                            "Wear dentures" = "1"),

                 maritial= c("Widowed,divorced, or unmarried"= "0",
                             "Married"= "1"),

                 social_6cat= c("Everyday"= "1",
                                "2-3 times a week"= "2",
                                "Once a week"= "3",
                                "1-2 times a month"= "4",
                                "Few times a year"= "5",
                                "Never"= "6"),

                 social_2cat= c("Yes"= "1",
                                "No"= "0",
                                "Censored"= "-88"),

                 sex= c("Male"="1", "Female"="0"),

                 srh= c("Very good"= "1",
                        "Good"= "2",
                        "Fair"= "3",
                        "Poor"= "4"))

  varlabels <- list(Age= "Age (Years)",
                    Sex="Sex",
                    L0_inc="Household income (2010)",
                    L1_inc="Household income (2013)",
                    L0_iadl= "IADL score (2010)",
                    L1_iadl= "IADL score (2013)",
                    L0_srh= "Self-rated health (2010)",
                    L1_srh= "Self-rated health (2013)",
                    L0_mari= "Marital status (2010)",
                    L1_mari= "Marital status (2013)",
                    Y0_any= "Social participation 2010",
                    A0_teeth= "Number of teeth (2010)",
                    A1_teeth= "Number of teeth (2013)",
                    L0_den= "Denture status (2010)",
                    L1_den= "Denture status (2013)",
                    Y2= "Social participation in 2016")

  tab_data <- df %>%

    dplyr::mutate(across(where(is_cat), factor)) %>%
    dplyr::mutate(A0_teeth= forcats::fct_recode(A0_teeth, !!!levels$teeth)%>%
                    forcats::fct_relevel(">= 20 teeth","10-19 teeth","1-9 teeth","Edentate"),
                  A1_teeth= forcats::fct_recode(A1_teeth, !!!levels$teeth),

                  L0_den=  forcats::fct_recode(L0_den, !!!levels$denture ),
                  L1_den=  forcats::fct_recode(L1_den, !!!levels$denture ),

                  L0_mari= forcats::fct_recode(L0_mari, !!!levels$maritial) ,
                  L1_mari= forcats::fct_recode(L1_mari, !!!levels$maritial) ,

                  L0_srh= forcats::fct_recode(L0_srh, !!!levels$srh),
                  L1_srh= forcats::fct_recode(L1_srh, !!!levels$srh),

                  Sex= forcats::fct_recode(Sex, !!!levels$sex),

                  Y0_any= forcats::fct_recode(Y0_any, !!!levels$social_6cat) ,

                  Y2= forcats::fct_recode(Y2, !!!levels$social_2cat) %>%
                    forcats::fct_relevel("Yes","No","Censored"))

  arsenal::labels(tab_data) <- varlabels

  return(tab_data)

}

# use descriptive_data for analysis of missing, create flow of participants
# plot missingness ----
plot_missing <- function(df, by_var= "Y2", x_lab= "Categories of outcome variable"){
  base_vars <- df %>% dplyr::select(Age,Sex,dplyr::contains("0_")) %>% colnames()
  L1_vars <- df %>% dplyr::select(dplyr::contains("1_")) %>% colnames()
  df %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(L1_vars,"Y2"))),
                     ~ dplyr::if_else(c1==0 & is.na(.),-99,.)) %>%
    dplyr::mutate(Y2= dplyr::if_else(c2==0 ,-88,Y2)) %>%

    get_labelled_data() %>%
    dplyr::mutate_at(
      dplyr::vars(-dplyr::all_of(dplyr::contains(c("inc","iadl")))),as.factor) %>%
    dplyr::select(dplyr::contains(c("teeth","iadl","inc","mari","den","srh","Y0")), !!by_var) %>%


    naniar::add_shadow(dplyr::contains(c("teeth","iadl","inc","mari","den","srh","Y0"))) %>%
    naniar::add_label_shadow(missing = "NA", complete = "!NA") %>%
    # mutate_at(vars(contains("NA")), as.character) %>%

    # mutate(any_missing= as.factor(any_missing)) %>%
    tidyr::pivot_longer(contains("_NA"), names_to = "na_var", values_to = "na_stat") %>%
    dplyr::mutate(na_var= dplyr::recode(na_var,
                                        L0_iadl_NA= "IADL 2010",
                                        L1_iadl_NA= "IADL 2013",
                                        L0_srh_NA= "SRH 2010",
                                        L1_srh_NA= "SRH 2013",
                                        L0_den_NA= "Denture use 2010",
                                        L1_den_NA= "Denture use 2013",
                                        L0_inc_NA= "Household income 2010",
                                        L1_inc_NA= "Household income 2013",
                                        L0_mari_NA= "Maritial status 2010",
                                        L1_mari_NA= "Maritial status 2013",
                                        A0_teeth_NA= "Number of teeth 2010",
                                        A1_teeth_NA= "Number of teeth 2013",
                                        Y0_any_NA= "Social participation 2010"
                                        # any_missing= "Missing any variable"

    ) ) %>%
    dplyr::filter(!is.na(!!rlang::sym(by_var))) %>%
    # dplyr::mutate(Y2= forcats::fct_recode(Y2, "Yes"= "1", "No"="0", "Censored"="-88")) %>%
    # dplyr::mutate(Y2= forcats::fct_relevel(Y2, "Yes", "No", "Censored")) %>%
    ggplot2::ggplot(ggplot2::aes(x= .data[[by_var]], fill= na_stat))+
    ggplot2::geom_bar(alpha=0.7,
                      position = "fill",
                      width = 0.5)+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(labels = scales::percent)+
    ggplot2::scale_x_discrete(expand=c(0.5,0))+

    ggplot2::facet_wrap(~na_var,scales = "free")+
    ggplot2::xlab(x_lab)+
    ggplot2::ylab("Percentage of missing")+
    ggplot2::theme_minimal()+
    # ggplot2::theme(legend.position = "bottom")+
    ggplot2::theme(text = ggplot2::element_text(size=12))+
    ggplot2::scale_fill_manual(name = 'Missing?', labels = c("No", "Yes"),values=c("darkblue",
                                             "red"))

}

# flow of participants  ----
flow_chart <- function(df,expo,out,base_cov,l1_cov){

  df_nomis_expo <- df %>% filter_at(vars(expo),~!is.na(.))
  df_nomis_out_expo <- df %>% filter_at(vars(c(expo,out)),~!is.na(.))

  df_nomis_base <- df_nomis_out_expo %>%filter_at(vars(base_cov),~!is.na(.))
  df_nomis_base_l1 <- df_nomis_out_expo %>%filter_at(vars(base_cov,l1_cov),~!is.na(.))

  # Number of eligible participants
  n_baseline <- nrow(df)

  n_nomis_out_expo <- nrow(df_nomis_out_expo)

  missing_expo <- n_baseline- nrow(df_nomis_expo)

  missing_out <- n_baseline- nrow(df_nomis_out_expo)- missing_expo

  # Number of people with missing baseline covariates
  missing_base_cov<- n_nomis_out_expo - nrow(df_nomis_base)

  # Number of people with missing follow-up covariates
  missing_l1_cov <-   n_nomis_out_expo - missing_base_cov- nrow(df_nomis_base_l1)

  # Checking numbers
  # nrow(analytic_data)+ missing_base_cov+ missing_l1_cov==n_baseline

  # Get the number censored at c1
  c1_censored <- df_nomis_base %>% filter(c1==0) %>% nrow()

  #Get the number censored at c2
  c2_censored <- df_nomis_base_l1 %>% filter(c1==1 & c2==0) %>% nrow()

  followed_l1 <- df_nomis_base %>% filter(c1==1) %>% nrow()

  followed_end <- df_nomis_base_l1 %>%  filter(c1==1 & c2==1) %>% nrow()

  n_at_2013 <-  n_nomis_out_expo -missing_base_cov

  n_at_2016 <- nrow(df_nomis_base_l1)

  # censoring ---------


  n_died_13 <- df_nomis_base %>% dplyr::filter(C1==5) %>% nrow()

  n_ineligible_13 <- df_nomis_base %>% dplyr::filter(C1==3|C1==4) %>% nrow()

  n_lost_13 <- df_nomis_base %>% dplyr::filter(C1==6) %>% nrow()

  # Check
  # n_died_13+ n_ineligible_13+ n_lost_13==c1_censored

  n_died_16 <- df_nomis_base_l1 %>% dplyr::filter(c1==1 & C2==5) %>% nrow()

  n_ineligible_16 <- df_nomis_base_l1 %>% dplyr::filter(c1==1 & (C2==3|C2==4)) %>% nrow()

  n_lost_16 <- df_nomis_base_l1 %>% dplyr::filter(c1==1 & C2==6) %>% nrow()

  # Check
  # n_died_16+ n_ineligible_16+ n_lost_16==c2_censored


  # Creating the flow chart ---------
  library(Gmisc, htmlTable,quietly = T)

  baseline <- Gmisc::boxGrob(glue::glue("Eligible baseline participants",
                                        "n = {txtInt(x=n_baseline)}",
                                        .sep = "\n"),
                             y=0.8, x = 0.5)

  followup13 <- Gmisc::boxGrob(glue::glue("Number of participants at 2013 follow-up",
                                          "n = {txtInt(n_at_2013)}",
                                          "{txtInt(c1_censored)} of them were censored:*",
                                          "- {txtInt(n_died_13)} had died",
                                          "- {txtInt(n_ineligible_13)} became ineligible",
                                          "- {txtInt(n_lost_13)} were lost to follow-up",

                                          .sep = "\n"),
                               y=0.5,x = 0.5)

  followup16 <- Gmisc::boxGrob(glue::glue("Number of participants at 2016",
                                          "n = {txtInt(n_at_2016)}",
                                          "{txtInt(c2_censored)} of them were censored:*",
                                          "- {txtInt(n_died_16)} had died",
                                          "- {txtInt(n_ineligible_16)} became ineligible",
                                          "- {txtInt(n_lost_16)} were lost to follow-up",

                                          .sep = "\n"),
                               y=0.2,x = 0.5)

  mis_expo_out <- Gmisc::boxGrob(glue::glue("Excluded due to missing exposure",
                                            "- {txtInt(missing_expo)}",
                                            "Excluded due to missing outcome",
                                            "- {txtInt(missing_out)}",
                                            .sep = "\n"),
                                 just = "left",
                                 y = 0.68,x = 0.25)

  mis_base_cov <- Gmisc::boxGrob(glue::glue("Excluded due to missing one or more",
                                            "baseline covariates",
                                            "- {txtInt(missing_base_cov)}",
                                            .sep = "\n"),
                                 just = "left",
                                 y = 0.68,x = 0.75)

  mis_l1_cov <- Gmisc::boxGrob(glue::glue("Excluded due to missing one or more",
                                            "covariates at the follow-up",
                                            "- {txtInt(missing_l1_cov)}",
                                            .sep = "\n"),
                                 just = "left",
                                 y = 0.35,x = 0.75)


  grDevices::svg(filename = "figures/flowchart.svg",width = 10,height = 12)

  grid::grid.newpage()

  print(baseline)
  print(mis_expo_out)
  print(mis_base_cov)
  print(followup13)
  print(mis_l1_cov)
  print(followup16)

  dev.off()

}

# flow of participants (imp data) ----
flow_chart <- function(df){



  # Number of eligible participants
  n_baseline <- nrow(df)

  n_2013 <-  df %>% filter(c1==1) %>% nrow()

  n_2016 <-  df %>% filter(c1==1 & c2==1) %>% nrow()


  # censoring ---------

  n_died_13 <- df %>% dplyr::filter(C1==5) %>% nrow()

  n_ineligible_13 <- df %>% dplyr::filter(C1==3|C1==4) %>% nrow()

  n_lost_13 <- df %>% dplyr::filter(C1==6) %>% nrow()

  n_c1 <- n_died_13+n_ineligible_13+ n_lost_13

  # Check
  # n_died_13+ n_ineligible_13+ n_lost_13+ n_2013==c1_censored

  n_died_16 <- df %>% dplyr::filter(c1==1 & C2==5) %>% nrow()

  n_ineligible_16 <- df %>% dplyr::filter(c1==1 & (C2==3|C2==4)) %>% nrow()

  n_lost_16 <- df %>% dplyr::filter(c1==1 & C2==6) %>% nrow()

  n_c2 <-  n_died_16+n_ineligible_16+ n_lost_16
  # Check
  # n_died_16+ n_ineligible_16+ n_lost_16==c2_censored


  # Creating the flow chart ---------
  library(Gmisc, htmlTable,quietly = T)

  baseline <- Gmisc::boxGrob(glue::glue("Eligible baseline participants",
                                        "n = {txtInt(x=n_baseline)}",
                                        .sep = "\n"),
                             y=0.9, x = 0.5)

  followup13 <- Gmisc::boxGrob(glue::glue("Number of participants at 2013 follow-up",
                                          "n = {txtInt(n_2013)}",
                                          .sep = "\n"),
                               y=0.5,x = 0.5)

  followup16 <- Gmisc::boxGrob(glue::glue("Number of participants at 2016",
                                          "n = {txtInt(n_2016)}",
                                          .sep = "\n"),
                               y=0.1,x = 0.5)

  cens1 <- Gmisc::boxGrob(glue::glue("Censored between baseline and 2013\n(n= {txtInt(n_c1)})",
                                     "- {txtInt(n_died_13)} died",
                                     "- {txtInt(n_ineligible_13)} became functionally dependent",
                                     "- {txtInt(n_lost_13)} were lost to folow-up",
                                     .sep = "\n"),
                          just = "left",
                          y = 0.7,x = 0.22)

  cens2 <- Gmisc::boxGrob(glue::glue("Censored between 2013 and 2016\n(n= {txtInt(n_c2)})",
                                     "- {txtInt(n_died_16)} died",
                                     "- {txtInt(n_ineligible_16)} became functionally dependent",
                                     "- {txtInt(n_lost_16)} were lost to folow-up",
                                     .sep = "\n"),
                          just = "left",
                          y = 0.3,x = 0.23)


  grDevices::svg(filename = "figures/flowchart_imp.svg",width = 10,height = 12)

  grid::grid.newpage()

  print(baseline)
  print(cens1)
  print(followup13)
  print(cens2)
  print(followup16)

  dev.off()

}


# Use analytic data for table 1 ----
get_tab1_data <- function(df, imp=T){

  if(imp){

  analytic_data <- df %>%
    dplyr::mutate_all(~dplyr::if_else(.==-99 | .==-88 ,NA_real_,.)) %>%
    get_labelled_data()

  }else{

  analytic_data <- df %>%
    stats::na.omit() %>%
    dplyr::mutate_all(~dplyr::if_else(.==-99 | .==-88 ,NA_real_,.)) %>%
    get_labelled_data()

  }

  return(analytic_data)

}

# supplementary table 1 -----
get_dropout_comparison <- function(df, rowvars){

  dat <- df %>%

    mutate(Follow_up = case_when(C1==1 & C2== 1 ~ "Remained",
                                 C1==5 | (c1==1 & C2==5) ~ "Died",
                                 (C1==3|C1==4)| (c1==1 & (C2==3|C2==4)) ~ "Became ineligible",
                                 C1==6 | (c1==1 & C2==6) ~ "Lost to follow-up")
           %>% as_factor() %>%  fct_relevel("Remained",
                                            "Became ineligible",
                                            "Died",
                                            "Lost to follow-up"
           ))

  upulR::create_table1(df = dat,
                       headvar = "Follow_up",
                       rowvars = rowvars,
                       file_name = "tables/supplementary_table1",
                       header = "Follow-up Status")
}


get_tmle_data <- function(df,imp=T){

  if (imp){

  for_tmle <- df %>%
    dplyr::mutate_at(vars(-c(.imp,.id)),
                     ~dplyr::if_else(.==-99 | .==-88 ,NA_real_,.)) %>%
    dplyr::mutate(Y2= dplyr::if_else(c1==0 | c2==0 ,NA_real_,Y2))

  }else{

  for_tmle <- df %>%
    stats::na.omit() %>%
    dplyr::mutate_all(~dplyr::if_else(.==-99 | .==-88 ,NA_real_,.))

  }


  make_dums <- for_tmle %>% na.omit() %>%
    dplyr::select_if(function(x)
      length(unique(x))< 7 & length(unique(x))>2) %>%
    select(-contains(c("teeth",".imp"))) %>%
    colnames()

  tmle_df <- for_tmle %>%
  fastDummies::dummy_cols(all_of(make_dums),
                          remove_first_dummy = T,
                          ignore_na = T,remove_selected_columns = T ) %>%
    mutate_all(as.numeric)


  tmle_df
}




# library(tidyverse)
# fun <- function(){
#   a<-1
#   b<-2
#   lst(a,b)
# }


run_lmtp <- function(data,
                     shift=NULL,
                     svy=FALSE,
                     wt_only=FALSE,
                     wt_var="",
                     ...){

  if (svy==TRUE){

    svy <- survey::svydesign(~psu, weights = data[[wt_var]], data = data)
    wt <- svy$prob
    psu <- svy$psu

    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt,
                         id = psu
      ))
  }

  else if (wt_only==TRUE){

    svy <- survey::svydesign(~1, weights = data[[wt_var]], data = data)
    wt <- svy$prob

    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt
      ))

  }

  else {

    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift))

  }

  return(m)
}


run_lmtp_imp_data <- function(data, m, d, params){

  data %>%
    filter(.imp== m) %>%

  purrr::lift(run_lmtp)(data=.,params, shift= eval(as.symbol(d)))


}








# ---------------------------Function to get contrasts-------------------------

get_contrast<- function(results_list,
                        ref_d,
                        d_max,...){

  results_df <- data.frame()

  for (i in 0:d_max){

    j <- ref_d+1
    k <- i+1


    if (i!=ref_d){

      m<- lmtp::lmtp_contrast(results_list[[k]],
                              ref = results_list[[j]], ...)
      vals <- m$vals %>%
        dplyr::mutate(Estimand= glue::glue("d",{ref_d},"_vs_d", {i}))

      results_df <- rbind(results_df, vals)
    }

  }

  return(results_df %>%
           dplyr::select(Estimand, theta, conf.low,
                         conf.high,std.error, p.value, shift, ref)
         )

}


# get pooled estimates ----
# pool_estimates <- function(df,mi=5){
#
#   df %>%
#     group_by(Estimand,.groups = 'keep') %>%
#     dplyr::mutate(variance= std.error^2,
#                   p.z = qnorm(p.value)) %>%
#     dplyr::summarise(theta.combined = mean(theta),
#                      p.z.mean= mean(p.z),
#                      p.den= sqrt(1 + var(p.z)),
#                      p.combined= pnorm( p.z.mean / p.den),
#                      Vw = sum(variance)/mi, # Within imputation variance
#                      Vb = sum((theta - mean(theta))^2/(mi-1)), # Between imputation variance
#                      Vt = Vw + Vb + Vb/mi, # Total variance
#                      SE.combined = sqrt(Vt),
#                      vm = (mi-1)*(1 + (Vw/((1+1/mi)*Vb)))^2, #df correction
#                      conf.low = theta.combined - qt(0.975, vm)*SE.combined,
#                      conf.high = theta.combined + qt(0.975, vm)*SE.combined) %>%
#     dplyr::select(Estimand, theta= theta.combined,conf.low,
#                   conf.high, p.value= p.combined) %>%
#     ungroup()
# }

pool_estimates <- function(df,mi=5){

  # from https://rdrr.io/cran/mice/src/R/barnard.rubin.R
  barnard.rubin <- function(m, b, t, dfcom = Inf) {
    lambda <- (1 + 1 / m) * b / t
    lambda[lambda < 1e-04] <- 1e-04
    dfold <- (m - 1) / lambda^2
    dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
    ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
  }

  df %>%
    group_by(contrast,.groups = 'keep') %>%
    dplyr::mutate(variance= std.error^2,
                  p.z = qnorm(p.value)) %>%
    dplyr::summarise(
      p.z.mean= mean(p.z),
      p.den= sqrt(1 + var(p.z)),
      p.combined= pnorm( p.z.mean / p.den),
      qbar = mean(theta),
      ubar = mean(variance), # Within imputation variance
      b = var(theta), # Between imputation variance
      t = ubar + (1 + 1 / mi) * b, # Total variance
      SE.combined = sqrt(t),
      df = barnard.rubin(mi, b, t), #df correction
      conf.low = qbar - qt(0.975, df)*SE.combined,
      conf.high = qbar + qt(0.975, df)*SE.combined) %>%
    dplyr::select(contrast, theta= qbar,conf.low,
                  conf.high, p.value= p.combined) %>%
    ungroup()
}


# contrast and pool ----
get_pooled_results <- function(...,
                               est,
                               ref_d = 0,
                               d_max = 4,
                               type= "or"){

  lists <- list(...)

  m<- length(lists)


  col_names <- paste0("m",c(1:m))

  names(lists) <- col_names

  a<- as_tibble(lists)

  b<- apply(a,2,FUN = get_contrast,
            ref_d = ref_d,
            d_max = d_max,
            type=type)

  r<- map_dfr(b, bind_rows)

  pool_estimates(r,mi=m) %>%
    mutate(ref= paste0("d",ref_d),
           est= est)
}



# Get table 2 (Point estimates, CIs, Evalues for with SL estimator )----
get_table2 <- function(df, estimator="sl"){

  flextable::set_flextable_defaults(
    font.family = "Arial" ,
    font.size = 11,
    text.align = "left",
    table.layout = "autofit",
    line_spacing= 0.9)

  # function for E-value----
  evalue <- function(theta,lo, high ) {
    e<- evalues.OR(est = theta, lo = lo, hi = high,rare = T)
    return(as.numeric(e[[2]])  %>%
             format(x=round(x = .,2),nsmall=2))
  }


  tab<- df %>% dplyr::filter(est==estimator) %>%
    mutate(Estimand= str_replace(Estimand,"d0", "Observed"),
           Estimand= str_replace(Estimand,"d1", "Edentate"),
           Estimand= str_replace(Estimand,"d2", "1-9 teeth"),
           Estimand= str_replace(Estimand,"d3", "10-19 teeth"),
           Estimand= str_replace(Estimand,"d4", ">=20 teeth"),
           Estimand= str_replace_all(Estimand,"_", " ")
    ) %>%
    arrange(ref, theta) %>%
    select(Estimand,theta,conf.low, conf.high, p.value) %>%
    mutate_at(vars(-Estimand),as.numeric) %>%
    mutate(e= pmap(.l = list(.$theta,.$conf.low,.$conf.high),
                   ~ evalue(theta = .x,lo = .y, high = ..3))) %>%
    mutate_at(vars(theta,conf.low, conf.high), ~format(round(.,2),nsmall= 2)) %>%
    mutate(p.value= format(round(p.value,3),nsmall= 3)) %>%
    mutate(or_ci = glue::glue("{theta} [{conf.low}-{conf.high}]")) %>%
    mutate(e= as.numeric(e)) %>%
    select(Contrast= Estimand, `OR [95% CI]`= or_ci, `P value`=p.value,
           `E-value`= e) %>%
    flextable::flextable()

  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, value = tab)
  fileout <- "tables/table_2_i.docx" # write in your working directory

  print(doc, target = fileout)

}


# Get figure 2 (Compare estimates of with_SL and without SL estimations)----

get_figure2 <- function(df){

  df %>% mutate_at(vars(-c(Estimand,ref,est)),as.numeric) %>%
    mutate(Estimand= str_replace(Estimand,"d0", "Observed"),
           Estimand= str_replace(Estimand,"d1", "Edentate"),
           Estimand= str_replace(Estimand,"d2", "1-9 teeth"),
           Estimand= str_replace(Estimand,"d3", "10-19 teeth"),
           Estimand= str_replace(Estimand,"d4", ">=20 teeth"),
           Estimand= str_replace_all(Estimand,"_", " ")
    ) %>%
    arrange(desc(theta)) %>%
    mutate(est= factor(est, levels = c("glm","sl"),
                       labels = c("Without SuperLearner",
                                  "With SuperLearner"
                       ))) %>%
    mutate(y= factor(Estimand),
           y= fct_reorder(y,desc(theta))) %>%
    mutate(ref= factor(ref,levels = c("d1","d0"),
                       labels = c("(a) Reference= TMLE estimate when the exposure shifted to be edentate",
                                  "(b) Reference= TMLE estimate related to observed level of exposure"))) %>%
    group_by(y,theta) %>%
    ggplot(aes(x=theta, y=y, color=est,xmin=conf.low,xmax= conf.high))+
    scale_color_manual(aesthetics = "color",values = c("grey70", "black"),
                       name=NULL)+
    geom_errorbar(position=ggstance::position_dodgev(height=0.5),
                  size=0.5,
                  width = 0.2)+
    facet_wrap(~ref,scales = "free_y",nrow = 2)+
    geom_point(position=ggstance::position_dodgev(height=0.5))+
    geom_segment(aes(x = 1 ,y=0,xend = 1, yend = 4.3),
                 color="grey60", linetype="dashed")+
    xlab("Odds Ratio (error bars indicate 95% confidence intervals)")+
    theme_classic()+
    theme(legend.position= "bottom",
          panel.grid.major.y = element_line(color="grey95"),
          legend.direction = "horizontal",
          # strip.text.x = element_blank(),
          strip.background = element_blank(),
          legend.background = element_blank(),
          axis.title.y = element_blank())

  ggsave("figures/figure_2_imp.pdf",device = "pdf",width = 8, height = 9,dpi = 900)


}


imp_filter <- function(data, m){

  data %>%
    dplyr::filter(.imp==m)

}


# rounding ----
round_uc <- function(data){

  data %>%
    mutate_at(vars(theta,conf.low, conf.high), ~format(round(.,2),nsmall= 2)) %>%
    mutate(`P value`= format(round(p.value,3),nsmall= 3),
           est_ci = glue::glue("{theta} [{conf.low}-{conf.high}]")) %>%

    dplyr::select(contrast,
                  `OR [95% CI]`= est_ci,
                  `P value`)
}




# Get tmle results table -----
# get_results_tables <- function(df,
#                                file_name="tables/table_2",
#                                out_type= "pdf"){
#
#   # function for plot
#   point_range <- function(df){
#
#     ggplot(data = df , aes(x=df$theta,  y=df$Estimand,
#                            xmin = df$conf.low, xmax= df$conf.high))+
#       geom_point(color="blue")+
#       geom_errorbarh(height=0.1, size=0.2) +
#       geom_vline(xintercept = 1, linetype = "longdash",
#                  colour = "grey50",size=.2)+
#       scale_x_continuous(limits=c(0.9, 1.45))+
#       theme_void()+
#       theme(panel.border = element_blank())
#   }
#   # function for E-value
#   evalue <- function(x) {
#
#     e<- evalues.OR(est = x[[1]], lo = x[[2]], hi = x[[3]],rare = T)
#     return(e[[2]])
#   }
#
#   grp <- c(1:nrow(df))
#
#   temp <- data.frame(grp,df) %>% mutate_at(vars(-Estimand,ref,est),as.numeric)
#
#   # Adding E-value ----
#   df_e <- temp %>% select(grp,theta,conf.low, conf.high) %>% group_by(grp) %>% nest()
#
#   Eval<- df_e %>% mutate(E= map_dbl(data, evalue)) %>% pull(E) %>% round(2)
#
#   # Adding point range ----
#   df_nest <- temp %>% group_by(grp) %>% nest()
#
#   z <- df_nest %>% mutate(plot = map(data, ~point_range(.x)))
#
#   p<- left_join(z %>% ungroup(),temp) %>%
#     select(-data,-grp) %>%
#     mutate(or_ci = glue::glue("{theta} [{conf.low}-{conf.high}]"),
#            `E-value` = Eval) %>%
#     select(Estimand, or_ci, `E-value`, plot)
#
#   # Creating table ----
#
#
#   flextable::set_flextable_defaults(
#     font.family = "Arial" ,
#     font.size = 10,
#     # table.layout = "autofit",
#     line_spacing= 0.9)
#
#
#   tab <- p %>%
#     flextable(cwidth = c(0.5,1.5,0.5,2)) %>%
#     set_header_labels(Estimand = "Contrast", or_ci= "OR [95% CI]", plot=" ") %>%
#     mk_par(
#       j = 4,
#       value = as_paragraph(gg_chunk(value = plot
#                                     , height = 0.2, width = 2
#       ))) %>%
#     flextable::align( align = "left", part = "all") %>%
#     flextable::align( j=4, align = "left", part = "all")  %>%
#     border_remove() %>%
#     hline(i=1, j=1:3,part = "header") %>%
#     hline_top(j=1:3,part = "header") %>%
#     hline_bottom(j=1:3)
#
#   if ("pdf" %in% out_type){
#     return(tab)
#   }else{
#     doc <- officer::read_docx()
#     doc <- flextable::body_add_flextable(doc, value = tab)
#     fileout <- tempfile(fileext = "/.docx")
#     fileout <- glue::glue({file_name},".docx") # write in your working directory
#
#     print(doc, target = fileout)
#
#   }
#
# }






