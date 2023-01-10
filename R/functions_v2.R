
is_binary <- function(x) {
  x0 <- na.omit(x) %>% as.character()
  length(unique(x0)) %in% 1:2
}

is_cat <- function(x) {
  x0 <- na.omit(x)
  (length(unique(x0)) > 2 & length(unique(x0)) < 7)
}

# Prep data-------
get_descriptive_data <- function(df){

  df %>%
    filter(adl_3_10==1) %>%  # Functionally independent at the baseline
    mutate(across(.fns = ~ifelse(. %in% c(-9999), NA, .)),
           across(.cols = contains("iadl2"), ~ifelse(.x==2,0,.x), .names = "l_{.col}")) %>%
    dplyr::mutate(
      A0_teeth = teeth10clean(teeth4_10),
      A1_teeth = teeth13_16(teeth4_13),
      L0_iadl = rowSums(dplyr::select(.,l_iadl2bt10:l_iadl2ty10)),
      L1_iadl = rowSums(dplyr::select(.,l_iadl2bt13:l_iadl2ty13)),
      y0_any = pmin(cmnt6sg10,
                    cmnt6hb10,
                    cmnt6re10,
                    cmnt6sp10,
                    cmnt6vl10,
                    na.rm = T),
      w_y0 = dplyr::if_else(y0_any<= 4,1,0),
      Y1_any = pmin(cmnt6sg13, cmnt6hb13,
                    cmnt6re13,cmnt6sp13,cmnt6vl13,
                    na.rm = T),
      Y2_any = pmin(cmnt6sg16, cmnt6hb16,
                    cmnt6re16,cmnt6sp16,cmnt6vl16,
                    na.rm = T),
      Y2 = dplyr::if_else(Y2_any<= 4,1,0),
      Y2_week = dplyr::if_else(Y2_any<= 3,1,0),
      mebr2nb10 = dplyr::if_else(mebr2nb10==0,1,mebr2nb10),
      L0_inc = eqincome(hhine_10)/sqrt(mebr2nb10),
      L1_inc = eqincome(hhine_13)/sqrt(mebr2nb13),
      L0_mari = dplyr::if_else(mari5st10>1,0, mari5st10),
      L1_mari = dplyr::if_else(mari5st13>1,0, mari5st13),
      L0_srh = srh_4_10,
      L1_srh = srh_4_13,
      L0_vision= dgns2vi10,
      L1_vision= dgns2vi13,
      L0_hear= dgns2he10,
      L1_hear= dgns2he13,
      L0_cancer= dgns2ca10,
      L1_cancer= dgns2ca13,
      L0_heart= dgns2hd10,
      L1_heart= dgns2hd13,
      L0_stroke= dgns2st10,
      L1_heart= dgns2st13,
      across(c("mebr2nb10","mebr2nb13"),
             ~case_when(.x<2~1,
                        .x>=6~6,
                        T~ .x), .names = "L{c(0:1)}_membr"),
      C1 = part13,
      C2 = part16,
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
      w_age = age_ysl10,
      w_sex = sex_2_10,
      w_educ= ifelse(educ5_10==5,NA,educ5_10),
      w_vision= dgns2vi10,
      w_hear= dgns2vi10) %>%
    select(starts_with(c("w_", "L0_", "A0_","c1","A1_","L1_","c2","Y2_")), Y2) %>%
    mutate(c2= ifelse(c1==0,0,c2)) %>%
    mutate_at(vars(contains(c("L1","A1_","Y2"))),
              ~ifelse(c1==0 , NA,.)) %>%
    mutate(Y2= ifelse(c2==0 ,NA,Y2)) %>%
    mutate(
      w_sex= factor(w_sex- 1, levels = c(0,1), labels = c("Male","Female")),  # make sex 0=male 1=female
      w_educ= factor(w_educ,
                     levels = 1:4,
                     labels = c("6yrs",
                                "6_9yrs",
                                "10_12yrs",
                                "12yrs" ), ordered = T),
      across(c(L0_srh,L1_srh),
             factor,
             levels=1:4,
             labels= c("Very good","Good","Fair","Poor"),
             ordered=T),
      across(c(A0_teeth,A1_teeth),
             factor,
             levels=c(1:4),
             labels= c( "Edentate",
                        "1-9 teeth",
                        "10-19 teeth",
                        ">= 20 teeth"),
             ordered=T),

      across(contains("mari"),
             factor,
             levels=c(1,0),
             labels= c("Married",
                       "Widowed,divorced, or unmarried")),
      across(c(w_y0,
               Y2,
               ends_with(c("vision","hear","cancer", "heart", "stroke"))),
             factor,
             levels = c(1,0),
             labels = c("Yes","No")))
}

# Impute missing----------
get_mice_data <- function(df, ...){

  #impute all missing values (not missing due to censoring)----
  to_imp<- df %>%
    mutate_at(vars(contains(c("L1","A1_"))),
              ~ifelse(c1==0 , -99,.)) %>%
    mutate(Y2= ifelse(c2==0 ,-999,Y2)) %>%
    mutate(Y2_week= ifelse(c2==0 ,-999,Y2_week)) %>%
    mice::make.where("missing")

  # Use only base line vars for mice
  mice_vars<- df %>%
    select(starts_with(c("w_","L0_","A0_"))) %>% colnames()

  # variables that get imputed only (not contributing to mice)
  imp_only_vars<- df %>%
    select(starts_with(c("L1_","A1_")), Y2, Y2_week) %>% colnames()

  # get predictor matrix----

  allVars <- names(df)

  ## names of variables with missingness
  missVars <- names(df)[colSums(is.na(df)) > 0]

  ## mice predictorMatrix
  predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
  rownames(predictorMatrix) <- allVars
  colnames(predictorMatrix) <- allVars

  imputerMatrix <- predictorMatrix
  imputerMatrix[,mice_vars] <- 1

  imputedMatrix <- predictorMatrix
  imputedMatrix[,unique(c(mice_vars,imp_only_vars))] <- 1

  ## Keep correct imputer-imputed pairs only
  predictorMatrix <- imputerMatrix * imputedMatrix
  ## Diagonals must be zeros (a variable cannot impute itself)
  diag(predictorMatrix) <- 0

  set.seed(19851111)

  m <- mice::mice(data = df,
                  predictorMatrix = predictorMatrix,
                  where = to_imp,
                  ...)

  imp_df <- mice::complete(m,"long")

  return(imp_df)

}

# Labelling data ----
get_labelled_data <- function(df){

  varlabels <- list(w_age= "Age",
                    w_sex="Sex",
                    w_educ="Educational attainment",
                    w_y0="Baseline social participation",
                    L0_inc="Household income",
                    L1_inc="Household income (2013)",
                    L0_iadl= "IADL score (2010)",
                    L1_iadl= "IADL score (2013)",
                    L0_srh= "Self-rated health (2010)",
                    L1_srh= "Self-rated health (2013)",
                    L0_mari= "Marital status",
                    L1_mari= "Marital status (2013)",
                    L0_vision= "Eye impairment",
                    L1_vision= "Eye impairment (2013)",
                    L0_hear= "Ear impairment",
                    L1_hear= "Ear impairment (2013)",
                    L0_cancer="Cancer",
                    L1_cancer="Cancer (2013)",
                    L0_heart= "Heart disease",
                    L1_heart= "Heart disease (2013)",
                    L0_stroke="Stroke",
                    L1_stroke="Stroke (2013)",
                    L0_membr= "Number of household members",
                    L1_membr= "Number of household members (2013)",
                    Y0_any= "Social participation 2010",
                    A0_teeth= "Number of teeth (2010)",
                    A1_teeth= "Number of teeth (2013)",
                    L0_den= "Denture status (2010)",
                    L1_den= "Denture status (2013)",
                    Y2= "Social participation in 2016")

  arsenal::labels(df) <- varlabels

  return(df)

}



# Missing diagnostics-----
plot_missing <- function(df, by_var= "A0_teeth",
                         x_lab= "Categories of outcome variable"){
  df %>%
    dplyr::mutate(across(contains("1_"),as.character)) %>%
    dplyr::mutate(across(contains("1_"),~ if_else(c1==0 ,
                                                  "-99" ,
                                                  .x))) %>%
    dplyr::mutate(Y2= dplyr::if_else(c2==0 ,"Censored",
                                     as.character(.[[by_var]]))) %>%
    get_labelled_data() %>%
    dplyr::select(dplyr::contains(c("teeth",
                                    "educ",
                                    "inc",
                                    "mari",
                                    "den",
                                    "srh",
                                    "y0")),!!by_var) %>%
    naniar::add_shadow(dplyr::contains(c("teeth","educ",
                                         "inc","mari",
                                         "den","srh","y0"))) %>%
    naniar::add_label_shadow(missing = "NA", complete = "!NA") %>%
    # mutate_at(vars(contains("NA")), as.character) %>%

    # mutate(any_missing= as.factor(any_missing)) %>%
    tidyr::pivot_longer(contains("_NA"), names_to = "na_var", values_to = "na_stat") %>%
    dplyr::mutate(na_var= dplyr::recode(na_var,
                                        w_educ_NA= "Educational attainment",
                                        w_y0_NA= "Baseline social participation",
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
    ggplot2::theme(text = ggplot2::element_text(size=10))+
    ggplot2::scale_fill_manual(name = 'Missing?',
                               labels = c("No", "Yes"),
                               values=c("darkblue","red"))
}


# Flow of participants ----
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


  grDevices::svg(filename = "figures/flowchart_imp.svg",
                 width = 10,
                 height = 12)

  grid::grid.newpage()

  print(baseline)
  print(cens1)
  print(followup13)
  print(cens2)
  print(followup16)

  dev.off()

}

# Table 1-------
get_table1 <- function(df,y_var,file_name){

  x<- df %>% select(contains(c("age",
                               "0_inc",
                               "sex",
                               "A0_teeth",
                               "educ",
                               "0_srh",
                               "0_mari",
                               "w_y0"))) %>% colnames()
  y <- y_var

  form <- as.formula(paste(y, paste(x, collapse="+"), sep="~"))

  df<- df %>% get_labelled_data() %>%

    mutate(across(contains("teeth"), ~fct_relevel(.x,">= 20 teeth",
                                                "10-19 teeth",
                                                "1-9 teeth",
                                                "Edentate"))) %>%
    mutate(w_educ= fct_rev(w_educ))

  if(".imp" %in% colnames(df)){
    res<- compareGroups::compareGroups(formula = form,
                                       data = df %>% filter(.imp==1),
                                       byrow = T)
  }else{
    res<- compareGroups::compareGroups(formula = form,
                                       data = df ,byrow = T)
  }

  restab <- compareGroups::createTable(res,digits = 1)

  compareGroups::export2csv(restab, file=file_name)

  return(file_name)

}

# supplementary table 1 -----
get_dropout_comparison <- function(df){

  dat <- df %>%

    mutate(follow = case_when(C1==1 & C2== 1 ~ "Remained",
                                 C1==5 | (c1==1 & C2==5) ~ "Died",
                                 (C1==3|C1==4)| (c1==1 & (C2==3|C2==4)) ~ "Became ineligible",
                                 C1==6 | (c1==1 & C2==6) ~ "Lost to follow-up")
           %>% as_factor() %>%  fct_relevel("Remained",
                                            "Became ineligible",
                                            "Died",
                                            "Lost to follow-up"
           ))

  get_table1(df= dat,
             y_var = "follow",
             file_name= "tables/dropout_comp.csv")


}


get_tmle_data <- function(df){

  cat <- df %>%
    select(-contains(c(".imp","teeth",".id")),-C1,-C2) %>%
    select_if(is_cat) %>%
    colnames()

  binary <- df %>%
    select(-contains(c(".imp","teeth",".id","c1","c2"))) %>%
    select_if(is_binary) %>%
    colnames()

  tmle_df <- df %>%
    mutate(across(contains("teeth"), fct_rev )) %>%
    mutate(across(contains("teeth"), ~factor(.x,labels = 1:4)  )) %>%
    mutate(across(binary, function(x) if_else(as.numeric(x)==2,0,1))) %>%
    fastDummies::dummy_cols(all_of(cat),
                            remove_first_dummy = T,
                            ignore_na = T,
                            remove_selected_columns = T ) %>%
    mutate(across(where(is.integer), as.numeric))

  tmle_df
}



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

# from https://rdrr.io/cran/mice/src/R/barnard.rubin.R
barnard.rubin <- function(m, b, t, dfcom = Inf) {
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
}



pool_estimates <- function(df,mi=5){
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


pool_marginal <- function(res,mi=5){

  res %>%
    pivot_longer(-imp, names_to = "d") %>%
    mutate(
      est= map_dbl(.x=value, ~.x$theta,),
           se= map_dbl(.x=value, ~.x$standard_error)) %>%

    mutate(variance= se^2) %>%
    group_by(d) %>%
    summarise(
      qbar = mean(est),
      ubar = mean(variance), # Within imputation variance
      b = var(est), # Between imputation variance
      t = ubar + (1 + 1 / mi) * b, # Total variance
      SE.combined = sqrt(t),
      df = barnard.rubin(mi, b, t), #df correction
      conf.low = qbar - qt(0.975, df)*SE.combined,
      conf.high = qbar + qt(0.975, df)*SE.combined) %>%
    dplyr::select(Scenario=d, tmle_estimate= qbar,conf.low,
                  conf.high) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), ~round(.x,digits = 3) ))

}



get_table2_data <- function(df){

  v1<- df$contrast

  v2<- paste("Observed", "vs", "Scenario", 1:8)

  v3<- set_names(x = v2,nm = v1)

  df %>%
    mutate(contrast= recode(contrast,!!!v3))

}



get_table2 <- function(df){

  flextable::set_flextable_defaults(
    font.family = "Arial" ,
    font.size = 11,
    text.align = "left",
    table.layout = "autofit",
    line_spacing= 0.9)

  # function for E-value----
  evalue <- function(theta,lo, high ) {
    e<- evalues.OR(est = theta, lo = lo, hi = high,rare = T)
    return(as.numeric(e[[2]]))
  }

  tab<- df %>%
    # mutate(contrast= recode(contrast,!!!v3)) %>%
    mutate(e= pmap_dbl(.l=list(..1=theta,..2=conf.low,..3=conf.high),
                  .f= ~ evalue(theta = ..1,lo = ..2, high = ..3))) %>%
    mutate_at(vars(theta,conf.low, conf.high,e), ~format(round(.,2),nsmall= 2)) %>%
    mutate(p.value= format.pval(round(p.value,3),digits = 3,eps = .001)) %>%
    mutate(or_ci = glue::glue("{theta} [{conf.low}-{conf.high}]")) %>%
    select(Contrast= contrast, `OR [95% CI]`= or_ci, `P value`=p.value,
           `E-value`= e) %>%
    flextable::flextable() %>%
    flextable::add_footer_lines( values =
                                   "Scenario 1= What if edentate were 1-9;
                                   Scenario 2= What if edentate were 1-9 & 1-9 were 10-19;
                                   Scenario 3= What if edentate were 1-9 & 1-9 were 10-19 & 10-19 were >=20;
                                   Scenario 4= What if everyone were >=20;
                                   Scenario 5= What if >=20 were 10-19;
                                   Scenario 6= What if >=20 were 10-19 & 10-19 were 1-9;
                                   Scenario 7= What if >=20 were 10-19 & 10-19 were 1-9 & 1-9 were edentate;
                                 Scenario 8= What if everyone were edentate")
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, value = tab)
  fileout <- "tables/table_2.docx" # write in your working directory

  print(doc, target = fileout)

}


quick_table <- function(df,file="tab"){

  flextable::set_flextable_defaults(
    font.family = "Arial" ,
    font.size = 11,
    text.align = "left",
    table.layout = "autofit",
    line_spacing= 0.9)

  tab<- df %>% flextable()
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, value = tab)
  fileout <- glue::glue("tables/{file}.docx") # write in your working directory

  print(doc, target = fileout)


}





plot_or <- function(df){

  library(ggtext)
  what_ifs <- c("edentate were 1-9",
                "edentate were 1-9 \n& 1-9 were 10-19",
                "edentate were 1-9 \n& 1-9 were 10-19 \n& 10-19 were >=20",
                "everyone had >=20 teeth",
                ">=20 were 10-19",
                ">=20 were 10-19 \n& 10-19 were 1-9",
                ">=20 were 10-19 \n& 10-19 were 1-9 \n& 1-9 were edentate",
                "everyone were edentate")
  cont <- df$contrast

  x_tics <- set_names(what_ifs,cont)
  xmin <- min(df$conf.low)
  xmax <- max(df$conf.high)
  ymax <- unique(df$contrast) %>% length()

  df %>%
    mutate( int= if_else(str_detect(contrast,
                                    paste0(1:4)),
                         "Preventive scenarios","Tooth loss scenarios" ),
            contrast= recode(contrast,!!!x_tics),
            contrast= factor(contrast),
            contrast= fct_reorder(contrast,theta)
    ) %>%
    ggplot(aes(x=theta,
               y=contrast,
               xmin=conf.low,
               xmax= conf.high,
               label= round(theta,2),
               color= int,shape=int))+
    geom_errorbar(size=0.5,
                  width = 0.15)+
    geom_point()+
    geom_text(nudge_y = 0.2,size=3,show.legend = F, color="black")+
    geom_segment(aes(x = 1 ,y=0,xend = 1, yend = ymax+0.3),
                 color="grey60", linetype="dashed")+
    xlab("Odds ratio for social participation \n(Error bars indicate 95% CI)")+
    theme_classic()+
    scale_color_manual(values= c("#030303", "#7A7A7A"))+
    theme(axis.text.y = element_text(hjust = 1),
          legend.background = element_blank(),legend.justification = "left",
          legend.position= c(0.01,0.98),
          legend.title = element_blank(),
          panel.grid.major.y = element_line(color="grey95",linetype = "dashed"),
          legend.direction = "horizontal",
          axis.title.y = element_blank())+
    annotate("text",x=xmin-0.05,y= ymax+0.3,label="What if:")+
    coord_cartesian(xlim = c(xmin,xmax),clip="off")

  ggsave("figures/figure_3.pdf",device = "pdf",width = 6, height = 7.5,dpi = 900)

}




round_uc <- function(data){

  data %>%
    mutate_at(vars(theta,conf.low, conf.high), ~format(round(.,2),nsmall= 2)) %>%
    mutate(`P value`= format(round(p.value,3),nsmall= 3),
           est_ci = glue::glue("{theta} [{conf.low}-{conf.high}]")) %>%

    dplyr::select(contrast,
                  `OR [95% CI]`= est_ci,
                  `P value`)
}


