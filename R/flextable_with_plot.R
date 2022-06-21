library(targets)
library(EValue)
library(tidyverse)
library(flextable)


df <- tar_read(results_combined)

get_table2 <- function(df, estimator="sl"){

  flextable::set_flextable_defaults(
    font.family = "Arial" ,
    font.size = 11,
    text.align = "left",
    table.layout = "autofit",
    line_spacing= 0.9)

  # function for E-value
  evalue <- function(theta,lo, high ) {
    e<- evalues.OR(est = theta, lo = lo, hi = high,rare = T)
    return(as.numeric(e[[2]])  %>%
             format(x=round(x = .,2),nsmall=2))
  }


  tab<- df %>% dplyr::filter(est== estimator) %>%
    mutate(Estimand= str_replace(Estimand,"d0", "Observed"),
           Estimand= str_replace(Estimand,"d1", "Edentate"),
           Estimand= str_replace(Estimand,"d2", "1-9 teeth"),
           Estimand= str_replace(Estimand,"d3", "10-19 teeth"),
           Estimand= str_replace(Estimand,"d4", ">=20 teeth"),
           Estimand= str_replace_all(Estimand,"_", " ")
           ) %>%
    arrange(theta) %>%
    select(Estimand,theta,conf.low, conf.high) %>%
    mutate_at(vars(-Estimand),as.numeric) %>%
    mutate(e= pmap(.l = list(.$theta,.$conf.low,.$conf.high),
                   ~ evalue(theta = .x,lo = .y, high = ..3))) %>%
    mutate_if(is.numeric, ~format(round(.,2),nsmall= 2)) %>%
    mutate(or_ci = glue::glue("{theta} [{conf.low}-{conf.high}]")) %>%
    mutate(e= as.numeric(e)) %>%
    select(Contrast= Estimand, `OR [95% CI]`= or_ci, `E-value`= e) %>%
    flextable::flextable()

  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, value = tab)
  fileout <- "tables/table_2.docx" # write in your working directory

  print(doc, target = fileout)


}

get_table2(df)


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

    group_by(y,theta) %>%
    ggplot(aes(x=theta, y=y, color=est,xmin=conf.low,xmax= conf.high))+
    scale_color_manual(aesthetics = "color",values = c("grey70", "black"),
                       name="Estimator: ")+
    geom_errorbar(position=ggstance::position_dodgev(height=0.5),
                  size=0.5,
                  width = 0.2)+
    geom_point(position=ggstance::position_dodgev(height=0.5))+
    geom_segment(aes(x = 1 ,y=0,xend = 1, yend = 8.3),
                 color="grey60", linetype="dashed")+
    xlab("Odds Ratio (error bars indicate 95% confidence intervals)")+
    theme_classic()+
    theme(legend.position= c(0.4,0.99),
          panel.grid.major.y = element_line(color="grey95"),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          axis.title.y = element_blank())

    ggsave("figures/figure_2.pdf",device = "pdf",width = 8, height = 9,dpi = 900)


}


get_figure2(df)

