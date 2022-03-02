library(targets)
library(EValue)
library(tidyverse)
library(flextable)


df <- tar_read(results_df)


get_results_tables <- function(df,
                               file_name="tables/table_2",
                               out_type= "pdf"){

  # function for plot
  point_range <- function(df){

    ggplot(data = df , aes(x=df$theta,  y=df$Estimand,
                           xmin = df$conf.low, xmax= df$conf.high))+
      geom_point(color="blue")+
      geom_errorbarh(height=0.1, size=0.2) +
      geom_vline(xintercept = 1, linetype = "longdash",
                 colour = "grey50",size=.2)+
      scale_x_continuous(limits=c(0.9, 1.45))+
      theme_void()+
      theme(panel.border = element_blank())
  }
  # function for E-value
  evalue <- function(x) {

    e<- evalues.OR(est = x[[1]], lo = x[[2]], hi = x[[3]],rare = T)
    return(e[[2]])
  }

  grp <- c(1:nrow(df))

  temp <- data.frame(grp,df) %>% mutate_at(vars(-Estimand),as.numeric)

  # Adding E-value ----
  df_e <- temp %>% select(grp,theta,conf.low, conf.high) %>% group_by(grp) %>% nest()

  Eval<- df_e %>% mutate(E= map_dbl(data, evalue)) %>% pull(E) %>% round(2)

  # Adding point range ----
  df_nest <- temp %>% group_by(grp) %>% nest()

  z <- df_nest %>% mutate(plot = map(data, ~point_range(.x)))

  p<- left_join(z %>% ungroup(),temp) %>%
    select(-data,-grp) %>%
    mutate(or_ci = glue::glue("{theta} [{conf.low}-{conf.high}]"),
           `E-value` = Eval) %>%
    select(Estimand, or_ci, `E-value`, plot)

  # Creating table ----


  flextable::set_flextable_defaults(
    font.family = "Arial" ,
    font.size = 10,
    # table.layout = "autofit",
    line_spacing= 0.9)


  tab <- p %>%
    flextable(cwidth = c(0.5,1.5,0.5,2)) %>%
    set_header_labels(Estimand = "Contrast", or_ci= "OR [95% CI]", plot=" ") %>%
    mk_par(
      j = 4,
      value = as_paragraph(gg_chunk(value = plot
                                    , height = 0.2, width = 2
                                    ))) %>%
    flextable::align( align = "left", part = "all") %>%
    flextable::align( j=4, align = "left", part = "all")  %>%
    border_remove() %>%
    hline(i=1, j=1:3,part = "header") %>%
    hline_top(j=1:3,part = "header") %>%
    hline_bottom(j=1:3)

  if ("pdf" %in% out_type){
    return(tab)
  }else{
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, value = tab)
    fileout <- tempfile(fileext = "/.docx")
    fileout <- glue::glue({file_name},".docx") # write in your working directory

    print(doc, target = fileout)

  }

}




point_range(temp)

get_results_tables(df, out_type = "word")

# p %>%
#   flextable(col_keys = c("Contrast", "OR [95% CI]", "E-value", " "),
#             cwidth = c(1,1.75,1,2)) %>%
#   merge_at(j = 4,
#            i = c(1:4)) %>%
#   border_remove() %>%
#   hline(i=1, j=1:3,part = "header") %>%
#   hline_top(j=1:3,part = "header") %>%
#   hline_bottom(j=1:3) %>%
#   compose(
#     j = 4,
#     value = as_paragraph(as_image(src="test.png",width = 2, height = 2.2, unit = "in")), part="body"
#   )
#
