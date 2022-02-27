

res <- targets::tar_read(tmle_contrast_k0)


get_or_plot <-  function(data, table_text_size= 3, title_test_size= 11,
                         add_g= NULL, add_t= NULL,
                         title_xcord= 0.03, title_ycord= 0.2,
                         column_space= c(-0.1, 0.05, 0.1)){

  library(magrittr, quietly = T)
  library(extrafont)
  loadfonts(device = "win")

  tmp<- data %>%
    dplyr::mutate(estimand = forcats::fct_reorder(estimand, dplyr::desc(theta)) %>%
                    forcats::fct_recode("Observed vs Edentate" = "observed_vs_edentulous",
                                        "Observed vs 1-9 teeth" = "observed_vs_`1_to_9`",
                                        "Observed vs 10-19 teeth" = "observed_vs_`10_to_19`",
                                        "Observed vs <=20 teeth" = "observed_vs_`20_or_more`",
                                        "Observed vs One step down" = "observed_vs_one_stepdown",
                                        "Observed vs Two steps down" = "observed_vs_two_stepdown"
                    )) %>%
    dplyr::mutate(size= abs(ref-shift)) %>%
    dplyr::mutate(or_ci= stringr::str_c("OR=", round(theta,2),
                                        " [95% CI:",
                                        format(conf.low,digits=2),
                                        "-",
                                        round(conf.high,digits=2),
                                        ", p=",
                                        format(p.value),

                                        "]"))


  g1 = ggplot2::ggplot(tmp, ggplot2::aes(x = as.numeric(theta), xmin = as.numeric(conf.low), xmax  = as.numeric(conf.high),
                                         y = estimand))+
    ggplot2::geom_point(ggplot2::aes(size= size),shape=22, fill="darkblue")+
    ggplot2::geom_errorbarh(height=0.1, size=0.1) +
    ggplot2::geom_vline(xintercept = 1, linetype = "longdash", colour = "black")+
    # scale_x_continuous(trans="log10", breaks= breaks)+
    ggplot2::xlab("Odds ratio for social participation in 2016 (95% CI)")+
    ggplot2::theme_classic(14)+
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 9),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   text=ggplot2::element_text(family="serif"),
                   legend.position="none")


  t1 = ggplot2::ggplot(tmp, ggplot2::aes(x = as.numeric(theta), y = estimand))+
    ggplot2::annotate("text", x = column_space[1], y = tmp$estimand,
                      label=tmp[,"estimand"], hjust=0, size=table_text_size)+

    ggplot2::annotate("text", x = column_space[2], y = tmp$estimand,
                      label=tmp[,"or_ci"], hjust=1, size=table_text_size)+

    # annotate("text", x = column_space[1], y = 6.5,
    #          label="Contrast of estimates", hjust=0, size=7)+
    # annotate("text", x = column_space[3], y = tmp$estimand, label=res[,7],hjust=1, size=table_text_size)+
    ggplot2::theme_classic(14)+
    ggplot2::theme(axis.title.x = ggplot2::element_text(colour = "white"),
                   axis.text.x = ggplot2::element_text(colour = "white"),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   text=ggplot2::element_text(family="serif"),
                   line = ggplot2::element_blank())

  g1 <-  g1+ add_g
  t1 <-  t1+ add_t

  ge<- gridExtra::grid.arrange(t1, g1, ncol=2, widths = c(3,2),
                          top=grid::textGrob("Contrast of TMLE estimates for social participation in 2016: OR,95% CI, p-value ", x=title_xcord, y=title_ycord,
                                             gp=grid::gpar(fontsize=title_test_size), just="left"))
  return(ge)

}

g <- get_or_plot(res)


ggplot2::ggsave(plot = g,filename = "test.pdf", width = 20, height = 13, units = "cm")

