# Function to create reuseable case_when logic

create_case_when <- function(..., vars = "x") {
  fun_fmls <- purrr::map(rlang::set_names(vars), ~ rlang::missing_arg())
  fun_body <- substitute({
    for (name in var) {
      symb <- rlang::eval_bare(rlang::sym(name))
      var <- rlang::eval_tidy(rlang::enquo(symb))
      assign(name, var)
    }
    forms <- purrr::map(formulas, rlang::`f_env<-`, value = environment())
    do.call(dplyr::case_when, forms)
  })
  formulas <- rlang::dots_list(...)
  var <- vars
  structure(
    rlang::new_function(fun_fmls, fun_body),
    class = c("case_when", "function")
  )
}



# Function to plot categorical varaibles

plot_cat_vars <- function(df, fill_var, cat_vars, ncol=2){

  quo_fill <- rlang::sym(fill_var)

  df%>%
    select(!!quo_fill, all_of(cat_vars)) %>%
    mutate_all(as.factor) %>%
    pivot_longer(2: last_col()) %>%
    ggplot(aes(y = value, fill = !!quo_fill)) +
    geom_bar(position = "fill") +

    facet_wrap(vars(name), scales = "free", ncol = ncol) +
    scale_x_continuous(labels= scales::label_percent())+
    labs(x = NULL, y = NULL)
}


sum_x <- function(x){

  val <- sum(is.na(x))
  perc <- (sum(is.na(x))/length(x))*100

  return(glue("{val)} [{round(perc,1)}%]"))
}


# sort( apply( df[,],
#              2,
#              sum_x),
#       decreasing = TRUE ) %>% bind_rows() %>%
#   pivot_longer(cols = colnames(.), names_to = "var")


# boxplot with x-axis categorical y-axis numerical

plot_num_vars <- function(df, num_var, x_var){

  quo_num <- rlang::sym(num_var)
  quo_x <- rlang::sym(x_var)

  df%>%
    select(all_of(x_var), all_of(num_var)) %>%
    ggplot(aes(as.factor(!!quo_x), !!quo_num, fill = !!quo_x)) +
    geom_boxplot(alpha = 0.4, show.legend = FALSE) +
    labs(x = NULL, y = quo_num)
}





formulas <- function(x, ...) UseMethod("formulas")

formulas.case_when <- function(x, ...) get("formulas", envir = environment(x))

print.case_when <- function(x, ...) {
  formulas <- formulas(x)
  n <- length(formulas)
  out <- capture.output(purrr::walk(formulas, print, showEnv = FALSE))
  out <- c(crayon::cyan("<CASE WHEN>"),
           crayon::magenta(paste(n, "conditions:")),
           crayon::green(paste("->", out)), "")
  cat(paste0(out, collapse = "\n"))
  invisible(x)
}

# ==============================================================================
eqincome <- create_case_when(is.na(x)~x,
                         x ==1~ 0.25,
                         x ==2~ 0.75,
                         x ==3~ 1.25,
                         x ==4~ 1.75,
                         x ==5~ 2.25,
                         x ==6~ 2.75,
                         x ==7~ 3.50,
                         x ==8~ 4.50,
                         x ==9~ 5.50,
                         x ==10~ 6.50,
                         x ==11~ 7.50,
                         x ==12~ 8.50,
                         x ==13~ 9.50,
                         x ==14~ 11.00,
                          TRUE~ 13.00,
                         vars = "x")

teeth10clean <- create_case_when(is.na(x)~x,
                           x==1 ~4,
                           x==2 ~3,
                           x==3 ~2,
                           x==4 ~1,
                           vars = "x")

teeth13_16 <- create_case_when(is.na(x)~x,
                                 x==3 ~2,
                                 x==4 ~3,
                                 x==5 ~4,
                                 TRUE ~ x,
                                 vars = "x")


# IADL logic




# =============================================================================
# To update an Rdata file

resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}





# Get a stata like summary for all variables

mysum <- function(data) {

  ins <- function(x) {
    min=min(x,na.rm = TRUE)
    max=max(x,na.rm = TRUE)
    Na=sum(is.na(x))
    Non_NA=sum(!is.na(x))
    out=c(min, max,Na, Non_NA)}

  data %>% as.data.frame() %>%  summarise_all(~ins(.x)) %>%
  `row.names<-`(c("min","max","Na's","Non_NA")) %>%
    tibble::rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from=rowname, values_from=value) %>%  print(n = 50)
}


# ------------------------------------------------------------------------------

tableby_to_flextable <- function(tblby,
                                 header_row= "add span row title",
                                 colwidths= c(1,4) ){

  flextable::set_flextable_defaults(
    font.family = "Times New Roman",
    font.size = 11,
    table.layout = "autofit",
    line_spacing= 0.9)

  df<- tblby %>% summary(text=T) %>% as.data.frame()

  colnames(df)[1] <-  "Characteristics"


  indent_cols <- which(startsWith(df$Characteristics, "- "))


  df <- df %>% dplyr::mutate(Characteristics = stringr::str_remove_all(.data$Characteristics, "- "))



  df %>% flextable::flextable() %>%

    flextable::padding(i = indent_cols, j = 1, padding.left = 15, part = "body") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::bold(~ !startsWith(Characteristics, " "), ~Characteristics) %>%
    flextable::add_header_row(values = c("", header_row),
                              colwidths = colwidths) %>%
    flextable::align(i = 1, align = "center", part = "header") %>%
    flextable::hline_top(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body") %>%
    flextable::hline(i=1,j=1, border = officer::fp_border(width = 0), part = "header") %>%
    flextable::merge_v(j=1,part = "header") %>%
    flextable::set_header_labels(Characteristics = "Baseline characteristics") %>%
    flextable::footnote(i = 2, j = 1,
                        value = flextable::as_paragraph("Mean (SD) for continuous variables; Frequency (%) for categorical variables"),
                        ref_symbols = c("1"),
                        part = "header")
}






