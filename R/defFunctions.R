#create apa style corrlation tables
#use: xtable(corstarsl(swiss[,1:4]))
corstarsl <- function(x){
        require(Hmisc)
        x <- as.matrix(x)
        R <- rcorr(x)$r
        p <- rcorr(x)$P

        ## define notions for significance levels; spacing is important.
        mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))

        ## trunctuate the matrix that holds the correlations to two decimal
        R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

        ## build a new matrix that includes the correlations with their apropriate stars
        Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
        diag(Rnew) <- paste(diag(R), " ", sep="")
        rownames(Rnew) <- colnames(x)
        colnames(Rnew) <- paste(colnames(x), "", sep="")

        ## remove upper triangle
        Rnew <- as.matrix(Rnew)
        Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
        Rnew <- as.data.frame(Rnew)

        ## remove last column and return the matrix (which is now a data frame)
        Rnew <- cbind(Rnew[1:length(Rnew)-1])
        return(Rnew)
}

# from https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE, repos = "http://cran.r-project.org")
    sapply(pkg, require, character.only = TRUE)
}
# usage
#packages <- c("ggplot2",  "gplots", "lattice", "plyr", "reshape2",
#              "RColorBrewer", "grid", "gridExtra", "igraph", "igraphdata")
#suppressMessages(ipak(packages))

#report ttests
t.report <- function(tt){
  tvalue <- tt$statistic %>% formatC(digits = 2, format = "f")
  pvalue <- tt$p.value %>% formatC(digits = 2, format = "f")
  if (round(tt$parameter, 0) == tt$parameter) {
    df <- tt$parameter
  } else {
    df <- as.numeric(formatC(tt$parameter, digits = 2, format = "f"))
  }
  if (tt$p.value < 0.001) {
    pvalue <- " < .001"
  } else {
    if (tt$p.value < 0.01) {
      pvalue <- " < .001"
    } else {
      if (tt$p.value < 0.05) {
        pvalue <- " < .05"
      } else {
        pvalue <- gsub("0\\.","\\.",paste0(" = ",as.character(tt$p.value %>% formatC(digits = 2, format = "f"))))
      }
    }
  }
  cohensD <- round(tt$statistic/sqrt(df+1),2)
  paste0("*t*(",df,") = ",tvalue, ", *p*", pvalue, ", *d* = ", cohensD)
  }

#report beta coefficients
coeff.report <- function(b, p){
        betavalue <- b %>% formatC(digits = 2, format = "f")
        pvalue <- p %>% formatC(digits = 2, format = "f")

        if (p < 0.001) {
                pvalue <- " < .001"
        } else {
                if (p < 0.01) {
                        pvalue <- " < .001"
                } else {
                        if (p < 0.05) {
                                pvalue <- " < .05"
                        } else {
                                pvalue <- paste0(" = ", gsub(pattern = "0\\.", replacement = "\\.", as.character(p %>% formatC(digits = 2, format = "f"))))
                        }
                }
        }
        paste0("*$\\beta$* = ",betavalue, ", *p*", pvalue)
}

#report pvalues
pvalue.report <- function(p){
        pvalue <- p %>% formatC(digits = 2, format = "f")
        if (p < 0.001) {
                pvalue <- " < .001"
        } else {
                if (p < 0.01) {
                        pvalue <- " < .001"
                } else {
                        if (p < 0.05) {
                                pvalue <- " < .05"
                        } else {
                                pvalue <- paste0(" = ", gsub(pattern = "0\\.", replacement = "\\.", as.character(p %>% formatC(digits = 2, format = "f"))))
                        }
                }
        }
        paste0("*p*", pvalue)
}

pvalue.correct <- function(p){
  library(stringr)
  library(dplyr)
  p <- p %>% as.numeric
  pvalue = case_when(p < 0.001 ~ "< .001",
                     p < 0.01 ~ "< .01",
                     p < 0.05 ~ "< .05",
                     TRUE ~ p %>% round(2) %>% formatC(digits = 2, format = "f") %>% as.character() %>% str_replace("0.", ".")
                     )

    # if (p < 0.001) {
  #   pvalue <- "< .001"
  # } else {
  #   if (p < 0.01) {
  #     pvalue <- "< .001"
  #   } else {
  #     if (p < 0.05) {
  #       pvalue <- "< .05"
  #     } else {
  #       pvalue <- pvalue  %>% round(2) %>% formatC(digits = 2, format = "f") %>% as.character() %>% str_replace("0.", ".")
  #     }
  #   }
  # }
  return(pvalue)
}
#install and require a package
#use: packages(Hmisc)
# packages<-function(x, repos="http://cran.r-project.org", ...){
#         x <- deparse(substitute(x))
#         if (!require(x,character.only=TRUE)){
#                 install.packages(pkgs=x, repos=repos, ...)
#                 require(x,character.only=TRUE)
#         }
# }

#ggplot2 APA-style theme
suppressMessages(ipak("ggplot2"))
windowsFonts(Times=windowsFont("TT Times New Roman"))
#modified version of papaja's theme_apa()
theme_apa <- function (base_size = 14, base_family = "", box = FALSE)
{
        adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
                ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.1),
                                                                  margin = ggplot2::margin(0, 0, ggplot2::rel(14),
                                                                                           0)), axis.title = ggplot2::element_text(size = ggplot2::rel(1.1)),
                               axis.title.x = ggplot2::element_text(margin = ggplot2::margin(ggplot2::rel(18),
                                                                                             0, 0, 0)), axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,
                                                                                                                                                                      ggplot2::rel(18), 0, 0)), axis.ticks.length = ggplot2::unit(ggplot2::rel(6),
                                                                                                                                                                                                                                  "points"), axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
                               axis.text.x = ggplot2::element_text(margin = ggplot2::margin(ggplot2::rel(6),
                                                                                            0, 0, 0)), axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0,
                                                                                                                                                                    ggplot2::rel(8), 0, 0)), axis.line.x = ggplot2::element_line(),
                               axis.line.y = ggplot2::element_line(), legend.title = ggplot2::element_text(),
                               legend.key = ggplot2::element_rect(fill = NA, color = NA),
                               legend.key.width = ggplot2::unit(ggplot2::rel(20),
                                                                "points"), legend.key.height = ggplot2::unit(ggplot2::rel(25),
                                                                                                             "points"), legend.spacing = ggplot2::unit(ggplot2::rel(18),
                                                                                                                                                      "points"), panel.spacing = ggplot2::unit(ggplot2::rel(16),
                                                                                                                                                                                              "points"), panel.grid.major.x = ggplot2::element_line(size = NA),
                               panel.grid.minor.x = ggplot2::element_line(size = NA),
                               panel.grid.major.y = ggplot2::element_line(size = NA),
                               panel.grid.minor.y = ggplot2::element_line(size = NA),
                               strip.background = ggplot2::element_rect(fill = NA,
                                                                        color = NA), strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.1),
                                                                                                                          margin = ggplot2::margin(0, 0, ggplot2::rel(16),
                                                                                                                                                   0)), strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.1),
                                                                                                                                                                                             margin = ggplot2::margin(0, 0, 0, ggplot2::rel(16))))
        if (box) {
                adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
        }
        else {
                adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
        }
        adapted_theme
}
#report cocor's different of correlations
cor_diff_report <- function(cor_p){
        pvalue <- pvalue.report(cor_p$fisher1925$p.value)
        cohensQ <- round(fisherz(cor_p$fisher1925$estimate[1])-fisherz(cor_p$fisher1925$estimate[2]),2)
        return(paste0("*z* = ",round(cor_p$fisher1925$statistic,2),", ", pvalue, ", *Cohen's q* = ", cohensQ))
}
# for GGally ggdou
lm_with_cor <- function(data, mapping, ..., method = "pearson") {
        x <- data[[deparse(mapping$x)]]
        y <- data[[deparse(mapping$y)]]
        cor <- cor(x, y, method = method)
        ggally_smooth_lm(data, mapping, ...) +
                ggplot2::geom_label(
                        data = data.frame(
                                x = min(x, na.rm = TRUE),
                                y = max(y, na.rm = TRUE),
                                lab = round(cor, digits = 3)
                        ),
                        mapping = ggplot2::aes(x = x, y = y, label = lab),
                        hjust = 0, vjust = 1,
                        size = 5, fontface = "bold"
                )
}
#report var.test's different of variances
var_diff_report <- function(var_d){
        pvalue <- pvalue.report(var_d$p.value)
        DFnum <- var_d$parameter[["num df"]]
        DFdenom <- var_d$parameter[["denom df"]]
        return(paste0("*F*$\\textsubscript{(",DFnum,",",DFdenom,")}$ = ",round(var_d$statistic,2),", ", pvalue))
}
#report chi-square different of proportions
chi_prop_diff_report <- function(var_d){
        pvalue <- pvalue.report(var_d$p.value)
        DFnum <- var_d$parameter[["num df"]]
        DFdenom <- var_d$parameter[["denom df"]]
        return(paste0("*F*$\\textsubscript{(",DFnum,",",DFdenom,")}$ = ",round(var_d$statistic,2),", ", pvalue))
}


multi.tests <- function(fun = t_test, df, vars, group.var, ...) {
  require(apa)
    sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
           vars,                                                # loop on vector of outcome variable names
           function(var) {
               formula <- as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
               fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
           }
    )
}

#To be used with the 'apa' package, on the results of t_test()
apa.desc <- function(t_test, x){
  group <- as.character(x)
  group_data <- t_test$data[[group]]
  mean <- mean(group_data, na.rm = T)
  sd <- sd(group_data, na.rm = T)
  return(paste0("(*M* = ",round(mean,2),", *SD* = ", round(sd,2),")"))
}

bayesvalues <- function(){
  BFvalues <- data_frame(BFxy = c(">100" , "30-100" , "10-30" , "3-10" , "1-3" , "1" , "0.3-1" , "0.1-0.3" , "0.03-0.1" , "0.01-0.03" , "<0.01"),
                         Interpretation = c("Decisive for X" , "Very strong for X" , "Strong for X" , "Moderate for X" , "Ancedotal for X" , "No evidence" , "Ancedotal for Y" , "Moderate for Y" , "Strong for Y" , "Very strong for Y" , "Decisive for Y")
  )
  BFvalues
}

fitvalues <- function(){
  print("Taken from http://www.sicotests.com/psyarticle.asp?id=277")
  print("Fit is good if:")
  print("NFI > .90 (Byrne, 1994) or .95 (Schumacker & Lomax, 2004)")
  print("GFI > .90 (Byrne, 1994)")
  print("CFI > .93 (Byrne, 1994)")
  print("RMSEA < .08 (Browne & Cudeck, 1993), ideally RMSEA < .05 (Stieger, 1990).")
  print("RMSEA upper confidence interval < .08 (Hu & Bentler, 1998)")
}


cor.bf <- function(data) {
  data      <- na.omit(data)
  x.name    <- names(data)[1]
  y.name    <- names(data)[2]
  cor_obj   <- cor.test(as.numeric(data[[1]]),as.numeric(data[[2]]))
  n_obj     <-  min(length(na.omit(data[,1])), length(na.omit(data[,2])))
  bf_obj    <- jzs_cor(data[[1]], data[[2]])
  return(list(cor = cor_obj, n = n_obj, bf = bf_obj))
}

omit.na <- na.omit

quick.csv <- function(x) {
  name <- deparse(substitute(x))
  write_csv(x, path = paste0(name, ".csv"), na = "")
}

flipPath <- function(text) {   # flip back-slash to double slahes, and vice-versa
        changes <- as.integer(gregexpr("[/\\]", text)[[1]])
        if (length(changes) == 1 && changes == -1) {
                return(text)
        }
        else {
                replacement <- strsplit(text, "")[[1]]
                for (pos in changes) {
                        replacement[pos] <- ifelse(replacement[pos] == "/",
                                                   "\\", "/")
                }
                return(paste0(replacement, collapse = ""))
        }
}

exploreWD <- function() { # Explore the current working directroy in Windows Explorer
        wd <- flipPath(getwd())
        shell(paste("explorer", wd, sep = " "), intern = TRUE)
}
