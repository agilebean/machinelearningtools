# create grouped data for group comparisons
summarise_wins <- function(result_data, max_wins, grouping) {

  result_data %>%
    group_by(across(all_of(grouping))) %>%
    dplyr::summarise(
      mean = mean(wins / max_wins),
      median = median(wins / max_wins),
      sd = sd(wins / max_wins),
      se = sd / sqrt(n()),
      n = n()
    )
}

summary_stats <- function(
  data_set, grouping_labels, dv_label, remove_data = TRUE) {

  dv <- rlang::sym(dv_label)

  data_set %>%
    nest(data = !any_of(grouping_labels)) %>%
    dplyr::mutate(
      ci = map(data, ~ DescTools::MeanCI(.x[[dv]])),
      var = map(data, ~ var(.x[[dv]])),
      sd = map(data, ~ sd(.x[[dv]])),
      se = map(data, ~ sd(.x[[dv]]) / sqrt(nrow(.x))),
      n = map(data, ~ nrow(.x))
    ) %>%
    unnest(c(var, sd, se, n)) %>%
    unnest_wider(ci) %>%
    {
      if (remove_data) {
        select(., -data)
      } else {
        .
      }
    } %>%
    arrange(across(grouping_labels))
}

# assumes data_object is nested into a data column
perform_aov <- function(data_object, formula_aov) {

  response.label <- all.vars(formula_aov)[1]
  predictor.label <- all.vars(formula_aov)[2]
  predictor <- data_object$data %>% pluck(1) %>% .[[predictor.label]]

  data_object %>%
    dplyr::mutate(
      aov = map(data, ~ aov(formula_aov, data = .x)),
      lm = map(data, ~ lm(formula_aov, data = .x)),
      glanced = map(lm, broom::glance), # aov doesn't yield "statistic"
      tidied = map(aov, broom::tidy),
      # CIs on DV extracted from formula
      ci = map(data, ~ MeanCI(.x[[response.label]])),
      se = map(data, ~ sd(.x[[response.label]]) / sqrt(nrow(.x)))
    ) %>%
    {
      if (class(predictor) == "factor") {
        # posthoc scheffe shows which mean differences are significant
        scheffe = map(aov, ~ DescTools::ScheffeTest(.x)) # only on factor
      } else {
        .
      }
    }
}

test_non_parametric <- function(data_object, formula_nonparam) {

  require(DescTools)
  require(rstatix)

  response <- formula_nonparam %>% all.vars() %>% .[1]
  group <- formula_nonparam %>% all.vars() %>% .[2]

  data_object %>%
    dplyr::mutate(
      # Kruskal-Wallis test: non-parametric alternative to one-way ANOVA
      # uses sample medians instead of means
      kruskaled = map(data,
        ~ kruskal.test(formula_nonparam, data = .x) %>%
          broom::glance(.) %>%
          dplyr::rename(df = parameter)),
      # wilcox does not correct for multiple comparisons with pooled variance
      wilcoxed = map(data, ~ pairwise.wilcox.test(x = response, g = group)),
      # dunn better than wilcox
      # remove DescTools::DunnTest bec no df and precision limited to p < 2e-16
      # dunned = map(data, ~ DescTools::DunnTest(formula_nonparam, data = .x)),
      dunned = map(data, ~ rstatix::dunn_test(formula_nonparam, data = .x))
    )
}


test_smirnov <- function(data_object, score_var = "wins", cum_fun = "pnorm") {

  data_object %>%
    dplyr::mutate(
      # Kolmogorov-Smirnov test
      smirnoved = map(data, ~ .x[[score_var]] %>%
                        ks.test(., y = cum_fun, mean = mean(.), sd = sd(.)) %>%
                        broom::glance(.))
    )
}


test_normality <- function(data_object, formula) {

  response <- all.vars(formula)[1]

  data_object %>%
    test_smirnov(score_var = response) %>%
    dplyr::mutate(
      # Shapiro-Wilk test: preferably on residuals than DV
      # Source: https://psychometroscar.com/2018/07/11/normality-residuals-or-dependent-variable/
      shapiroed = map(aov, ~ .x %>%
                        residuals %>%
                        shapiro.test() %>%
                        broom::glance(.)),
      shapiroed.dv = map(data, ~ .x[[response]] %>%
                         shapiro.test() %>%
                         broom::glance(.)),

      # Welch test is a form of ANOVA that allows for heterogeneity
      welched = map(
        data, ~ oneway.test(formula, data = .x, var.equal = FALSE) %>%
          broom::glance(.))
    )
}


test_homogeneity <- function(data_object, formula) {

  require(onewaytests) # # Brown-Forsythe Test
  require(DescTools)

  predictor.label <- all.vars(formula)[2]
  predictor <- data_object$data %>% pluck(1) %>% .[[predictor.label]]

  data_object %>%
    dplyr::mutate(
      # Bartlett Test of Homogeneity of Variances BUT only one-way ANOVA!
      bartletted = map(data, ~ bartlett.test(formula, data = .x) %>%
                         broom::glance(.)),

      # Fligner-Killeen Test of Homogeneity of Variances
      flignered = map(data, ~ fligner.test(formula, data = .x) %>%
                        broom::glance(.)),

    ) %>%
    {
      if (class(predictor) == "factor") {
        dplyr::mutate(
          # very tricky: for inline conditions, need explicit LHS!
          .,
          # Levene Test: homogeneious if p > 0.05, works only on factor
          levened = map(data, ~ DescTools::LeveneTest(formula, data = .x)),

          # Brown-Forsythe Test
          brownforsythe = map(data, ~ bf.test(formula, data = .x)),
          brownforsythed = map(
            brownforsythe,
            ~ tibble(statistic = .x$statistic, p.value = .x$p.value)
          )
        )

      } else {
        .
      }
    }



}

test_independence <- function(data_object, model_label = "aov") {

  require(DescTools)
  model <- rlang::sym(model_label)
  data_object %>%
    dplyr::mutate(
      # independent samples test
      durbined = map(!!model, ~ DescTools::DurbinWatsonTest(.x) %>%
                     broom::glance(.))
    )
}


create_plots_lm  <- function(data_object, model_label = "aov") {

  require(gglm)
  model <- rlang::sym(model_label)
  data_object %>%
    dplyr::mutate(
      plot.residuals = map(!!model, ~ ggplot(data = .x) + stat_fitted_resid() ),
      plot.qq = imap(!!model, ~ ggplot(data = .x) +
                      stat_normal_qq(alpha = 0.2) +
                      labs(
                        title = paste(.y),
                        x = "", y = ""
                      )
                    ),
      plot.leverage = map(!!model, ~ ggplot(data = .x) + stat_resid_leverage() ),
      plot.hist  = map(!!model, ~ ggplot(data = .x) + stat_resid_hist() ),
      plot.scale = map(!!model, ~ ggplot(data = .x) + stat_scale_location() ),
    )
}


print_stats <- function(data_set,
                        stat_type,
                        grouping = NULL,
                        param_var = "parameter",
                        save_label = "",
                        format = "html",
                        kable = FALSE,
                        latex = FALSE,
                        digits = 2,
                        ...) {

  require(dplyr)

  result.table <- data_set %>%
    purrr::when(
      stat_type == "levened" ~ {

        unnest(., levened) %>%
          select(param_var, grouping, F.levene = `F value`, p.levene = `Pr(>F)`) %>%
          filter(!is.na(F.levene))

      },
      stat_type == "glanced" ~ {

        unnest(., c(glanced, se)) %>%
          unnest_wider(ci) %>%
          select(param_var, grouping,
                 mean, se, lwr.ci, upr.ci, # MeanCI
                 F.anova = statistic, p.anova = p.value # glanced
          )

      },
      stat_type == "shapiroed" ~ {

        unnest(., shapiroed) %>%
          select(param_var, grouping, W = statistic, p.shapiro = p.value)

      },
      stat_type == "kruskaled" ~ {

        unnest(., kruskaled) %>%
          select(param_var, grouping, K = statistic, p.kruskal = p.value)

      },
      stat_type == "dunned" ~ {

        unnest(., dunned) %>%
          mutate(`eta-squared` = statistic^2 / (n1 + n2)) %>%
          select(param_var, grouping, group1, group2,
                 z = statistic, `eta-squared`,
                 p, p.adj, p.adj.signif
                 ) %>%
          unite("groups", c(group1, group2), sep = "~")

      },
      stat_type == "durbined" ~ {

        unnest(., durbined) %>%
          select(param_var, grouping, autocorrelation,
                 dw = statistic, p.durbin = p.value)

      },
      stat_type == "tidied" ~ {

        unnest(., tidied) %>%
          select(param_var, term, grouping, F = statistic, p.value)

      },
      TRUE ~ {
        # covers bartletted, flignered, brownforsythed
        unnest(., stat_type, names_repair = "minimal") %>%
          select(param_var, grouping, statistic, p.value)
      }
    ) %>%
    {
      if (!is.null(grouping)) {

        dplyr::arrange(., param_var, grouping)
      } else {
        .
      }
    }

  if (kable) {

    result.table %>%
      convert_kable(., digits = digits) %>%
      {
        if (save_label != "") {

          cat(., file = paste0(save_label, ".", format))

        } else {
          .
        }
      }
  }

  if (latex) {

    result.table %>%
      convert_latex(., digits = digits) %>%
      {
        if (save_label != "") {

          require(stargazer)
          stargazer(., out = paste0(save_label, ".tex"))

        } else {
          .
        }
      }
  }

  result.table

}

convert_kable <- function(data, digits = 4, format = "html", ...) {

  require(knitr)
  require(kableExtra)

  data %>%
    knitr::kable(format = format, digits = digits) %>%
    kableExtra::kable_styling(bootstrap_options = c("bordered", "hover")) %>% print

}

convert_latex <- function(data, digits = 4, ...) {

  require(xtable)
  data %>%
    xtable(digits = digits, ...) %>% print

}

analyze_aov <- function(
  data_object, nesting_labels, formula_aov, score_var = "score",
  test_independence = TRUE,
  test_homogeneity = TRUE,
  test_normality = TRUE,
  create_plots_lm = TRUE
) {

  data_object %>%
    nest(data = -nesting_labels) %>%
    perform_aov(formula_aov) %>%
    {
      if (test_independence) {
        test_independence(.)
      } else {
        .
      }
    } %>%
    {
      if (test_homogeneity) {
        test_homogeneity(., formula_aov)
      } else {
        .
      }
    } %>%
    {
      if (test_normality) {
        test_normality(., formula_aov)
      } else {
        .
      }
    } %>%
    {
      if (create_plots_lm) {
        create_plots_lm(.)
      } else {
        .
      }
    }
}

analyze_non_parametric <- function(
  data_object, nesting_labels, formula_nonparam,
  test_homogeneity = TRUE
) {

  data_object %>%
    nest(data = -nesting_labels) %>%
    test_non_parametric(formula_nonparam) %>%
    {
      if (test_homogeneity) {
        test_homogeneity(., formula_nonparam)
      } else {
        .
      }
    }
}

paired_ttest <- function(
  data_post, data_pre,
  meta.type = "SMCC",
  vars = "vb",
  simple = TRUE) {

  n <- data_post %>% nrow()

  tt <- map2(data_post, data_pre,
             ~ t.test(.x, .y, paired = TRUE) %>%
               .[c("estimate", "p.value", "stderr")] %>%
               as_tibble %>%
               dplyr::rename(.,
                             m.diff = estimate,
                             se.diff = stderr,
                             p.diff = p.value
               ) %>%
               mutate(
                 d.diff = m.diff / (sqrt(n) * se.diff),
                 sd.pre = sd(.y),
                 sd.post = sd(.x),
                 r = cor(.x, .y),
                 # Ross & DeShon (2002): correlated sds
                 d.rm = (m.diff * sqrt(2*(1-r)) )/
                   (sd.pre^2 + sd.post^2 - 2*r*sd.pre*sd.post),
                 # Ross & DeShon (2002): prior sd
                 # d.sd.pre = (estimate * sqrt(2*(1-r)) )/ sd.pre,
                 d.sd.pre = m.diff/ sd.pre,
                 # change score standardization Wolfgang
                 d.mf = metafor::escalc(
                   measure = meta.type,
                   x1i = !!.x, x2i = !!.y,
                   m1i = mean(!!.x), m2i = mean(!!.y),
                   sd1i = sd(!!.x), sd2i = sd(!!.y),
                   ni = nrow(!!data_post),
                   ri = cor(!!.x, !!.y)
                 ) %>%
                   summary() %>%
                   select(yi, pval)
                 ,
                 m1 = mean(!!.y), m2 = mean(!!.x)
               )
  ) %>%
    imap_dfr( ~.x, .id = "item") %>%
    select(., item, m1, m2, m.diff, se.diff,
           sd.pre, sd.post, r,
           d.diff, p.diff, d.rm, d.sd.pre, d.mf)

  if (vars == "vb") {
    tt <- tt %>%
      filter(item %in% vb.perceptions)
  }

  if (simple) {
    tt <- tt %>%
      select(item, m1, m2, m.diff, se.diff,
             d.diff, p.diff, d.mf)
  }

  tt %>%
    select(., -d.mf) %>%
    bind_cols(tt[["d.mf"]]) %>%
    dplyr::rename(d.smcc = yi, p.smcc = pval)

}
