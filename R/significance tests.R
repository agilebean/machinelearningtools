# create grouped data for group comparisons
summarise_wins <- function(result_data, max_wins, grouping) {

  result_data %>%
    group_by(across(all_of(grouping))) %>%
    summarise(
      mean = mean(wins / max_wins),
      median = median(wins / max_wins),
      sd = sd(wins / max_wins),
      se = sd / sqrt(n()),
      n = n()
    )
}

summary_stats <- function(
  data_set, grouping_labels, dv_label, retain_data = FALSE) {

  dv <- rlang::sym(dv_label)

  data_set %>%
    nest(data = !any_of(grouping_labels)) %>%
    mutate(
      ci = map(data, ~ MeanCI(.x[[dv]])),
      var = map(data, ~ var(.x[[dv]])),
      sd = map(data, ~ sd(.x[[dv]])),
      se = map(data, ~ sd(.x[[dv]]) / sqrt(nrow(.x))),
      n = map(data, ~ nrow(.x))
    ) %>%
    unnest(c(var, sd, se, n)) %>%
    unnest_wider(ci) %>%
    {
      if (retain_data) {
        select(-data)
      } else {
        .
      }
    } %>%
    arrange(across(grouping_labels))

}

# assumes data_object is nested into a data column
perform_aov <- function(data_object, formula_aov) {

  response <- all.vars(formula_aov)[1]
  data_object %>%
    mutate(
      aov = map(data, ~ aov(formula_aov, data = .x)),
      lm = map(data, ~ lm(formula_aov, data = .x)),
      glanced = map(lm, broom::glance), # aov doesn't yield "statistic"
      tidied = map(aov, broom::tidy),
      # CIs on DV extracted from formula
      ci = map(data, ~ MeanCI(.x[[response]])),
      se = map(data, ~ sd(.x[[response]]) / sqrt(nrow(.x))),
      # posthoc scheffe shows which mean differences are significant
      scheffe = map(aov, ~ DescTools::ScheffeTest(.x)) # only on factor
    )
}

test_non_parametric <- function(data_object, formula_nonparam) {

  require(DescTools)
  require(rstatix)

  response <- formula_nonparam %>% all.vars() %>% .[1]
  group <- formula_nonparam %>% all.vars() %>% .[2]

  data_object %>%
    mutate(
      # Kruskal-Wallis test: non-parametric alternative to one-way ANOVA
      # uses sample medians instead of means
      kruskaled = map(data,
        ~ kruskal.test(formula_nonparam, data = .x) %>%
          broom::glance(.) %>%
          rename(df = parameter)),
      # wilcox does not correct for multiple comparisons with pooled variance
      wilcoxed = map(data, ~ pairwise.wilcox.test(x = response, g = group)),
      # dunn better than wilcox
      dunned = map(data, ~ DescTools::DunnTest(formula_nonparam, data = .x)),
      dunned2 = map(data, ~ rstatix::dunn_test(formula_nonparam, data = .x))
    )
}

test_normality <- function(data_object, formula) {

  response <- all.vars(formula)[1]

  data_object %>%
    mutate(
      # Shapiro-Wilk test: preferably on residuals than DV
      # Source: https://psychometroscar.com/2018/07/11/normality-residuals-or-dependent-variable/
      shapiroed = map(aov, ~ .x %>%
                        residuals %>%
                        shapiro.test %>%
                        broom::glance(.)),
      shapiroed2 = map(data, ~ .x[[response]] %>%
                         shapiro.test() %>%
                         broom::glance(.)),

      # Welch test is a form of ANOVA that allows for heterogeneity
      welched = map(
        data, ~ oneway.test(formula, data = .x, var.equal = FALSE) %>%
          broom::glance(.))
    )
}

test_smirnov <- function(data_object, score_var = "wins", cum_fun = "pnorm") {

  data_object %>%
    mutate(
      # Kolmogorov-Smirnov test
      smirnoved = map(data, ~ .x[[!!score_var]] %>%
          ks.test(., y = cum_fun, mean = mean(.), sd = sd(.)) %>%
            broom::glance(.))
    )
}

test_homogeneity <- function(data_object, formula) {

  require(onewaytests) # # Brown-Forsythe Test
  require(DescTools)

  data_object %>%
    mutate(
      # Levene test: homogeneious if p > 0.05, works only on factor
      levened = map(data, ~ DescTools::LeveneTest(formula, data = .x)),

      # Bartlett Test of Homogeneity of Variances BUT only one-way ANOVA!
      bartletted = map(data, ~ bartlett.test(formula, data = .x) %>%
                         broom::glance(.)),

      # Fligner-Killeen Test of Homogeneity of Variances
      flignered = map(data, ~ fligner.test(formula, data = .x) %>%
                        broom::glance(.)),

      # Brown-Forsythe Test
      brownforsythe = map(data, ~ bf.test(formula, data = .x)),
      brownforsythed = map(
        brownforsythe,
        ~ tibble(statistic = .x$statistic, p.value = .x$p.value)
      )
    )
}

test_independence <- function(data_object, model_label = "aov") {

  require(DescTools)
  model <- rlang::sym(model_label)
  data_object %>%
    mutate(
      # independent samples test
      durbined = map(!!model, ~ DescTools::DurbinWatsonTest(.x) %>%
                     broom::glance(.))
    )
}


create_plots_lm  <- function(data_object, model_label = "aov") {

  require(gglm)
  model <- rlang::sym(model_label)
  data_object %>%
    mutate(
      plot.residuals = map(!!model, ~ ggplot(data = .x) + stat_fitted_resid() ),
      plot.qq = map(!!model, ~ ggplot(data = .x) + stat_normal_qq() ),
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
                        convert_kable = FALSE,
                        convert_latex = FALSE,
                        digits = 4,
                        ...) {

  require(dplyr)

  param.sym = rlang::sym(param_var)

  result.table <- data_set %>%
    {
      if (stat_type == "levened") {
        unnest(., levened) %>%
          select(!!param.sym, !!grouping, F.levene = `F value`, p.levene = `Pr(>F)`) %>%
          filter(!is.na(F.levene))

      } else if (stat_type == "glanced") {

        unnest(., c(glanced, se)) %>%
          unnest_wider(ci) %>%
          select(param, !!grouping,
                 F.anova = statistic, p.anova = p.value, # glanced
                 mean, se, lwr.ci, upr.ci # MeanCI
          )

      } else if (stat_type == "shapiroed") {

        unnest(., shapiroed) %>%
          select(!!param.sym, !!grouping, W = statistic, p.shapiro = p.value)

      } else if (stat_type == "kruskaled") {

        unnest(., kruskaled) %>%
          select(!!param.sym, !!grouping, K = statistic, p.kruskal = p.value)

      } else if (stat_type == "dunned2") {

        unnest(., dunned2) %>%
          select(!!param.sym, !!grouping, group1, group2, F = statistic,
                 p, p.adj, p.adj.signif) %>%
          unite("groups", c(group1, group2), sep = "~")

      } else if (stat_type == "tidied") {

        unnest(., tidied) %>%
          select(!!param.sym, term, !!grouping, F = statistic, p.value)

      } else if (stat_type == "durbined") {

        unnest(., durbined) %>%
          select(!!param.sym, !!grouping, autocorrelation,
                 dw = statistic, p.durbin = p.value)

      } else if (stat_type == "tidied") {

        unnest(., tidied) %>%
          select(!!param.sym, term, !!grouping, F = statistic, p.value)

      } else if (stat_type == "brownforsythed") {

        unnest(., tidied) %>%
          select(!!param.sym, term, !!grouping, F = statistic, p.value)
      } else {
        unnest(., stat_type) %>%
          select(!!param.sym, !!grouping, statistic, p.value)
      }
    } %>%
    {
      if (!is.null(grouping)) {
        arrange(., !!param.sym, !!rlang::sym(grouping))
      } else {
        .
      }
    }


  if (convert_kable) {

    result.table %>%
      convert_kable(., digits = digits, ...) %>%
      {
        if (save_label != "") {

          cat(., file = paste0(save_label, ".", format))

        } else {
          .
        }
      }
  }

  if (convert_latex) {

    result.table %>%
      convert_latex(., digits = digits, ...) %>%
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
  data_object, nesting_labels, formula_aov,
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


