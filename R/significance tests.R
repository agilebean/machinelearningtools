perform_aov <- function(data_object, formula_aov) {

  data_object %>%
    mutate(
      aov = map(data, ~ aov(formula_aov, data = .x)),
      lm = map(data, ~ lm(formula_aov, data = .x)),
      glanced = map(lm, broom::glance), # aov doesn't yield "statistic"
      tidied = map(aov, broom::tidy),
    )
}

test_independence <- function(data_object, model_label = "aov") {

  require(DescTools)
  model <- rlang::sym(model_label)
  data_object %>%
    mutate(
      # independent samples test
      durbin = map(!!model, ~ DescTools::durbinWatsonTest(.x)),
      durbined = map(durbin, broom::glance),
    )
}

test_normality <- function(data_object, formula) {

  data_object %>%
    mutate(
      # Shapiro-Wilk test
      shapiro = map(aov, ~ .x %>% residuals %>% shapiro.test),
      shapiroed = map(shapiro, broom::glance),

      # # Kolmogorov-Smirnov test
      # smirnov = map(data, ~ ks.test(.x, "pnorm", mean = mean(.x), sd = sd(.x))),
      # smirnoved = map(smirnov, broom::glance),

      # ANOVA alternative: if no normality BUT only one-way ANOVA!
      # Kruskal-Wallis rank sum test: non-parametric test
      # uses sample medians instead of means
      kruskal = map(data, ~ kruskal.test(formula, data = .x)),
      kruskaled = map(kruskal, broom::glance),
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


perform_nonparametric <- function(data_object, formula) {

  data_object %>%
    mutate(
      # ANOVA alternative: if no homogeneity
      # Welch test
      welch = map(data,
                  ~ oneway.test(formula, data = .x, var.equal = FALSE)),
      welched = map(welch, broom::glance)

      # Bartlett test
    )
}

perform_posthoc_tests <- function(data_object, formula) {

  data_object %>%
    mutate(
      scheffe = map(aov, ~ DescTools::ScheffeTest(.x)), # only on factor
    )
}

create_plots <- function(data_object, model_label = "aov") {

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


print_html <- function(data_set, stat_type, grouping = NULL) {

  data_set %>%
    {
      if (stat_type == "levened") {
        unnest(., levened) %>%
          select(param, !!grouping, F.levene = `F value`, p.levene = `Pr(>F)`) %>%
          filter(!is.na(F.levene))

      } else if (stat_type == "glanced") {

        unnest(., glanced) %>%
          select(param, !!grouping, F.anova = statistic, p.anova = p.value)

      } else if (stat_type == "shapiroed") {

        unnest(., shapiroed) %>%
          select(param, !!grouping, W = statistic, p.shapiro = p.value)

      } else if (stat_type == "kruskaled") {

        unnest(., kruskaled) %>%
          select(param, !!grouping, K = statistic, p.kruskal = p.value)

      } else if (stat_type == "tidied") {

        unnest(., tidied) %>%
          select(param, term, !!grouping, F = statistic, p.value)
      } else if (stat_type == "durbined") {

        unnest(., durbined) %>%
          select(param, !!grouping, autocorrelation,
                 dw = statistic, p.durbin = p.value)

      } else if (stat_type == "tidied") {

        unnest(., tidied) %>%
          select(param, term, !!grouping, F = statistic, p.value)

      } else if (stat_type == "brownforsythed") {

        unnest(., tidied) %>%
          select(param, term, !!grouping, F = statistic, p.value)
      } else {
        unnest(., stat_type) %>%
          select(param, !!grouping, statistic, p.value)
      }
    } %>%
    {
      if (!is.null(grouping)) {
        arrange(., param, !!rlang::sym(grouping))
      } else {
        .
      }
    } %>%
    knitr::kable(format = "html", digits = 4) %>%
    kableExtra::kable_styling(bootstrap_options = c("bordered", "hover")) %>% print

}

analyze_aov <- function(
  data_object, nesting_labels, formula_aov,
  test_independence = TRUE,
  test_homogeneity = TRUE,
  test_normality = TRUE,
  perform_nonparametric = TRUE,
  create_plots = TRUE
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
      if (perform_nonparametric) {
        perform_nonparametric(., formula_aov)
      } else {
        .
      }
    } %>%
    {
      if (create_plots) {
        create_plots(.)
      } else {
        .
      }
    }
}

# create grouped data for group comparisons
summarise_wins <- function(result_data, max_wins, grouping) {

  result_data %>%
    group_by(across(all_of(grouping))) %>%
    summarise(
      mean = mean(wins / max_wins),
      sd = sd(wins / max_wins),
      se = sd / sqrt(n())
    )
}
