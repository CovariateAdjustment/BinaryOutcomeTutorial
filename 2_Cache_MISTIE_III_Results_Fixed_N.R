library(dplyr)
library(labelled)
library(pwr)
library(rpact)

parameters_example <-
  config::get(
    config = "binary_example_mistie",
    file = file.path(here::here(), "config.yml")
  )

load(
  file = file.path(here::here(), parameters_example$example_data_file_name)
)

### Set Up Fixed Sample Design #################################################
set.seed(seed = parameters_example$rng_seed)


### Crude Estimates ############################################################
mrs_2x2 <-
  with(data = sim_miii_fixed,
    table(arm, mrs_365d_binary)
  )

y1 <- mrs_2x2["surgical", 2]
n1 <- sum(mrs_2x2["surgical", ])
y0 <- mrs_2x2["medical", 2]
n0 <- sum(mrs_2x2["medical", ])




### Logistic Regression Model ##################################################
mrs_unadjusted_logistic_glm <-
  stats::glm(
    formula = mrs_365d_binary ~ arm,
    data = sim_miii_fixed,
    family = binomial(link = "logit")
  )

pr_outcome_unadj_control <-
  stats::predict(
    object = mrs_unadjusted_logistic_glm,
    newdata = 
      within(data = sim_miii_fixed,
             expr = {arm = "medical"}),
    type = "response"
  )

pr_outcome_unadj_treatment <-
  stats::predict(
    object = mrs_unadjusted_logistic_glm,
    newdata = 
      within(data = sim_miii_fixed,
             expr = {arm = "surgical"}),
    type = "response"
  )

e_y_0_unadj <- mean(pr_outcome_unadj_control)
e_y_1_unadj <- mean(pr_outcome_unadj_treatment)


# Risk Difference
rd_unadj <- e_y_1_unadj - e_y_0_unadj
# Relative Risk
rr_unadj <- e_y_1_unadj/e_y_0_unadj
# Odds Ratio
or_unadj <- (e_y_1_unadj*(1 - e_y_0_unadj))/(e_y_0_unadj*(1 - e_y_1_unadj))




### G-Computation ##############################################################
source("boot_p_value.R")



### Risk Difference - Unadjusted ###
m3_rd_unadjusted_verb <-
  impart::standardization(
    data = sim_miii_fixed,
    outcome_formula = mrs_365d_binary ~ tx,
    family = binomial(link = "logit"),
    estimand = "difference",
    treatment_column = "tx",
    se_method = "bootstrap",
    variance_adjustment = NULL,
    verbose = TRUE
  )

rdu <- m3_rd_unadjusted_verb$result

rdu_p_value <-
  boot_p_value(
    boot_object =
      m3_rd_unadjusted_verb$boot_object,
    ci_method = "bca",
    null_value = 0
  )


### Risk Difference - Adjusted ###
m3_rd_adjusted_verb <-
  impart::standardization(
    data = sim_miii_fixed,
    outcome_formula = mrs_365d_binary ~ tx +
      age + male + hx_cvd + hx_hyperlipidemia +
      on_anticoagulants + on_antiplatelets +
      ich_location + ich_s_volume + ivh_s_volume + gcs_category,
    family = binomial(link = "logit"),
    estimand = "difference",
    treatment_column = "tx",
    se_method = "bootstrap",
    variance_adjustment = NULL,
    verbose = TRUE
  )

rda <- m3_rd_adjusted_verb$result

rda_p_value <-
  boot_p_value(
    boot_object =
      m3_rd_adjusted_verb$boot_object,
    ci_method = "bca",
    null_value = 0
  )

m3_rd_adjusted_eif <-
  impart::standardization(
    data = sim_miii_fixed,
    outcome_formula = mrs_365d_binary ~ tx +
      age + male + hx_cvd + hx_hyperlipidemia +
      on_anticoagulants + on_antiplatelets +
      ich_location + ich_s_volume + ivh_s_volume + gcs_category,
    family = binomial(link = "logit"),
    estimand = "difference",
    treatment_column = "tx",
    se_method = "influence",
    verbose = TRUE
  )



### Risk Ratio - Unadjusted ###
m3_rr_unadjusted_verb <-
  impart::standardization(
    data = sim_miii_fixed,
    outcome_formula = mrs_365d_binary ~ tx,
    family = binomial(link = "logit"),
    estimand = "ratio",
    treatment_column = "tx",
    se_method = "bootstrap",
    variance_adjustment = NULL,
    verbose = TRUE
  )

rru <- m3_rr_unadjusted_verb$result

rru_p_value <-
  boot_p_value(
    boot_object =
      m3_rr_unadjusted_verb$boot_object,
    ci_method = "bca",
    null_value = 1
  )




### Risk Ratio - Adjusted ###
m3_rr_adjusted_verb <-
  impart::standardization(
    data = sim_miii_fixed,
    outcome_formula = mrs_365d_binary ~ tx +
      age + male + hx_cvd + hx_hyperlipidemia +
      on_anticoagulants + on_antiplatelets +
      ich_location + ich_s_volume + ivh_s_volume + gcs_category,
    family = binomial(link = "logit"),
    estimand = "ratio",
    treatment_column = "tx",
    se_method = "bootstrap",
    variance_adjustment = NULL,
    verbose = TRUE
  )

rra <- m3_rr_adjusted_verb$result

rra_p_value <-
  boot_p_value(
    boot_object =
      m3_rr_adjusted_verb$boot_object,
    ci_method = "bca",
    null_value = 1
  )




### Odds Ratio - Unadjusted ###
m3_or_unadjusted_verb <-
  impart::standardization(
    data = sim_miii_fixed,
    outcome_formula = mrs_365d_binary ~ tx,
    family = binomial(link = "logit"),
    estimand = "oddsratio",
    treatment_column = "tx",
    se_method = "bootstrap",
    variance_adjustment = NULL,
    verbose = TRUE
  )

oru <- m3_or_unadjusted_verb$result

oru_p_value <-
  boot_p_value(
    boot_object =
      m3_or_unadjusted_verb$boot_object,
    ci_method = "bca",
    null_value = 1
  )




### Odds Ratio - Adjusted ###
m3_or_adjusted_verb <-
  impart::standardization(
    data = sim_miii_fixed,
    outcome_formula = mrs_365d_binary ~ tx +
      age + male + hx_cvd + hx_hyperlipidemia +
      on_anticoagulants + on_antiplatelets +
      ich_location + ich_s_volume + ivh_s_volume + gcs_category,
    family = binomial(link = "logit"),
    estimand = "oddsratio",
    treatment_column = "tx",
    se_method = "bootstrap",
    variance_adjustment = NULL,
    verbose = TRUE
  )

ora <- m3_or_adjusted_verb$result

ora_p_value <-
  boot_p_value(
    boot_object =
      m3_or_adjusted_verb$boot_object,
    ci_method = "bca",
    null_value = 1
  )


### Summarize Results ##########################################################
sim_miii_fixed_results_unadjusted <-
  dplyr::tribble(
    ~ estimand, ~ estimate, ~ se, ~lcl, ~ucl, ~pvalue,
    "RD", rdu$estimate, rdu$se_boot_df_adjusted,
    rdu$lcl_bca_df_adjusted_0.95, rdu$ucl_bca_df_adjusted_0.95, rdu_p_value,
    
    "RR", rru$estimate, rru$se_boot_df_adjusted,
    rru$lcl_bca_df_adjusted_0.95, rru$ucl_bca_df_adjusted_0.95, rru_p_value,
    
    "OR", oru$estimate, oru$se_boot_df_adjusted,
    oru$lcl_bca_df_adjusted_0.95, oru$ucl_bca_df_adjusted_0.95, oru_p_value,
  ) %>% 
  dplyr::mutate(
    `CI Width` = ucl - lcl,
  ) %>% 
  dplyr::select(
    Estimand = estimand,
    Estimate = estimate,
    `SE` = se,
    `L95%` = lcl,
    `U95%` = ucl,
    `CI Width`,
    `p-Value` = pvalue
  )


sim_miii_fixed_results_adjusted <-
  dplyr::tribble(
    ~ estimand, ~ estimate, ~ se, ~lcl, ~ucl, ~pvalue,
    "RD", rda$estimate, rda$se_boot_df_adjusted,
    rda$lcl_bca_df_adjusted_0.95, rda$ucl_bca_df_adjusted_0.95, rda_p_value,
    
    "RR", rra$estimate, rra$se_boot_df_adjusted,
    rra$lcl_bca_df_adjusted_0.95, rra$ucl_bca_df_adjusted_0.95, rra_p_value,
    
    "OR", ora$estimate, ora$se_boot_df_adjusted,
    ora$lcl_bca_df_adjusted_0.95, ora$ucl_bca_df_adjusted_0.95, ora_p_value,
  ) %>% 
  dplyr::mutate(
    `CI Width` = ucl - lcl,
  ) %>% 
  dplyr::select(
    Estimand = estimand,
    Estimate = estimate,
    `SE` = se,
    `L95%` = lcl,
    `U95%` = ucl,
    `CI Width`,
    `p-Value` = pvalue
  )


sim_miii_fixed_results_compared <-
  dplyr::full_join(
    x = sim_miii_fixed_results_unadjusted,
    y = sim_miii_fixed_results_adjusted,
    by = "Estimand"
  ) %>% 
  dplyr::rename_with(
    .fn = function(x) 
      stringr::str_replace(
        string = x,
        pattern = "\\.x",
        replacement = "_u"
      ) %>% 
      stringr::str_replace(
        string = .,
        pattern = "\\.y",
        replacement = "_a"
      )
  ) %>% 
  dplyr::mutate(
    var_u = SE_u^2,
    var_a = SE_a^2,
    re = var_u/var_a,
    rcv = (var_a - var_u)/var_u,
    rcp = re - 1,
    rciw = `CI Width_a`/`CI Width_u`
  ) %>% 
  dplyr::select(
    Estimand,
    `RE` = re,
    `RCV` = rcv,
    `RCP` = rcp,
    `RCIW` = rciw
  )

re_change <- sort(100*(range(sim_miii_fixed_results_compared$RE) - 1))
rcv_change <- sort(100*(range(sim_miii_fixed_results_compared$RCV)))
rcp_change <- sort(100*(range(sim_miii_fixed_results_compared$RCP)))
rciw_change <- sort(100*(1 - range(sim_miii_fixed_results_compared$RCIW)))

sim_miii_fixed_results_compared <-
  sim_miii_fixed_results_compared %>%
  dplyr::rename(
    `Relative Efficiency` = RE,
    `Relative Change in Variance` = RCV,
    `Relative Change in Precision` = RCP,
    `Relative CI Width` = RCIW
  )

save(
  list = ls(),
  file = 
    file.path(
      here::here(), 
      parameters_example$example_output_file_name
    )
)