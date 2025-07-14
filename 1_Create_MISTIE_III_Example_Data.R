library(dplyr)
library(here)
library(impart)
library(labelled)
library(pwr)
library(rpact)

parameters_example <-
  config::get(
    config = "binary_example_mistie",
    file = file.path(here::here(), "config.yml")
  )




### Set Up Fixed Sample Design #################################################
set.seed(seed = parameters_example$rng_seed)

alpha_design <- parameters_example$alpha_design
power_design <- parameters_example$power_design
test_sides <- parameters_example$test_sides
enroll_start <- as.Date(parameters_example$enroll_start)
enroll_end <- as.Date(parameters_example$enroll_end)

p0_design <- parameters_example$p0_design
p1_design <- parameters_example$p1_design
risk_difference_design <- p1_design - p0_design
es_h_design <- pwr::ES.h(p1 = p1_design, p2 = p0_design)
pr_missing <- parameters_example$pr_missing

n_per_arm_fixed <-
  pwr::pwr.2p.test(
    h = es_h_design,
    sig.level = alpha_design,
    power = power_design
  )$n %>% ceiling()

# Increase N for missingness
n_per_arm_fixed <-
  ceiling(n_per_arm_fixed/(1 - pr_missing))

n_total_fixed <- 2*n_per_arm_fixed




### Set Up Group Sequential Design #############################################
information_rates <-
  parameters_example$information_rates
type_of_design_asof <- parameters_example$type_of_design_asof
type_beta_spending_asof <- parameters_example$type_beta_spending_asof

type_of_design_asof_bsof <- parameters_example$type_of_design_asof_bsof
type_beta_spending_asof_bsof <- parameters_example$type_beta_spending_asof_bsof




### Set up group sequential testing procedure: O-F, No Futility ################
trial_design_asof <-
  rpact::getDesignGroupSequential(
    alpha = alpha_design,
    beta = 1 - power_design,
    sided = test_sides,
    informationRates = information_rates,
    typeOfDesign = type_of_design_asof,
    typeBetaSpending = type_beta_spending_asof
  )

inflation_factor_asof <-
  rpact::getDesignCharacteristics(trial_design_asof)$inflationFactor

n_total_gsd_asof <-
  ceiling(2*n_per_arm_fixed*inflation_factor_asof)

n_at_analyses_asof <-
  ceiling(information_rates*n_total_gsd_asof)


### Set up group sequential testing procedure: O-F Efficacy/Futility ###########
trial_design_asof_bsof <-
  rpact::getDesignGroupSequential(
    alpha = alpha_design,
    beta = 1 - power_design,
    sided = test_sides,
    informationRates = information_rates,
    typeOfDesign = type_of_design_asof_bsof,
    typeBetaSpending = type_beta_spending_asof_bsof,
    bindingFutility = FALSE
  )

inflation_factor_asof_bsof <-
  rpact::getDesignCharacteristics(trial_design_asof_bsof)$inflationFactor

n_total_gsd_asof_bsof <-
  ceiling(2*n_per_arm_fixed*inflation_factor_asof_bsof)

n_at_analyses_asof_bsof <-
  ceiling(information_rates*n_total_gsd_asof_bsof)

# Maximum number needed under either design
n_total_gsd <- pmax(n_total_gsd_asof, n_total_gsd_asof_bsof)




### Load Simulated MISTIE III Data #############################################
sim_miii_csv_path <-
  file.path(here::here("Simulated_MISTIE_III_v1.2.csv"))

if(!file.exists(sim_miii_csv_path)){
  data_url <-
    paste0("https://github.com/jbetz-jhu/CovariateAdjustmentTutorial",
           "/raw/main/Simulated_MISTIE_III_v1.2.csv")
  
  sim_miii_full <- read.csv(file = url(data_url))
  
  write.csv(
    x = sim_miii_full,
    file = sim_miii_csv_path,
    row.names = FALSE
  )
} else {
  sim_miii_full <- 
    read.csv(
      file = sim_miii_csv_path
    )
}


sim_miii_full <-
  sim_miii_full %>%
  dplyr::tibble() %>%
  dplyr::mutate(
    # Convert variables from binary indicators to labeled categorical variables
    male =
      factor(
        x = male,
        levels = 0:1,
        labels = c("0. Female", "1. Male")
      ),

    across(
      .cols =
        all_of(
          x = c("hx_cvd", "hx_hyperlipidemia",
                "on_anticoagulants", "on_antiplatelets")
        ),
      .fns = function(x) factor(x, levels = 0:1, labels = c("0. No", "1. Yes"))
    ),

    # Convert GCS and MRS variables from character data to categorical variables
    across(
      .cols = starts_with("gcs") | starts_with("mrs"),
      .fns = factor
    ),

    ich_location =
      factor(
        x = ich_location,
        levels = c("Deep", "Lobar")
      ),

    arm =
      factor(
        x = arm,
        levels = c("medical", "surgical")
      ),

    tx = 1*(arm == "surgical"),

    mrs_30d_binary =
      case_when(
        mrs_30d %in% c("0-3") ~ 1,
        mrs_30d %in% c("4", "5", "6") ~ 0,
      ),

    mrs_180d_binary =
      case_when(
        mrs_180d %in% c("0-2", "3") ~ 1,
        mrs_180d %in% c("4", "5", "6") ~ 0,
      ),

    mrs_365d_binary =
      case_when(
        mrs_365d %in% c("0-1", "2", "3") ~ 1,
        mrs_365d %in% c("4", "5", "6") ~ 0,
      ),

    mrs_365d_binary_factor =
      factor(
        x = mrs_365d_binary
      )
  )

# Apply labels to Variables
labelled::var_label(sim_miii_full$age) <-
  "Age at Presentation (y)"
labelled::var_label(sim_miii_full$male) <-
  "Sex"
labelled::var_label(sim_miii_full$hx_cvd) <-
  "Hx Cardiovascular Disease"
labelled::var_label(sim_miii_full$hx_hyperlipidemia) <-
  "Hx Hyperlipidemia"
labelled::var_label(sim_miii_full$on_anticoagulants) <-
  "On Anticoagulants: Presentation"
labelled::var_label(sim_miii_full$on_antiplatelets) <-
  "On Antiplatelets: Presentation"

labelled::var_label(sim_miii_full$ich_location) <-
  "ICH Location"
labelled::var_label(sim_miii_full$ich_s_volume) <-
  "ICH Volume (mL): Stability"
labelled::var_label(sim_miii_full$ivh_s_volume) <-
  "IVH Volume (mL): Stability"
labelled::var_label(sim_miii_full$gcs_category) <-
  "Glasgow Coma Scale: Presentation"

labelled::var_label(sim_miii_full$mrs_30d) <-
  "Modified Rankin Scale: 30 Days"
labelled::var_label(sim_miii_full$mrs_180d) <-
  "Modified Rankin Scale: 180 Days"
labelled::var_label(sim_miii_full$mrs_365d) <-
  "Modified Rankin Scale: 365 Days"
labelled::var_label(sim_miii_full$mrs_365d_binary) <-
  "Modified Rankin Scale < 4: 365 Days"
labelled::var_label(sim_miii_full$mrs_365d_binary_factor) <-
  "Modified Rankin Scale < 4: 365 Days"

# Set Enrollment Time
enroll_duration_days <-
  difftime(
    time1 = enroll_end,
    time2 = enroll_start
  ) %>% 
  as.numeric()

sim_miii_max <-
  sim_miii_full %>% 
  dplyr::slice(1:n_total_gsd) %>% 
  as.data.frame()

sim_miii_max$enroll_time <-
  runif(
    n = n_total_gsd,
    min = 0,
    max = enroll_duration_days
  ) %>% 
  round %>% 
  sort

sim_miii_max <-
  sim_miii_max %>%
  # Simulate Study Times
  dplyr::mutate(
    time_to_30 = 
      case_when(
        mrs_30d == 6 ~ 30 - 7,
        is.na(mrs_30d) ~ 30 + 7,
        TRUE ~ runif(n = n(), min = 30 - 7, max = 30 + 7)
      ),
    time_30 = enroll_time + time_to_30,
    time_to_180 =
      case_when(
        mrs_30d == 6 | mrs_180d == 6 ~ 180 - 7*2,
        is.na(mrs_180d) ~ 180 + 2*7,
        TRUE ~ runif(n = n(), min = 180 - 7*2, max = 180 + 7*2)
      ),
    time_180 = enroll_time + time_to_180,
    time_to_365 = 
      case_when(
        mrs_30d == 6 | mrs_180d == 6 ~ 365 - 7*2,
        is.na(mrs_365d) ~ 365 + 2*7,
        mrs_365d == 6 & days_on_study <= 365 - 7*2 ~ 365 - 7*2,
        mrs_365d == 6 & days_on_study > 365 - 7*2 ~ days_on_study,
        mrs_365d %in% c("0-1", "2", "3", "4", "5") ~ days_on_study,
        TRUE ~ NA
      ),
    time_365 = enroll_time + time_to_365
  )

last_event <-
  sim_miii_max[, c("enroll_time", "time_30", "time_180", "time_365")] %>% 
  unlist() %>% 
  max(na.rm = TRUE) %>% 
  ceiling()

# Get Fixed Sample Size Design Sample
sim_miii_fixed <-
  sim_miii_max %>% 
  dplyr::slice(1:n_total_fixed) %>% 
  as.data.frame()

# Get Group Sequential Design Sample
sim_miii_asof <-
  sim_miii_max %>% 
  dplyr::slice(1:n_total_gsd_asof) %>% 
  as.data.frame()

sim_miii_asof_bsof <-
  sim_miii_max %>% 
  dplyr::slice(1:n_total_gsd_asof_bsof) %>% 
  as.data.frame()




### Analysis Times: ASOF Design ################################################
prepared_sim_miii_asof <-
  impart::prepare_monitored_study_data(
    data = sim_miii_asof,
    study_time = last_event,
    id_variable = "sim_participant_id",
    covariates_variables = 
      c("age", "male", "hx_cvd", "hx_hyperlipidemia",
        "on_anticoagulants", "on_antiplatelets",
        "ich_location", "ich_s_volume", "ivh_s_volume", "gcs_category"),
    enrollment_time_variable = "enroll_time",
    treatment_variable = "tx",
    outcome_variables = c("mrs_30d_binary", "mrs_180d_binary", "mrs_365d_binary"),
    outcome_time_variables = c("time_30", "time_180", "time_365"),
    observe_missing_times = c(30 + 7, 180 + 2*7, 365 + 2*7)
  )

outcome_counts_asof <-
  impart::count_outcomes(
    prepared_data = prepared_sim_miii_asof
  )

analysis_times_asof <-
  subset(
    x = outcome_counts_asof,
    event == "mrs_365d_binary" & 
      count_complete %in% n_at_analyses_asof
  )

if(nrow(analysis_times_asof) < length(n_at_analyses_asof)){
  analysis_times_asof <-
    rbind(
      analysis_times_asof,
      subset(
        x = outcome_counts_asof,
        event == "mrs_365d_binary"
      ) |>
        tail(n = 1)
    )
}




### Analysis Times: ASOF + BSOF Design #########################################
prepared_sim_miii_asof_bsof <-
  impart::prepare_monitored_study_data(
    data = sim_miii_asof_bsof,
    study_time = last_event,
    id_variable = "sim_participant_id",
    covariates_variables = 
      c("age", "male", "hx_cvd", "hx_hyperlipidemia",
        "on_anticoagulants", "on_antiplatelets",
        "ich_location", "ich_s_volume", "ivh_s_volume", "gcs_category"),
    enrollment_time_variable = "enroll_time",
    treatment_variable = "tx",
    outcome_variables = c("mrs_30d_binary", "mrs_180d_binary", "mrs_365d_binary"),
    outcome_time_variables = c("time_30", "time_180", "time_365"),
    observe_missing_times = c(30 + 7, 180 + 2*7, 365 + 2*7)
  )

outcome_counts_asof_bsof <-
  impart::count_outcomes(
    prepared_data = prepared_sim_miii_asof_bsof
  )

analysis_times_asof_bsof <-
  subset(
    x = outcome_counts_asof_bsof,
    event == "mrs_365d_binary" & 
      count_complete %in% n_at_analyses_asof_bsof
  )

if(nrow(analysis_times_asof_bsof) < length(n_at_analyses_asof_bsof)){
  analysis_times_asof_bsof <-
    rbind(
      analysis_times_asof_bsof,
      subset(
        x = outcome_counts_asof_bsof,
        event == "mrs_365d_binary"
      ) |>
        tail(n = 1)
    )
}




### Efficacy: O-F; No Futility #################################################
sim_miii_asof_final <-
  data_at_time_t(
    prepared_data = prepared_sim_miii_asof,
    study_time = analysis_times_asof$time[3]
  )$data

sim_miii_asof_ia_2 <-
  data_at_time_t(
    prepared_data = prepared_sim_miii_asof,
    study_time = analysis_times_asof$time[2]
  )$data

sim_miii_asof_ia_1 <-
  data_at_time_t(
    prepared_data = prepared_sim_miii_asof,
    study_time = analysis_times_asof$time[1]
  )$data

### Efficacy: O-F; Futility: O-F ###############################################
sim_miii_asof_bsof_final <-
  data_at_time_t(
    prepared_data = prepared_sim_miii_asof_bsof,
    study_time = analysis_times_asof_bsof$time[3]
  )$data

sim_miii_asof_bsof_ia_2 <-
  data_at_time_t(
    prepared_data = prepared_sim_miii_asof_bsof,
    study_time = analysis_times_asof_bsof$time[2]
  )$data

sim_miii_asof_bsof_ia_1 <-
  data_at_time_t(
    prepared_data = prepared_sim_miii_asof_bsof,
    study_time = analysis_times_asof_bsof$time[1]
  )$data

save(
  list =
    c(
      "alpha_design",
      "power_design",
      "test_sides",
      "p0_design",
      "p1_design",
      "risk_difference_design",
      "es_h_design",
      "pr_missing",
      "n_per_arm_fixed",
      "n_total_fixed",
      "information_rates",
      "type_of_design_asof",
      "type_beta_spending_asof",
      "type_of_design_asof_bsof",
      "type_beta_spending_asof_bsof",
      "trial_design_asof",
      "inflation_factor_asof",
      "n_total_gsd_asof",
      "n_at_analyses_asof",
      "trial_design_asof_bsof",
      "inflation_factor_asof_bsof",
      "n_total_gsd_asof_bsof",
      "n_at_analyses_asof_bsof",
      "n_total_gsd",
      "sim_miii_fixed",
      "sim_miii_asof_final",
      "sim_miii_asof_ia_2",
      "sim_miii_asof_ia_1",
      "sim_miii_asof_bsof_final",
      "sim_miii_asof_bsof_ia_2",
      "sim_miii_asof_bsof_ia_1"
    ),
  file = 
    file.path(
      here::here(), 
      parameters_example$example_data_file_name
    )
)