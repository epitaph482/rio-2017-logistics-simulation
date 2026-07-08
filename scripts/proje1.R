########################################################
# RIO 2017 LOGISTICS SIMULATION PROJECT
# COMBINED ANALYSIS SCRIPT (PRE-CARRIER & FINAL SCENARIOS)
########################################################

# ======================================================
# 0. GLOBAL SETUP & PACKAGES
# ======================================================

packages <- c(
  "readxl",
  "ggplot2",
  "fitdistrplus",
  "moments",
  "nortest",
  "dplyr",
  "tidyr",
  "gridExtra"
)

missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

if(length(missing_packages) > 0){
  install.packages(missing_packages)
}

invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# ------------------------------------------------------
# GLOBAL FILE PATH
# (Veri setinin, script ile ayni dizindeki 'data' klasorunde oldugunu varsayar)
# ------------------------------------------------------
global_file_path <- "data/rio_2017_queue_capacity_3_4_5_detailed_clarified.xlsx"
global_sheet_name <- "Full Data All Orders"


########################################################
# PART 1: QUEUE-ADJUSTED PRE-CARRIER HANDOFF TIME ANALYSIS
# Metric: carrier_dispatch_time + queue_delay_days
########################################################
cat("\n====================================\n")
cat("STARTING PART 1: PRE-CARRIER HANDOFF ANALYSIS\n")
cat("====================================\n")

available_sheets_1 <- readxl::excel_sheets(global_file_path)

if(!(global_sheet_name %in% available_sheets_1)){
  sheet_name_1 <- available_sheets_1[1]
} else {
  sheet_name_1 <- global_sheet_name
}

df <- readxl::read_excel(global_file_path, sheet = sheet_name_1)
df <- as.data.frame(df)

cat("\n====================================\n")
cat("DATA READ (PART 1)\n")
cat("====================================\n")
cat("Used sheet:", sheet_name_1, "\n")
cat("Rows before cleaning:", nrow(df), "\n")

required_cols_1 <- c(
  "order_purchase_interval",
  "order_approval_time",
  "carrier_dispatch_time",
  "delivery_transit_time",
  "queue_delay_days"
)

missing_cols_1 <- setdiff(required_cols_1, names(df))

if(length(missing_cols_1) > 0){
  stop(paste("Eksik kolon var:", paste(missing_cols_1, collapse = ", ")))
}

df$order_purchase_interval <- as.numeric(df$order_purchase_interval)
df$order_approval_time <- as.numeric(df$order_approval_time)
df$carrier_dispatch_time <- as.numeric(df$carrier_dispatch_time)
df$delivery_transit_time <- as.numeric(df$delivery_transit_time)
df$queue_delay_days <- as.numeric(df$queue_delay_days)

df$queue_delay_days[is.na(df$queue_delay_days)] <- 0

rows_before_cleaning_1 <- nrow(df)

df <- df[
  is.finite(df$order_purchase_interval) &
    is.finite(df$order_approval_time) &
    is.finite(df$carrier_dispatch_time) &
    is.finite(df$delivery_transit_time) &
    is.finite(df$queue_delay_days) &
    df$order_purchase_interval >= 0 &
    df$order_approval_time >= 0 &
    df$carrier_dispatch_time >= 0 &
    df$delivery_transit_time >= 0 &
    df$queue_delay_days >= 0,
]

rows_after_cleaning_1 <- nrow(df)

cat("\n====================================\n")
cat("CLEANING SUMMARY (PART 1)\n")
cat("====================================\n")
cat("Rows before cleaning:", rows_before_cleaning_1, "\n")
cat("Rows after cleaning:", rows_after_cleaning_1, "\n")
cat("Removed rows:", rows_before_cleaning_1 - rows_after_cleaning_1, "\n")

df$pre_carrier_handoff_days <- df$carrier_dispatch_time + df$queue_delay_days
df$pre_carrier_handoff_hours <- df$pre_carrier_handoff_days * 24

df$order_to_carrier_handoff_days <- (df$order_approval_time / 1440) + df$carrier_dispatch_time + df$queue_delay_days
df$order_to_carrier_handoff_hours <- df$order_to_carrier_handoff_days * 24

cat("\n====================================\n")
cat("METRIC CHECK (PART 1)\n")
cat("====================================\n")
cat("Minimum pre-carrier handoff days:", min(df$pre_carrier_handoff_days, na.rm = TRUE), "\n")
cat("Maximum pre-carrier handoff days:", max(df$pre_carrier_handoff_days, na.rm = TRUE), "\n")

output_dir_1 <- "pre_carrier_handoff_outputs_clean"
if(!dir.exists(output_dir_1)){ dir.create(output_dir_1) }

delayed_values <- df$queue_delay_days[df$queue_delay_days > 0]
mean_queue_delay_delayed <- if(length(delayed_values) > 0){ mean(delayed_values, na.rm = TRUE) } else { 0 }

summary_table <- data.frame(
  Metric = c(
    "N orders", "Mean pre-carrier handoff days", "Median pre-carrier handoff days",
    "SD pre-carrier handoff days", "Minimum pre-carrier handoff days",
    "Q1 pre-carrier handoff days", "Q3 pre-carrier handoff days",
    "Maximum pre-carrier handoff days", "Mean pre-carrier handoff hours",
    "Median pre-carrier handoff hours", "Mean order-to-carrier handoff days",
    "Median order-to-carrier handoff days", "Queue delayed orders",
    "Queue delay rate percent", "Mean queue delay days - all orders",
    "Mean queue delay days - delayed orders only", "Maximum queue delay days"
  ),
  Value = c(
    nrow(df), mean(df$pre_carrier_handoff_days, na.rm = TRUE), median(df$pre_carrier_handoff_days, na.rm = TRUE),
    sd(df$pre_carrier_handoff_days, na.rm = TRUE), min(df$pre_carrier_handoff_days, na.rm = TRUE),
    as.numeric(quantile(df$pre_carrier_handoff_days, 0.25, na.rm = TRUE)),
    as.numeric(quantile(df$pre_carrier_handoff_days, 0.75, na.rm = TRUE)),
    max(df$pre_carrier_handoff_days, na.rm = TRUE), mean(df$pre_carrier_handoff_hours, na.rm = TRUE),
    median(df$pre_carrier_handoff_hours, na.rm = TRUE), mean(df$order_to_carrier_handoff_days, na.rm = TRUE),
    median(df$order_to_carrier_handoff_days, na.rm = TRUE), sum(df$queue_delay_days > 0, na.rm = TRUE),
    mean(df$queue_delay_days > 0, na.rm = TRUE) * 100, mean(df$queue_delay_days, na.rm = TRUE),
    mean_queue_delay_delayed, max(df$queue_delay_days, na.rm = TRUE)
  )
)

cat("\n====================================\n")
cat("PRE-CARRIER HANDOFF SUMMARY\n")
cat("====================================\n")
print(summary_table)

write.csv(summary_table, file.path(output_dir_1, "pre_carrier_handoff_summary_clean.csv"), row.names = FALSE)

queue_delay_frequency <- as.data.frame(table(df$queue_delay_days))
names(queue_delay_frequency) <- c("Queue_Delay_Days", "Frequency")
queue_delay_frequency$Queue_Delay_Days <- as.numeric(as.character(queue_delay_frequency$Queue_Delay_Days))
queue_delay_frequency$Percent <- queue_delay_frequency$Frequency / sum(queue_delay_frequency$Frequency) * 100

cat("\n====================================\n")
cat("QUEUE DELAY FREQUENCY TABLE\n")
cat("====================================\n")
print(queue_delay_frequency)

write.csv(queue_delay_frequency, file.path(output_dir_1, "queue_delay_frequency_clean.csv"), row.names = FALSE)
write.csv(df, file.path(output_dir_1, "data_with_pre_carrier_handoff_time_clean.csv"), row.names = FALSE)

p_hist <- ggplot(df, aes(x = pre_carrier_handoff_days)) +
  geom_histogram(bins = 40, fill = "gray80", color = "black") +
  labs(title = "Distribution of Queue-Adjusted Pre-Carrier Handoff Time", subtitle = "Metric: carrier_dispatch_time + queue_delay_days", x = "Pre-carrier handoff time (days)", y = "Number of orders") +
  theme_minimal()
ggsave(filename = file.path(output_dir_1, "pre_carrier_handoff_histogram_clean.png"), plot = p_hist, width = 8, height = 5, dpi = 300)

p_box <- ggplot(df, aes(y = pre_carrier_handoff_days)) +
  geom_boxplot(fill = "gray80", color = "black") +
  labs(title = "Boxplot of Queue-Adjusted Pre-Carrier Handoff Time", subtitle = "Metric: carrier_dispatch_time + queue_delay_days", y = "Pre-carrier handoff time (days)") +
  theme_minimal()
ggsave(filename = file.path(output_dir_1, "pre_carrier_handoff_boxplot_clean.png"), plot = p_box, width = 6, height = 5, dpi = 300)

p_order_to_carrier_hist <- ggplot(df, aes(x = order_to_carrier_handoff_days)) +
  geom_histogram(bins = 40, fill = "gray80", color = "black") +
  labs(title = "Distribution of Queue-Adjusted Order-to-Carrier Handoff Time", subtitle = "Metric: order_approval_time / 1440 + carrier_dispatch_time + queue_delay_days", x = "Order-to-carrier handoff time (days)", y = "Number of orders") +
  theme_minimal()
ggsave(filename = file.path(output_dir_1, "order_to_carrier_handoff_histogram_clean.png"), plot = p_order_to_carrier_hist, width = 8, height = 5, dpi = 300)

report_note <- c(
  "Queue-adjusted pre-carrier handoff time was calculated as carrier_dispatch_time + queue_delay_days.",
  "This metric represents the total time before an order is handed over to the carrier under the capacity-constrained queue model.",
  "Order-to-carrier handoff time was also calculated as order_approval_time / 1440 + carrier_dispatch_time + queue_delay_days.",
  "Queue delay is not treated as a separate fitted input distribution.",
  "It is generated by the daily carrier capacity rule and analyzed as an output of the queue model.",
  "Because carrier capacity was defined as a daily capacity, queue delay was measured in days rather than minutes."
)
writeLines(report_note, file.path(output_dir_1, "pre_carrier_handoff_report_note_clean.txt"))


########################################################
# PART 2: FINAL R ANALYSIS CODE WITH QUEUE, SCENARIOS AND JAAMSIM COMPARISON
########################################################
cat("\n====================================\n")
cat("STARTING PART 2: FINAL SCENARIOS & JAAMSIM COMPARISON\n")
cat("====================================\n")

output_dir_2 <- "rio_final_outputs_capacity_scenarios_jaamsim_v4"
if(!dir.exists(output_dir_2)){ dir.create(output_dir_2, recursive = TRUE) }

capacity_345 <- data.frame(carrier_id = c("Carrier_A", "Carrier_B", "Carrier_C"), carrier_capacity = c(3, 4, 5), stringsAsFactors = FALSE)
capacity_456 <- data.frame(carrier_id = c("Carrier_A", "Carrier_B", "Carrier_C"), carrier_capacity = c(4, 5, 6), stringsAsFactors = FALSE)
capacity_567 <- data.frame(carrier_id = c("Carrier_A", "Carrier_B", "Carrier_C"), carrier_capacity = c(5, 6, 7), stringsAsFactors = FALSE)

peak_days_tr <- c("Pazartesi", "SalD1")
peak_extra_capacity_base <- 2      
peak_extra_capacity_balanced <- 1  

available_sheets_2 <- readxl::excel_sheets(global_file_path)

if(global_sheet_name %in% available_sheets_2){
  veri_raw <- readxl::read_excel(global_file_path, sheet = global_sheet_name)
} else {
  veri_raw <- readxl::read_excel(global_file_path, sheet = 1)
}

required_cols_2 <- c("order_purchase_interval", "order_approval_time", "carrier_dispatch_time", "delivery_transit_time")
missing_cols_2 <- setdiff(required_cols_2, names(veri_raw))

if(length(missing_cols_2) > 0){
  stop(paste("Missing required columns:", paste(missing_cols_2, collapse = ", ")))
}

date_candidates <- c("order_delivered_carrier_date", "carrier_handoff_date_real")
date_col <- date_candidates[date_candidates %in% names(veri_raw)][1]

if(is.na(date_col)){
  stop("Missing carrier handoff date column. Expected order_delivered_carrier_date or carrier_handoff_date_real.")
}

clean_numeric <- function(x){ x <- as.numeric(x); x <- x[is.finite(x)]; return(x) }

parse_to_date <- function(x){
  if(inherits(x, "Date")){ return(x) }
  if(inherits(x, "POSIXct") || inherits(x, "POSIXlt")){ return(as.Date(x)) }
  if(is.numeric(x)){ return(as.Date(x, origin = "1899-12-30")) }
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  out <- as.Date(rep(NA, length(x_chr)))
  
  date_formats <- c("%Y-%m-%d", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y", "%m/%d/%y %H:%M:%S", "%m/%d/%y %H:%M", "%m/%d/%y", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y")
  
  for(fmt in date_formats){
    idx <- is.na(out) & !is.na(x_chr)
    if(!any(idx)) break
    parsed <- suppressWarnings(as.POSIXct(x_chr[idx], format = fmt, tz = "UTC"))
    ok <- !is.na(parsed)
    if(any(ok)){ out[which(idx)[ok]] <- as.Date(parsed[ok]) }
  }
  
  serial_idx <- is.na(out) & !is.na(x_chr) & grepl("^[0-9]+(\\.[0-9]+)?$", x_chr)
  if(any(serial_idx)){ out[serial_idx] <- as.Date(as.numeric(x_chr[serial_idx]), origin = "1899-12-30") }
  return(out)
}

weekday_tr <- function(date_value){
  day_index <- as.POSIXlt(as.Date(date_value))$wday
  day_names <- c("Pazar", "Pazartesi", "SalD1", "C arE amba", "PerE embe", "Cuma", "Cumartesi")
  return(day_names[day_index + 1])
}

quality_check <- function(x, name){
  x_numeric <- as.numeric(x)
  data.frame(Variable = name, N_Total = length(x_numeric), Missing = sum(is.na(x_numeric)), Infinite = sum(is.infinite(x_numeric)), Zero = sum(x_numeric == 0, na.rm = TRUE), Negative = sum(x_numeric < 0, na.rm = TRUE), Positive = sum(x_numeric > 0, na.rm = TRUE), stringsAsFactors = FALSE)
}

summary_stats <- function(x, name, unit){
  x <- clean_numeric(x)
  if(length(x) == 0){ return(data.frame(Variable = name, Unit = unit, N = 0, Mean = NA, Median = NA, SD = NA, Variance = NA, Minimum = NA, Q1 = NA, Q3 = NA, Maximum = NA, Range = NA, Skewness = NA, Kurtosis = NA, Lower_Fence = NA, Upper_Fence = NA, Outlier_Count = NA, stringsAsFactors = FALSE)) }
  q1 <- as.numeric(quantile(x, 0.25))
  q3 <- as.numeric(quantile(x, 0.75))
  iqr_value <- IQR(x)
  lower_fence <- q1 - 1.5 * iqr_value
  upper_fence <- q3 + 1.5 * iqr_value
  outlier_count <- sum(x < lower_fence | x > upper_fence)
  data.frame(Variable = name, Unit = unit, N = length(x), Mean = mean(x), Median = median(x), SD = sd(x), Variance = var(x), Minimum = min(x), Q1 = q1, Q3 = q3, Maximum = max(x), Range = max(x) - min(x), Skewness = moments::skewness(x), Kurtosis = moments::kurtosis(x), Lower_Fence = lower_fence, Upper_Fence = upper_fence, Outlier_Count = outlier_count, stringsAsFactors = FALSE)
}

save_hist_density <- function(x, title, xlab, filename){
  x <- clean_numeric(x)
  p <- ggplot(data.frame(x = x), aes(x = x)) + geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "gray80", color = "black") + geom_density(linewidth = 1.1) + labs(title = title, x = xlab, y = "Density") + theme_minimal()
  ggsave(filename = file.path(output_dir_2, filename), plot = p, width = 8, height = 5, dpi = 300)
}

safe_shapiro <- function(x, name){
  x <- clean_numeric(x)
  if(length(x) >= 3){ print(shapiro.test(sample(x, min(length(x), 5000)))) }
}

safe_ad_test <- function(x, name){
  x <- clean_numeric(x)
  if(length(x) >= 8){ print(nortest::ad.test(x)) }
}

safe_fitdist <- function(x, dist_name){
  tryCatch({ suppressWarnings(fitdistrplus::fitdist(x, dist_name)) }, error = function(e){ return(NULL) })
}

extract_params <- function(fit_object){
  if(is.null(fit_object)){ return(NA) }
  params <- fit_object$estimate
  paste(paste(names(params), round(as.numeric(params), 6), sep = " = "), collapse = "; ")
}

fit_all_distributions <- function(x, variable_name, unit_name){
  x_fit <- clean_numeric(x)
  x_fit <- x_fit[x_fit > 0]
  if(length(x_fit) < 10){ return(NULL) }
  
  fits <- list(
    Normal = safe_fitdist(x_fit, "norm"),
    Exponential = safe_fitdist(x_fit, "exp"),
    Gamma = safe_fitdist(x_fit, "gamma"),
    Lognormal = safe_fitdist(x_fit, "lnorm"),
    Weibull = safe_fitdist(x_fit, "weibull")
  )
  fits <- fits[!sapply(fits, is.null)]
  
  if(length(fits) < 2){ return(NULL) }
  gof <- tryCatch({ fitdistrplus::gofstat(fits) }, error = function(e){ return(NULL) })
  if(is.null(gof)){ return(NULL) }
  
  ks_critical_005 <- 1.36 / sqrt(length(x_fit))
  ks_critical_001 <- 1.63 / sqrt(length(x_fit))
  best_aic_index <- which.min(as.numeric(gof$aic))
  best_bic_index <- which.min(as.numeric(gof$bic))
  
  summary_table <- data.frame(Variable = variable_name, Unit = unit_name, N_Fit = length(x_fit), Distribution = names(fits), AIC = as.numeric(gof$aic), BIC = as.numeric(gof$bic), KS_Statistic = as.numeric(gof$ks), KS_Critical_Alpha_0_05 = ks_critical_005, KS_Critical_Alpha_0_01 = ks_critical_001, Parameters = sapply(fits, extract_params), Best_By_AIC = seq_along(fits) == best_aic_index, Best_By_BIC = seq_along(fits) == best_bic_index, stringsAsFactors = FALSE)
  
  list(variable = variable_name, unit = unit_name, n_fit = length(x_fit), fits = fits, gof = gof, summary_table = summary_table, best_aic = names(fits)[best_aic_index], best_bic = names(fits)[best_bic_index])
}

paired_test_to_row <- function(test_object, comparison_name){
  data.frame(Comparison = comparison_name, T_Statistic = as.numeric(test_object$statistic), DF = as.numeric(test_object$parameter), P_Value = test_object$p.value, Mean_Difference = as.numeric(test_object$estimate), CI_Lower = test_object$conf.int[1], CI_Upper = test_object$conf.int[2], stringsAsFactors = FALSE)
}

veri <- veri_raw %>%
  mutate(
    order_purchase_interval = as.numeric(order_purchase_interval), order_approval_time = as.numeric(order_approval_time),
    carrier_dispatch_time = as.numeric(carrier_dispatch_time), delivery_transit_time = as.numeric(delivery_transit_time),
    arrival_min = order_purchase_interval, arrival_days = order_purchase_interval / 1440,
    approval_min = order_approval_time, approval_days = order_approval_time / 1440,
    dispatch_days = carrier_dispatch_time, transit_days = delivery_transit_time,
    carrier_handoff_date_real = parse_to_date(.data[[date_col]]), carrier_handoff_day_tr = weekday_tr(carrier_handoff_date_real)
  )

quality_table <- bind_rows(
  quality_check(veri$arrival_min, "Interarrival Time - Minutes"), quality_check(veri$approval_min, "Order Approval Time - Minutes"),
  quality_check(veri$approval_days, "Order Approval Time - Days"), quality_check(veri$dispatch_days, "Carrier Dispatch Time - Days"),
  quality_check(veri$transit_days, "Delivery Transit Time - Days")
)
write.csv(quality_table, file.path(output_dir_2, "01_data_quality_table.csv"), row.names = FALSE)

veri_clean <- veri %>%
  filter(is.finite(arrival_min), is.finite(approval_min), is.finite(dispatch_days), is.finite(transit_days), arrival_min >= 0, approval_min >= 0, dispatch_days >= 0, transit_days >= 0, !is.na(carrier_handoff_date_real))

if(!"carrier_id" %in% names(veri_clean)){
  q_1 <- quantile(veri_clean$transit_days, 1/3, na.rm = TRUE)
  q_2 <- quantile(veri_clean$transit_days, 2/3, na.rm = TRUE)
  veri_clean <- veri_clean %>% mutate(carrier_id = case_when(transit_days <= q_1 ~ "Carrier_A", transit_days <= q_2 ~ "Carrier_B", TRUE ~ "Carrier_C"))
} else {
  veri_clean <- veri_clean %>% mutate(carrier_id = as.character(carrier_id))
}

arrival_min <- veri_clean$arrival_min
arrival_days <- veri_clean$arrival_days
approval_min <- veri_clean$approval_min
approval_days <- veri_clean$approval_days
dispatch_days <- veri_clean$dispatch_days
transit_days <- veri_clean$transit_days

desc_table <- bind_rows(
  summary_stats(arrival_min, "Interarrival Time", "Minutes"), summary_stats(arrival_days, "Interarrival Time", "Days"),
  summary_stats(approval_min, "Order Approval Time", "Minutes"), summary_stats(approval_days, "Order Approval Time", "Days"),
  summary_stats(dispatch_days, "Carrier Dispatch Time", "Days"), summary_stats(transit_days, "Delivery Transit Time", "Days")
)
write.csv(desc_table, file.path(output_dir_2, "02_descriptive_statistics_input_variables.csv"), row.names = FALSE)

fit_arrival_min <- fit_all_distributions(arrival_min, "Interarrival Time", "Minutes")
fit_approval_min <- fit_all_distributions(approval_min, "Order Approval Time", "Minutes")
fit_dispatch_days <- fit_all_distributions(dispatch_days, "Carrier Dispatch Time", "Days")
fit_transit_days <- fit_all_distributions(transit_days, "Delivery Transit Time", "Days")

input_distribution_summary <- bind_rows(fit_arrival_min$summary_table, fit_approval_min$summary_table, fit_dispatch_days$summary_table, fit_transit_days$summary_table)
write.csv(input_distribution_summary, file.path(output_dir_2, "07_input_distribution_summary_table.csv"), row.names = FALSE)

best_input_distribution_table <- input_distribution_summary %>% filter(Best_By_AIC == TRUE) %>% select(Variable, Unit, N_Fit, Distribution, AIC, BIC, KS_Statistic, KS_Critical_Alpha_0_05, Parameters)
write.csv(best_input_distribution_table, file.path(output_dir_2, "08_best_input_distributions_by_aic.csv"), row.names = FALSE)

veri_clean <- veri_clean %>% mutate(historical_total_days = approval_days + dispatch_days + transit_days, historical_total_hours = historical_total_days * 24)
historical <- veri_clean$historical_total_days

build_queue_outputs <- function(data, capacity_table, scenario_label, peak_extra = 0, peak_days_tr = character(0)){
  data %>%
    mutate(original_row_id = row_number()) %>%
    select(-any_of(c("carrier_capacity_base", "peak_extra_capacity", "carrier_capacity", "daily_carrier_sequence", "daily_carrier_load", "capacity_exceeded", "queue_delay_days", "delayed_flag", "total_system_time_with_queue_days", "total_system_time_with_queue_hours"))) %>%
    left_join(capacity_table, by = "carrier_id") %>%
    rename(carrier_capacity_base = carrier_capacity) %>%
    mutate(peak_extra_capacity = ifelse(carrier_handoff_day_tr %in% peak_days_tr, peak_extra, 0), carrier_capacity = carrier_capacity_base + peak_extra_capacity, carrier_capacity = pmax(carrier_capacity, 1)) %>%
    arrange(carrier_id, carrier_handoff_date_real, original_row_id) %>%
    group_by(carrier_id, carrier_handoff_date_real) %>%
    mutate(daily_carrier_sequence = row_number(), daily_carrier_load = n(), capacity_exceeded = daily_carrier_load > carrier_capacity, queue_delay_days = floor((daily_carrier_sequence - 1) / carrier_capacity), queue_delay_days = ifelse(is.na(queue_delay_days), 0, queue_delay_days), delayed_flag = queue_delay_days > 0) %>%
    ungroup() %>%
    arrange(original_row_id) %>%
    mutate(total_system_time_with_queue_days = historical_total_days + queue_delay_days, total_system_time_with_queue_hours = total_system_time_with_queue_days * 24, queue_scenario_label = scenario_label)
}

queue_345 <- build_queue_outputs(veri_clean, capacity_345, "S0 Base Capacity 3-4-5")
queue_456 <- build_queue_outputs(veri_clean, capacity_456, "S1 Mild Capacity 4-5-6")
queue_567 <- build_queue_outputs(veri_clean, capacity_567, "S2 Stronger Capacity 5-6-7")
queue_345_peak <- build_queue_outputs(veri_clean, capacity_345, "S3 Peak-Day Overtime with Base 3-4-5", peak_extra = peak_extra_capacity_base, peak_days_tr = peak_days_tr)
queue_456_peak <- build_queue_outputs(veri_clean, capacity_456, "S4 Balanced Capacity 4-5-6 + Peak Overtime", peak_extra = peak_extra_capacity_balanced, peak_days_tr = peak_days_tr)

scenario_data <- data.frame(
  order_id = if("order_id" %in% names(veri_clean)) veri_clean$order_id else seq_len(nrow(veri_clean)),
  Benchmark_Historical_No_Queue = veri_clean$historical_total_days,
  S0_Base_Capacity_345 = queue_345$historical_total_days + queue_345$queue_delay_days,
  S1_Mild_Capacity_456 = queue_456$historical_total_days + queue_456$queue_delay_days,
  S2_Stronger_Capacity_567 = queue_567$historical_total_days + queue_567$queue_delay_days,
  S3_PeakDay_Overtime_345 = queue_345_peak$historical_total_days + queue_345_peak$queue_delay_days,
  S4_Balanced_456_Peak_Overtime = queue_456_peak$historical_total_days + queue_456_peak$queue_delay_days
)
write.csv(scenario_data, file.path(output_dir_2, "21_order_level_scenario_outputs_days.csv"), row.names = FALSE)

jaamsim_csv_path <- file.path(output_dir_2, "jaamsim_results.csv")
if(!file.exists(jaamsim_csv_path)){
  jaamsim_results <- data.frame(Scenario = c("S0_Base_Capacity_345", "S1_Mild_Capacity_456", "S2_Stronger_Capacity_567", "S3_PeakDay_Overtime_345", "S4_Balanced_456_Peak_Overtime"), JaamSim_Mean_Days = rep(NA_real_, 5), JaamSim_SD_Days = rep(NA_real_, 5), JaamSim_N_Replications = rep(NA_integer_, 5), stringsAsFactors = FALSE)
  write.csv(jaamsim_results, jaamsim_csv_path, row.names = FALSE)
}

cat("\n====================================\n")
cat("ALL ANALYSES COMPLETED SUCCESSFULLY\n")
cat("====================================\n")
