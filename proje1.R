########################################################
# SEPARATE CODE
# QUEUE-ADJUSTED PRE-CARRIER HANDOFF TIME ANALYSIS
# Metric: carrier_dispatch_time + queue_delay_days
########################################################

# ------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------

packages <- c("readxl", "ggplot2")

missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

if(length(missing_packages) > 0){
  install.packages(missing_packages)
}

library(readxl)
library(ggplot2)

# ------------------------------------------------------
# 2. READ EXCEL FILE
# ------------------------------------------------------

# Dosya se??me penceresi a????lacak.
# ??unu se??:
# rio_2017_queue_capacity_3_4_5_detailed_clarified.xlsx

file_path <- file.choose()

sheet_name <- "Full Data All Orders"

available_sheets <- readxl::excel_sheets(file_path)

if(!(sheet_name %in% available_sheets)){
  sheet_name <- available_sheets[1]
}

df <- readxl::read_excel(file_path, sheet = sheet_name)
df <- as.data.frame(df)

cat("\n====================================\n")
cat("DATA READ\n")
cat("====================================\n")
cat("Used sheet:", sheet_name, "\n")
cat("Rows before cleaning:", nrow(df), "\n")

# ------------------------------------------------------
# 3. REQUIRED COLUMN CHECK
# ------------------------------------------------------

required_cols <- c(
  "order_purchase_interval",
  "order_approval_time",
  "carrier_dispatch_time",
  "delivery_transit_time",
  "queue_delay_days"
)

missing_cols <- setdiff(required_cols, names(df))

if(length(missing_cols) > 0){
  stop(paste("Eksik kolon var:", paste(missing_cols, collapse = ", ")))
}

# ------------------------------------------------------
# 4. NUMERIC CONVERSION
# ------------------------------------------------------

df$order_purchase_interval <- as.numeric(df$order_purchase_interval)
df$order_approval_time <- as.numeric(df$order_approval_time)
df$carrier_dispatch_time <- as.numeric(df$carrier_dispatch_time)
df$delivery_transit_time <- as.numeric(df$delivery_transit_time)
df$queue_delay_days <- as.numeric(df$queue_delay_days)

# Queue delay NA ise 0 kabul ediyoruz.
# ????nk?? kuyruk gecikmesi olmayan sipari??lerde bo?? de??er 0 anlam??na gelir.
df$queue_delay_days[is.na(df$queue_delay_days)] <- 0

# ------------------------------------------------------
# 5. CLEANING
# ------------------------------------------------------

rows_before_cleaning <- nrow(df)

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

rows_after_cleaning <- nrow(df)

cat("\n====================================\n")
cat("CLEANING SUMMARY\n")
cat("====================================\n")
cat("Rows before cleaning:", rows_before_cleaning, "\n")
cat("Rows after cleaning:", rows_after_cleaning, "\n")
cat("Removed rows:", rows_before_cleaning - rows_after_cleaning, "\n")

# ------------------------------------------------------
# 6. CREATE MAIN METRICS
# ------------------------------------------------------

# Ana metrik:
# Kargoya verilmeden ??nceki kuyruk dahil toplam s??re
#
# pre_carrier_handoff_days =
# carrier_dispatch_time + queue_delay_days

df$pre_carrier_handoff_days <-
  df$carrier_dispatch_time + df$queue_delay_days

df$pre_carrier_handoff_hours <-
  df$pre_carrier_handoff_days * 24

# Sipari?? onay?? dahil kargoya verilme s??resi:
#
# order_to_carrier_handoff_days =
# order_approval_time / 1440 + carrier_dispatch_time + queue_delay_days

df$order_to_carrier_handoff_days <-
  (df$order_approval_time / 1440) +
  df$carrier_dispatch_time +
  df$queue_delay_days

df$order_to_carrier_handoff_hours <-
  df$order_to_carrier_handoff_days * 24

# Kontrol
cat("\n====================================\n")
cat("METRIC CHECK\n")
cat("====================================\n")
cat("Minimum pre-carrier handoff days:",
    min(df$pre_carrier_handoff_days, na.rm = TRUE), "\n")
cat("Maximum pre-carrier handoff days:",
    max(df$pre_carrier_handoff_days, na.rm = TRUE), "\n")

# ------------------------------------------------------
# 7. OUTPUT FOLDER
# ------------------------------------------------------

output_dir <- "pre_carrier_handoff_outputs_clean"

if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

# ------------------------------------------------------
# 8. SUMMARY TABLE
# ------------------------------------------------------

delayed_values <- df$queue_delay_days[df$queue_delay_days > 0]

mean_queue_delay_delayed <- if(length(delayed_values) > 0){
  mean(delayed_values, na.rm = TRUE)
} else {
  0
}

summary_table <- data.frame(
  Metric = c(
    "N orders",
    "Mean pre-carrier handoff days",
    "Median pre-carrier handoff days",
    "SD pre-carrier handoff days",
    "Minimum pre-carrier handoff days",
    "Q1 pre-carrier handoff days",
    "Q3 pre-carrier handoff days",
    "Maximum pre-carrier handoff days",
    "Mean pre-carrier handoff hours",
    "Median pre-carrier handoff hours",
    "Mean order-to-carrier handoff days",
    "Median order-to-carrier handoff days",
    "Queue delayed orders",
    "Queue delay rate percent",
    "Mean queue delay days - all orders",
    "Mean queue delay days - delayed orders only",
    "Maximum queue delay days"
  ),
  Value = c(
    nrow(df),
    mean(df$pre_carrier_handoff_days, na.rm = TRUE),
    median(df$pre_carrier_handoff_days, na.rm = TRUE),
    sd(df$pre_carrier_handoff_days, na.rm = TRUE),
    min(df$pre_carrier_handoff_days, na.rm = TRUE),
    as.numeric(quantile(df$pre_carrier_handoff_days, 0.25, na.rm = TRUE)),
    as.numeric(quantile(df$pre_carrier_handoff_days, 0.75, na.rm = TRUE)),
    max(df$pre_carrier_handoff_days, na.rm = TRUE),
    mean(df$pre_carrier_handoff_hours, na.rm = TRUE),
    median(df$pre_carrier_handoff_hours, na.rm = TRUE),
    mean(df$order_to_carrier_handoff_days, na.rm = TRUE),
    median(df$order_to_carrier_handoff_days, na.rm = TRUE),
    sum(df$queue_delay_days > 0, na.rm = TRUE),
    mean(df$queue_delay_days > 0, na.rm = TRUE) * 100,
    mean(df$queue_delay_days, na.rm = TRUE),
    mean_queue_delay_delayed,
    max(df$queue_delay_days, na.rm = TRUE)
  )
)

cat("\n====================================\n")
cat("PRE-CARRIER HANDOFF SUMMARY\n")
cat("====================================\n")
print(summary_table)

write.csv(
  summary_table,
  file.path(output_dir, "pre_carrier_handoff_summary_clean.csv"),
  row.names = FALSE
)

# ------------------------------------------------------
# 9. QUEUE DELAY FREQUENCY TABLE
# ------------------------------------------------------

queue_delay_frequency <- as.data.frame(table(df$queue_delay_days))
names(queue_delay_frequency) <- c("Queue_Delay_Days", "Frequency")

queue_delay_frequency$Queue_Delay_Days <-
  as.numeric(as.character(queue_delay_frequency$Queue_Delay_Days))

queue_delay_frequency$Percent <-
  queue_delay_frequency$Frequency /
  sum(queue_delay_frequency$Frequency) * 100

cat("\n====================================\n")
cat("QUEUE DELAY FREQUENCY TABLE\n")
cat("====================================\n")
print(queue_delay_frequency)

write.csv(
  queue_delay_frequency,
  file.path(output_dir, "queue_delay_frequency_clean.csv"),
  row.names = FALSE
)

# ------------------------------------------------------
# 10. SAVE UPDATED DATA
# ------------------------------------------------------

write.csv(
  df,
  file.path(output_dir, "data_with_pre_carrier_handoff_time_clean.csv"),
  row.names = FALSE
)

# ------------------------------------------------------
# 11. HISTOGRAM:
# PRE-CARRIER HANDOFF TIME
# ------------------------------------------------------

p_hist <- ggplot(df, aes(x = pre_carrier_handoff_days)) +
  geom_histogram(
    bins = 40,
    fill = "gray80",
    color = "black"
  ) +
  labs(
    title = "Distribution of Queue-Adjusted Pre-Carrier Handoff Time",
    subtitle = "Metric: carrier_dispatch_time + queue_delay_days",
    x = "Pre-carrier handoff time (days)",
    y = "Number of orders"
  ) +
  theme_minimal()

print(p_hist)

ggsave(
  filename = file.path(output_dir, "pre_carrier_handoff_histogram_clean.png"),
  plot = p_hist,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------
# 12. BOXPLOT:
# PRE-CARRIER HANDOFF TIME
# ------------------------------------------------------

p_box <- ggplot(df, aes(y = pre_carrier_handoff_days)) +
  geom_boxplot(
    fill = "gray80",
    color = "black"
  ) +
  labs(
    title = "Boxplot of Queue-Adjusted Pre-Carrier Handoff Time",
    subtitle = "Metric: carrier_dispatch_time + queue_delay_days",
    y = "Pre-carrier handoff time (days)"
  ) +
  theme_minimal()

print(p_box)

ggsave(
  filename = file.path(output_dir, "pre_carrier_handoff_boxplot_clean.png"),
  plot = p_box,
  width = 6,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------
# 13. HISTOGRAM:
# ORDER-TO-CARRIER HANDOFF TIME
# ------------------------------------------------------

p_order_to_carrier_hist <- ggplot(df, aes(x = order_to_carrier_handoff_days)) +
  geom_histogram(
    bins = 40,
    fill = "gray80",
    color = "black"
  ) +
  labs(
    title = "Distribution of Queue-Adjusted Order-to-Carrier Handoff Time",
    subtitle = "Metric: order_approval_time / 1440 + carrier_dispatch_time + queue_delay_days",
    x = "Order-to-carrier handoff time (days)",
    y = "Number of orders"
  ) +
  theme_minimal()

print(p_order_to_carrier_hist)

ggsave(
  filename = file.path(output_dir, "order_to_carrier_handoff_histogram_clean.png"),
  plot = p_order_to_carrier_hist,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------
# 14. REPORT NOTE
# ------------------------------------------------------

report_note <- c(
  "Queue-adjusted pre-carrier handoff time was calculated as carrier_dispatch_time + queue_delay_days.",
  "This metric represents the total time before an order is handed over to the carrier under the capacity-constrained queue model.",
  "Order-to-carrier handoff time was also calculated as order_approval_time / 1440 + carrier_dispatch_time + queue_delay_days.",
  "Queue delay is not treated as a separate fitted input distribution.",
  "It is generated by the daily carrier capacity rule and analyzed as an output of the queue model.",
  "Because carrier capacity was defined as a daily capacity, queue delay was measured in days rather than minutes."
)

writeLines(
  report_note,
  file.path(output_dir, "pre_carrier_handoff_report_note_clean.txt")
)

# ------------------------------------------------------
# 15. DONE
# ------------------------------------------------------

cat("\n====================================\n")
cat("DONE\n")
cat("====================================\n")
cat("Outputs saved in folder:", output_dir, "\n")
cat("Use these files in the report:\n")
cat("- pre_carrier_handoff_summary_clean.csv\n")
cat("- queue_delay_frequency_clean.csv\n")
cat("- pre_carrier_handoff_histogram_clean.png\n")
cat("- pre_carrier_handoff_boxplot_clean.png\n")
cat("- order_to_carrier_handoff_histogram_clean.png\n")