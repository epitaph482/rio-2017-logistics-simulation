# RIO 2017 Logistics Simulation Project

## Overview
This repository contains the R-side data analysis and JaamSim resource-based simulation models for the RIO 2017 logistics project. The project evaluates system performance under capacity-constrained carrier queue scenarios using a dataset of 2,545 validated orders.

## Project Structure
```
.
├── data/
│   └── rio_2017_queue_capacity_3_4_5_detailed_clarified.xlsx   # Source dataset (sheet: "Full Data All Orders")
├── scripts/
│   └── proje1.R                                                  # Combined R analysis script
├── simulation/
│   ├── RIO_2017_JaamSim_ABC_Resource_S0_345.cfg                  # Scenario S0: Base capacity (3-4-5)
│   ├── RIO_2017_JaamSim_ABC_Resource_S1_456.cfg                  # Scenario S1: Mild capacity (4-5-6)
│   └── RIO_2017_JaamSim_ABC_Resource_S2_567.cfg                  # Scenario S2: Stronger capacity (5-6-7)
└── docs/
    └── RIO_2017_Logistics_Simulation_Project_Report_FINAL_WITH_JAAMSIM.pdf
```

- **/data**: Contains the source dataset, an Excel workbook with the `Full Data All Orders` sheet.
- **/scripts**: Contains the integrated R analysis script (`proje1.R`), which runs in two parts:
    - **Part 1 — Pre-Carrier Handoff Analysis:** cleans the data and computes queue-adjusted pre-carrier handoff time (`carrier_dispatch_time + queue_delay_days`).
    - **Part 2 — Final Scenarios & JaamSim Comparison:** input distribution fitting (Interarrival, Approval, Dispatch, Transit times), scenario-based queue delay calculation, and statistical validation against JaamSim outputs.
- **/simulation**: Contains the JaamSim `.cfg` files for scenarios S0, S1, and S2 (Seize/Release resource-based queue models).
- **/docs**: Contains the final project report (PDF).

> Note: running `scripts/proje1.R` generates its own output folders (`pre_carrier_handoff_outputs_clean/` and `rio_final_outputs_capacity_scenarios_jaamsim_v4/`) with CSV reports and PNG visualizations. These are not committed to the repo — they're recreated each time the script runs.

## Methodology
The simulation approach integrates two main components:
1. **R Analysis:** Data cleaning, descriptive statistics, and scenario-based queue delay calculation.
2. **JaamSim Implementation:** Resource-based queue modeling (Seize/Release logic) for capacity scenarios (S0: 3-4-5, S1: 4-5-6, S2: 5-6-7).

## Scenarios
| Scenario | Description |
| :--- | :--- |
| **S0** | Base Capacity (3-4-5) |
| **S1** | Mild Capacity (4-5-6) |
| **S2** | Stronger Capacity (5-6-7) |
| **S3/S4** | Policy extensions with peak-day overtime logic |

## How to Run
1. Make sure `data/rio_2017_queue_capacity_3_4_5_detailed_clarified.xlsx` is present under `data/` (already included in this repo).
2. Open `scripts/proje1.R` in RStudio and run it — required packages (`readxl`, `ggplot2`, `fitdistrplus`, `moments`, `nortest`, `dplyr`, `tidyr`, `gridExtra`) are installed automatically if missing.
3. The script generates output folders with CSVs and PNGs relative to your working directory (see note above).
4. To run the JaamSim models, open the `.cfg` files under `simulation/` in [JaamSim](https://jaamsim.com/).

## Validation
The JaamSim models were validated against the R-side queue analysis, achieving a validation error rate of approximately 7.5-7.8%, confirming the consistency of our capacity-constrained queue model.

---
*This project was developed for the Logistics Simulation course requirements.*
