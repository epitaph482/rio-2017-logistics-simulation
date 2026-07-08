# RIO 2017 Logistics Simulation Project

## Overview
This repository contains the R-side data analysis and JaamSim resource-based simulation models for the RIO 2017 logistics project. The project evaluates system performance under capacity-constrained carrier queue scenarios using a dataset of 2,545 validated orders.

## Project Structure
- **/data**: Contains the source dataset (`Full Data All Orders.csv`).
- **/scripts**: Contains the integrated R analysis script (`main_analysis.R`) which handles:
    - Input distribution fitting (Interarrival, Approval, Dispatch, Transit times).
    - Queue delay modeling under various capacity scenarios.
    - Statistical validation of scenario outputs.
- **/jaamsim**: Contains `.cfg` files for JaamSim simulations (S0, S1, S2 scenarios).
- **/outputs**: Contains generated CSV reports and PNG visualizations for the final project report.

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
1. Ensure you have the `data/` folder populated with the project dataset.
2. Run the `main_analysis.R` script in RStudio.
3. The script will automatically generate the required output files in the designated directory.

## Validation
The JaamSim models were validated against the R-side queue analysis, achieving a validation error rate of approximately 7.5-7.8%, confirming the consistency of our capacity-constrained queue model.

---
*This project was developed for the Logistics Simulation course requirements.*
