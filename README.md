## 📌 Overview

This repository contains the analysis and recommendations developed for a take-home assignment as part of the **Validere Data Consultant** application process. The project focuses on:
- Understanding the behavior and needs of mom-and-pop investors
- Evaluating and comparing investment strategies suitable for financial advisors serving retail clients
- Simulating and optimizing portfolio performance through backtesting (maximizing profit and minimizing risk subject to client-specific constraints)
- Communicating insights to support advisor decision-making and client education
  
## 📂 Structure

| Folder / File      | Description                                     |
|--------------------|-------------------------------------------------|
| `preprocessing/`   | Scripts for retrieving and preprocessing data   |
| `analysis/`        | Portfolio simulation and strategy evaluation    |
| `slides/`          | Final results and analysis slide deck           |
| `utils/`           | Helper functions (e.g., IRR calculation, backtesting) |
| `README.md`        | You're here                                     |

## 🚀 How to Run

All analysis was done in R. To reproduce the results:
1. Clone the repo
2. Install required packages (e.g., tidyverse, xts, PerformanceAnalytics)
3. Run `preprocessing/eda.qmd` and `analysis/portfolio_sim.qmd` blocks sequentially, or render into `.html` files.
