# D-Optimal Experimental Design Shiny App

This repository provides an interactive **Shiny web application** for generating and evaluating **D-optimal experimental designs** with support for **categorical factors, blocking, and random effects**.

---

## 📦 Features

- Generate D-optimal designs for:
  - Numerical and categorical factors
  - Multiple blocking structures (fixed or random)
- Compute and visualize:
  - D-, A-, and G-efficiencies
  - Power analysis for each coefficient
  - ANOVA tables (fixed and random effects)
  - Block-level comparisons via plot
- Export results to Excel

---

## 🧠 Core Logic

### `rutgers.optimal.cat.blocks.random.ver.1.R`

Implements a flexible function `rutgers.optimal.cat.blocks.random()` to search for D-optimal designs using a coordinate-exchange-like algorithm with multiple random starts. Supports:
- Custom formulas for main effects and interactions
- Categorical and numerical variables
- Fixed or random block effects
- Efficiency calculations: **D-efficiency, A-efficiency, G-efficiency**
- Power calculations via the non-central F distribution

### `server.R`

Manages:
- Shiny reactivity logic for user inputs
- Execution of the optimal design generator
- Evaluation of post-hoc experiments using simulated responses
- ANOVA and mixed-effects modeling
- Result export and plot rendering

### `ui.R`

Builds the user interface with:
- Inputs for design generation
- Experimental controls (simulated response generation)
- Tabs to display results:
  - Design matrix
  - Efficiency metrics
  - Power table
  - ANOVA tables
  - Block estimates visualization

---

## 🚀 Getting Started

### Prerequisites

Install required R packages:

```r
install.packages(c("shiny", "shinycssloaders", "ggplot2", "tibble", "openxlsx", "lme4", "car", "gtools"))
