# EV Population Data Analysis - Washington State 

This repository contains analytical memos and R scripts developed for a comprehensive analysis of the Electric Vehicle (EV) population dataset provided by the Washington State Department of Licensing (DOL). The project employs various descriptive and inferential statistical methods to uncover insights about EV adoption trends, factors influencing adoption, and potential correlations.

## Project Overview
The primary objectives of this project include:
- Understanding the distribution of EV populations across various demographics and regions.
- Identifying factors influencing EV adoption.
- Performing statistical tests to validate hypotheses and uncover relationships within the data.

## Methodology
The analysis involves the following steps:

1. **Univariate Analysis**
   - Descriptive statistics (mean, median, mode, variance, etc.)
   - Distribution visualizations (histograms, bar charts, etc.)

2. **Bivariate Analysis**
   - Cross-tabulations and comparisons
   - Correlation analysis
   - Visualization of relationships (scatter plots, box plots, etc.)

3. **Trivariate Analysis**
   - Analysis involving three variables to uncover deeper relationships.
   - Use of color-coded visualizations to show additional dimensions.

4. **Statistical Tests**
   - **Chi-Square Test**: To test for independence between categorical variables.
   - **T-Test**: To compare means between two groups.

5. **Regression Analysis**
   - **Simple Linear Regression (OLS)**: To evaluate the relationship between a dependent variable and one independent variable.
   - **Multiple Linear Regression (OLS)**: To evaluate relationships involving multiple independent variables.

## Repository Structure
```
├── data/                    # Contains the raw and cleaned datasets
├── scripts/                 # R scripts for various analyses
├── memos/                   # Analytical memos detailing insights
├── figures/                 # Visualizations generated during analysis
├── results/                 # Output files (e.g., regression summaries)
└── README.md                # Project overview and instructions
```

## Requirements
- **R**
- Required R libraries:
  - `dplyr`
  - `ggplot2`
  - `readr`
  - `car`
  - `tidyr`

## Usage
1. Clone the repository:
   ```bash
   git clone https://github.com/dheerajshetty07/ev-population-analysis.git
   ```
2. Install the required R packages listed above.
3. Run the scripts in the `scripts/` directory sequentially for replicating the analysis.

## Results and Insights
Key findings from the analysis include:
- Demographic and geographic trends in EV adoption.
- Statistical evidence supporting factors that influence EV adoption.
- Predictive models for understanding EV population growth.

For detailed insights, refer to the analytical memos in the `memos/` directory.

## Data Source
The data used in this project is sourced from the [Washington State Department of Licensing (DOL)](https://catalog.data.gov/dataset/electric-vehicle-population-data)).

## Contributions
Contributions are welcome! Feel free to submit a pull request or open an issue for suggestions or improvements.

---

For any questions, please contact [Dheeraj Shetty](mailto:dheerajshetty07@gmail.com).
