# Cubic Zirconia Multiple Regression

Cubic zirconia is a synthetic gemstone that is well known for its affordability and aesthetics that
resemble real diamonds. The price of cubic zirconia is influenced by a combination of physical
and qualitative characteristics, hence, there is a need to understand how these attributes interact
and contribute to its value. This report aims to analyse the statistical relationships between these
attributes and the price of cubic zirconia.

The dataset contains 26,967 observations, with 10 recorded variables describing various attributes of cubic zirconia:
- Response Variable:
  - Price: The price of cubic zirconia.
- Covariates:
  - Carat: A measure of the gemstone's weight.
  - Cut: Quality of the cut (Fair, Good, Very Good, Premium, Ideal).
  - Colour: Graded: D, E, F, G, H, I, J.
  - Clarity: Graded: FL, IF, VVS2, VS1, VS2, SI1, SI2, I1, I2, I3
  - Depth: The gemstone's height from bottom tip to top flat surface.
  - Table: The width of the top flat surface.
  - Dimensions: Length (X), width (Y), and height (Z).
  
Cut, Quality, and Clarity are categorical variables and Carat, Depth, Table, and Dimension columns are continuous variables.

This analysis addresses the following:
1. Which factors significantly influence the price of cubic zirconia?
2. How do covariates interact with one another and contribute to price variation?
3. How can multicollinearity among covariates be addressed to build an interpretable model?

By focusing on inference, this report aims to provide actionable insights for manufacturers and retailers to optimize pricing strategies and understand value drivers in the gemstone market.

Our biggest challenge in this project was dealing appropriately with multicollinearity between our covariates. After the end of the term, I went back to this project to try solving our multicollinearity problem using PCA.
