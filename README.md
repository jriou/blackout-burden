# Code for: *Excess mortality attributable to the 2025 Iberian Peninsula blackout*

`R` code to reproduce the analyses in *Excess mortality attributable to the 2025 Iberian Peninsula blackout* (Konstantinoudis & Riou, 2026). 

## What this repo does
- Builds daily expected (counterfactual) all-cause mortality for Portugal and Spain using Bayesian spatiotemporal models (R-INLA).
- Estimates excess deaths for the blackout week **28 Apr–4 May 2025**, including lag windows (lag0, lag0–2, lag0–6).
- Produces the main tables/figures (overall + by age, sex, and NUTS2 region). 

