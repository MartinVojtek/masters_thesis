# Master's thesis
## Portfolio implications of persistent shifts in the real interest rate
COMENIUS UNIVERSITY, BRATISLAVA   <br>
FACULTY OF MATHEMATICS, PHYSICS AND INFORMATICS <br>
Department of Applied Mathematics and Statistics <br>
Mathematical Economics, Finance and Modelling

Thesis anotation: <em>Quantification of the utility loss from ignoring persistent variation in the real
interest rate (reinvestment risk). One of the key contributions of the thesis
is to develop and implement a simulation setup with the help of which we
quantify the utility loss an investor would incur when ignoring the persistent
variation in real interest rates. The simulation setup allows us to consider various
configurations of the real rate process</em>.
This repository allows a full replication of our master's thesis results.
The repositoriry is divided into 2 folders.

<h4>1. Data</h4>
Data used in codes below, with source links. Involves own calculations.

- US_GDP.xlsx - https://fred.stlouisfed.org/series/GDPC1
- cyclical_part_of_real_rate.xlsx - T-BILLS: https://fred.stlouisfed.org/series/TB3MS , inflation expectations: https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/atsix
- TermPremiumCalibration.xlsx - file describing estimation of Term Premium based on outputs of the simulation
- HLW_GDP_in_world.xlsx - https://www.newyorkfed.org/research/policy/rstar
- HLW_rstar_in_world.xlsx - same as HLW_GDP_in_world.xlsx
- 2yTIPSvs3mATIPS.xlsx - https://www.federalreserve.gov/pubs/feds/2008/200805/200805abs.html
- US_CPI.xlsx - https://fred.stlouisfed.org/series/CPIAUCSL
- nss_par_data.xlsx - same as 2yTIPSvs3mATIPS.xlsx

<h4>2. Codes</h4>
Simulation procedure and explanations are thoroughly described in the thesis. All codes listed below can be run separately.

- simulation.r - main code of the thesis, runs simulations of r-star, estimates bond yields
and bond returns, constructs a 3D yield curve. All of the research results are based
on this file.
- RMSE_TermPremium.r - code used to estimate \sigma and \beta in calibration of term
premium of interest rates
- TIPS_comparison.r - code used to compare 2-year TIPS and 3-month artificial TIPS
- plot_HLW_world_GDP.r - code used to plot the historical trend of GDP growth worldwide
- plot_HLW_world_rstar.r - code used to plot the estimate of historical r-star worldwide, based on Holston-Laubach-Williams (2016)
- plot_US_GDP.r - code used to plot and emphasize the pandemic GDP growth outliers
- historical_returns.r - code used to calculate historical nominal and real returns on
treasury securities based on bond indices, underlying data are not included as that would
violate copyrights
- nss.r - code used to estimate historical real yield curve using Nelson-Siegel-Svensson functional form
and calculate returns based off of it

Codes linkage to specific results in the thesis are provided in thesis's appendix.
