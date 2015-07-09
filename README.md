HalfLifeCalcs
=============

R scripts to calculate and visulise degradation of toxicants in a semi-static exposure

This repo contains R scripts that produce data.frames and plots that show changes in concentration through time without and with regular water and replacements.

####Plots
Plots are constructed using the base graphics package

####Variables

Variable | Definition | Units | Default
---------|------------|-------|--------
y0 |starting concentration |micro g/L |NA
yt |concentration at unknown time t |micro g/L |NA
tHalf |half-life |days |NA
t |total time of test |days |NA
rInt |replacement interval |days |NA
v0 |Initial volume |Litres (L) |1.5L
rVol |volume to be replaced |Litres (L) |50 % of original
rConc |Concentration of replacement volume |micro g/L |y0

Notes on Units
1. Concentration units can be varied but must be kept equivalent
2. time units can be varied but must be kept equivalent
3.  replacement interval must be intergers (no part intervals e.g., 1.5 days will not work)

Author = CR
Date = Jul 9 2015
