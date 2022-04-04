# Princeton Senior Thesis

## About
We build a market impact model for limit order book simulators and test order execution strategies formulated after optimizing against this model.

## Contents
timetrader.R: Contains functions that execute the time interval strategy through a basic LOB simulator.   
intervaltrader.R: Contains functions that execute the trade interval strategy through a basic LOB simulator.   
revisions.R: Contains the functions to construct time series, calibrate VAR model, design new execution strategy, and apply revisions to executed trades.   
simulator_v3.R: Contains all the simulating, testing, and plotting (main).   
