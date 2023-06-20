# Pricing Simulation for Petrol Stations

## Introduction

This is an application to simulate sales for petrol stations.  It uses a very simple setup where each station has a single competitor.  Each day, a number of cars pass the two stations.  Each driver cares about price, whether a petrol station has a coffee machine or not, and has a (random) personal preference that also effects their decision.  

Based on the prices at the two stations, presence of a coffee machine and their random personal preference, drivers decide which one to visit.  Apart from their random personal preference, every driver is the same.  

Your own petrol stations set prices by drawing randomly from a uniform distribution.  Competitors do the same.

It is also possible to set petrol stations to be "profit maximising", if a set of optimal prices is given for a set of competitor prices.

## Parameters

The user can specify the following parameters:

 - number of days (T): how many days to run the simulation for
 - number of cars passing per day (n_per_day): number of cars passing each station/competitor
 - number of petrol station sites (sites)
 - upper and lower bounds of prices for each of their own stations
 - upper and lower bounds for competitor stations (which are all the same)
 - which stations have a coffee machine
 - whether a station is going to be profit maximising (requires profit-maximising prices, see below)
 - a set of profit-maximising prices, for a set of competitor prices
 - coefficient in consumer utility function for price
 - coefficient in consumer utility function for coffee machine
