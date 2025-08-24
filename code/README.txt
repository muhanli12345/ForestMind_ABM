Repository Contents

This repository contains scripts and data files documenting the development of an agent-based model (ABM) designed to investigate the foraging decision-making processes of primates.

01_ABM_movement_rules.R

An R script that executes ABM simulations. This file currently implements and tests movement rules and is undergoing further refinement.

02_Function_ABM.R

An R script containing modular functions used within the ABM framework. These functions specify the behavioral rules governing agent movement, interactions with environmental features, and the dynamics of the focal primate "avatar."

envlandmark.csv

A structured dataset describing the spatial distribution and attributes of environmental landmarks, including fruit-bearing trees. These data serve as input for initializing the ABM environment.

data_analysis.zip

An archive containing all scripts and datasets required for post-simulation analyses of ABM outputs. This includes routines for evaluating movement trajectories, resource encounter rates, and decision-making outcomes.
