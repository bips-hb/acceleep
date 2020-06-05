# acceleep

> **Accel**erometry-based **E**nergy **E**xpenditure **P**redection (using D**eep** Learning)

Preliminary thesis title: *"Accelerometry-Based Prediction of Energy Expenditure in Preschoolers Using Deep Learning"*

<!-- badges: start -->
[![R build status](https://github.com/bips-hb/acceleep/workflows/R-CMD-check/badge.svg)](https://github.com/bips-hb/acceleep/actions)
<!-- badges: end -->

Associated R code for experimentation.  
Presumably also containing the actual analysis code for the master's thesis at a later date.

This repository is technically a valid R package to enable the documentation of helper functions, but will contain non-standard folders which are noted in `.Rbuildignore` to not interfere with the package building/installation process.

Additional folders with their intended purposes:

- [`simulated-data`](simulated-data): Simulation of structurally similiar accelerometry data, reshaping to keras/tensorflow-compatible shapes for experimentation. (The "dry run"-stage)
- [`modelling`](modelling): Will contain modelling code, i.e. keras models, output if possible, etc.
