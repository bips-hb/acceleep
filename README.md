# acceleep

> **Accel**erometry-based **E**nergy **E**xpenditure **P**redection (using D**eep** Learning)

Thesis title: *"Accelerometry-Based Prediction of Energy Expenditure in Preschoolers Using Deep Learning"*

<!-- badges: start -->
<!-- badges: end -->

Associated R code for experimentation and model development.  
The project is a valid R package, meaning you can `devtools::install()` it to load utility functions for analysis.

This repository is technically a valid R package to enable the documentation of helper functions, but will contain non-standard folders which are noted in `.Rbuildignore` to not interfere with the package building/installation process.

## Primary Result / Final Models

The final models can be found in [`output/models`](output/models).

Models are stored in [HDF5](https://en.wikipedia.org/wiki/Hierarchical_Data_Format) and using the following naming convention:

```
final-<network-type>-<Resolution>Hz-<accelerometer model>-<placement>-<outcome unit>-<timestamp>.hdf5
```

For example `final-CNN-100Hz-actigraph-hip_left-Jrel-20201029123026.hdf5`

## Project Structure

- [`R`](R) and [man](man): Utility functions for data preparation, analysis, model development, with their generated documentation.
- [`simulated-data`](simulated-data): Simulation of structurally similiar accelerometry data, reshaping to keras/tensorflow-compatible shapes for experimentation. (The "dry run"-stage)
- [`data-cleaning`](data-cleaning): Code to read the raw `.csv` accelerometry and spirometry data, merge them, and save them as more space-efficient `.rds` files.
- [`modelling`](modelling): Modelling code, i.e. keras models, output if possible, etc.

- [holdout-validation](holdout-validation), [cross-validation](cross-validation), [cross-validation-full](cross-validation-full): Code for final model evaluation runs.
- [final-model-fit](final-model-fit): Code to fit final models on all available data.

- [`output`](output): Intermediate output for data description, including summary data.
