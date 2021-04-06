# Nonlinear Kernel Methods: Theoretical aspects and robust extensions

This project includes the supplementary R code for my Bachelor's thesis about kernel methods and nonlinear kernel regression. Using this code you can recreate my simulations and comparisons of three different robust kernel methods. To get an overview of the thesis you can read the following abstract. If you for some reason want to read the whole thesis, feel free to contact me ;)

## Abstract

Kernel methods have been an active area of research since they were first used in the context of statistical learning and are still widely used throughout different machine learning tasks. This thesis focuses on the fundamental theory of kernel methods as well as their application in the context of robust nonlinear regression. First, we focus on the theory behind kernel methods and how the kernel trick functions. While kernel based methods like support vector machines are often applied in data science or machine learning, the fundamental principle is not always understood to full extend. We try to explain the main idea of kernel methods and why they are popular in statistical learning. We derive the basic characterisations of kernel functions as well as some fundamental theory about reproducing kernel Hilbert spaces. Furthermore, we derive the kernel trick in the context of support vector machines and kernel ridge regression.
Not only in kernel methods, but in general, data often suffers from contamination like outliers or noise. In the second part of this thesis we introduce several methods that are robust to outliers in the context of kernel regression. These methods are able to estimate the underlying function from the data, even if the data is heavily inflicted by outliers. Using a simulation experiment, we generate several datasets contaminated by outliers and noise that differ in parameters like the strength and percentage of outliers present. Each of the aforementioned methods is used to predict data, while under the influence of outliers, and the respective performance is measured. Using the simulation we are able to determine which methods work best for different outlier parameters. Our results suggest that robust kernel regression methods show a favourable behavior in the context of outliers, compared to the regular kernel ridge regression.

## Simulation

Includes all functions and code in order to replicate the simulation studies of my thesis.

## Data

The data was generated using a customized simulation as described in the thesis.

## Dashboard

All results can be visualized using a R shiny dashboard. This includes results from the comparison of the different robust methods as well as a complexity analysis.



## Authors

* **Christopher Bülte**
