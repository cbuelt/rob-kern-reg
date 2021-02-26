# Nonlinear Kernel Methods: Theoretical aspects and robust extensions

This project includes the supplementary R code for my Bachelor's thesis about kernel methods and nonlinear kernel regression. Using this code you can recreate my simulations and comparisons of three different robust kernel methods. To get an overview of the thesis you can read the following abstract.

## Abstract

Kernel methods have been an active area of research since they were first used in the context of statistical learning and are still widely used throughout different machine learning tasks. This thesis focuses on the fundamental theory of kernel methods as well as their application in the context of robust nonlinear regression. First, we focus on the theory behind kernel methods and how the kernel trick works. While kernel based methods like support vector machines are often applied in data science or machine learning, the fundamental principle is not always understood to full extend. We try to give an intuitive idea of kernel methods and why they are popular in statistical learning. We derive some basic characterisations of kernel functions as well as some fundamental theory about reproducing kernel Hilbert spaces. Furthermore, we derive the kernel trick in the context of support vector machines and kernel ridge regression.
Not only in kernel methods, but in general data often suffers from contamination like outliers or noise. In the second part of this thesis we introduce several methods that are robust to outliers in the context of kernel regression. These methods are able to estimate the underlying function from the data, even if the data is heavily inflicted by outliers. Using a simulation experiment, we generate several datasets contaminated by outliers and noise that differ in parameters like the strength and percentage of outliers present. Each of the aforementioned methods is used to predict data, while under the influence of outliers, and the respective performance is measured. Using the simulation we are able to determine which methods work best for different outlier parameters. Our results suggest that robust kernel regression methods show a favourable behavior in the context of outliers, compared to the regular kernel ridge regression.



## Authors

* **Christopher Bülte** - *Initial work*
