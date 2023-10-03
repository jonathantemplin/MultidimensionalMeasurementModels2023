In this folder, estimation of multidimensional measurement models with differing method across differing software packages is given.

Using simulated two-dimensional data for both CFA and IRT, the following models are estimated (if the package allows):

1. Three-dimensional model with a lower-echelon Q-matrix (Exploratory with confirmatory constraints)
2. Two-dimensional model with a lower-echeclon Q-matrix (Exploratory with confirmatory constraints)
3. Two-dimensional model with correct structure
4. One-dimensional model

When possible, then I used that package's exploratory model to attempt to estimate one, two, and three dimensional models.

## Pros and Cons of Each Package

### Mplus

#### Pros

* Easy to use
* The most models and estimation options of all packages
* Good options for limited information estimation
* Can do measurement and structural models 

#### Cons

* Proprietary software that is not open-source (so difficult to spot algorithm coding errors)
* Expensive
* Can have issues with numerical stability (original software seemed to use 32-bit floating point numbers)
* May take an excessive amount of time to estimate a model
  * May also have no results if the model does not estimate
* Model fit for categorical data with full information is very limited

### The lavaan package in R

#### Pros

* Open-source
* Free
* Can do measurement and structural models
* Can do limited information estimation
* Can do exploratory and confirmatory estimation

#### Cons

* Not many types of observed data allowed
* Limited information estimation is more limited than Mplus
* No full information estimation at all

### The mirt package in R

#### Pros

* Open-source
* Free
* Lots of variants of full information estimation
* Lots of types of IRT-ish models (e.g., unfolding models)
* Good limited information model fit for full information estimation

#### Cons

* No structural models (no normal data models/CFAs)
* No limited information estimation
* Can be difficult to use with poor documentation

