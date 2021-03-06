# R-Project-Pollinator-Habitat-Restoration-Tools
Statistical Consulting Project
## 1.Introduction
### Project Objective:
* Examine imazapic ("Plateau") as part of a strategy for successful development of pollinator habitat.
* Variables:
	1) Examine plant response using two different seed mixes
		Mix A: imazapic-tolerant seed mix (limited species)
		Mix B: traditional pollinator seed mix (broadly diverse species)
	2) Examine plant response based on two different seed treatments
		Treatment A: with cold stratification
		Treatment B: without cold stratification
	3) Examine the affect of timing of imazapic applications
		Timing A: pre-emergent application
		Timing B: post-emergent application

* Questions examined in relation to variables listed above:
  1) Two seed mixes: "How does the broadly diverse, traditional "pollinator habitat" seed mix perform with imazapic application?"
	2) Two treatments: "Does cold stratification contribute to the success of native plant establishment?"
	3) Timing: "How will plants respond to pre-emergent application of imazapic vs. post-emergent application?"

* Hypothesis:
  1) "An establishment strategy that combines cold stratification with post-emergent imazapic application will encourage the strongest community assembly of desired species while minimizing weed pressure.
  2) Control sites which receive no imazapic will have high thresholds of weeds.
* Want to show:
	1) species assembly and population strength of target plants in relation to different seed mixes, cold stratification treatment, and timing of application.
	2) Bar graphs displaying target species vs. common exotic weeds?

## 2.Data Preprocessing 
### 2.1 Data Cleaning
All the missing values in the dataset are replaced by 0 on the purpose of further analysis.
### 2.2 Data Integration
In this section, all the variables are created as follows:
The target variable is ratio, which calculated through the formula: Ratio = ( Grass + Forbs ) / ( Grass + Forbs + Weeds)
The predictors are created based on different treatment. All the predictor variables are categorical variable.

## Visualization
## ANOVA Analysis 
## Conclusion
## Appendix
