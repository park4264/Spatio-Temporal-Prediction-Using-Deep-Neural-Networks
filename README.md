# Spatio-Temporal Prediction Using Deep Neural Networks
## 딥러닝을 활용한 시공간 데이터 예측


- 08 / 2023
- Park Tae Jun
- Seoul National University
- Department of Statistics
- Master's Thesis


## Abstract
Kriging provides the Best Linear Unbiased Predictor (BLUP) under Gaussian, stationary assumption of the statistical spatio-temporal model. This is a method of interpolation used to predict data with spatio-temporal dependence. However, for complex data that do not satisfy the assumptions, Kriging is no longer optimal. Nowadays, Deep learning using Deep neural networks (DNNs) is being used in many fields. Deep feedforward networks can be used for regression, so I proposed a novel Kriging method using DNN structure in this study. This method may learn more complex spatio-temporal dependencies. Next, I study the traditional Kriging and my method in terms of statistical learning theory. Finally, I apply my method to real-life korea fine dust data to evaluate the performance. Here, the K-fold Cross Validation method for spatio-temporal data is used.

- **Keywords**: Spatio-temporal data, Kriging, Deep learning, Deep Neural Networks, Deep feedforward networks, Statistical learning theory

## Visualization of DNN Structure
![STDNNK](./img/STDNNK.png)

---

## Applicaition
We applied it to real-world data to compare the test performance of any of our models with the existing spatio-temporal kriging methods. For the data, Korea's fine dust spatio-temporal data was used. Fine dust data is known to be non-gaussian and difficult to predict. So, we try to evaluate the performance on complex models by using our method to show better performance of this data. 

### Data Description

As for the data, PM2.5 concentration data of land in Korea was obtained from AirKorea (https://www.airkorea.or.kr). Data from January 9, 2022, data from February 1, 2022, and data from June 29, 2022 were obtained from 1:00 to 24:00 (the next day 00:00) for a day with high, normal, and low  PM2.5 concentration. In this way, the performance of the model was evaluated for all cases by obtaining spatio-temporal data that included the time of three days of PM2.5 concentration and latitude longitude of the observatory. 







