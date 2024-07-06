[![Scikit-Learn](https://img.shields.io/badge/Scikit--Learn-RandomForest-blue)](https://scikit-learn.org/stable/)
[![TensorFlow LSTM](https://img.shields.io/badge/TensorFlow-LSTM-orange)](https://www.tensorflow.org/)
[![Python](https://img.shields.io/badge/Python-3.x-blue)](https://www.python.org/)
[![R](https://img.shields.io/badge/R-4.x-blue)](https://www.r-project.org/)
[![Prophet](https://img.shields.io/badge/Prophet-Facebook-blueviolet)](https://facebook.github.io/prophet/)

# **Forecasting Nominal Sales in the Footwear Industry in Colombia**

![image](https://github.com/DavidCastro88/SalesTimeSeriesPrediction/assets/91480088/0f756061-0341-4d88-a959-5082f34af8d4)

This project aims to develop time series models to predict nominal sales in the footwear industry in Colombia. Various modeling approaches, including SARIMA, Random Forest, LSTM, and Prophet, are explored to identify the most suitable for this specific context. The study time series corresponds to nominal sales in the footwear industry, in units of index numbers with the reference year of 2018, which was obtained from the Monthly Manufacturing Survey with Territorial Approach (EMMET) of the National Administrative Department of Statistics (DANE). ) where information on the evolution of the main economic variables of the Colombian manufacturing sector in the short term is obtained.The series data goes from January 2001 to April 2024.

[Data Available](https://www.dane.gov.co/files/operaciones/EMMET/anex-EMMET-TotalNacional-abr2024.xlsx)

## **Project Description**

In this project, historical data on nominal sales in the Colombian footwear industry are analyzed. Different time series modeling techniques are applied to generate accurate and actionable forecasts for business decision-making.

**Objectives**

- Implement SARIMA models to capture seasonal and trend patterns in sales data.

- Utilize Random Forest to explore the importance of various predictor variables in sales prediction.

- Apply LSTM neural networks to capture complex dependencies in sequential sales data.

- Evaluate the Prophet model from Facebook for its ability to handle time series data with multiple seasonalities.

## **Technologies Used**

Python: pandas, numpy, scikit-learn, statsmodels, tensorflow/keras

R: StatisticalModel (SARIMA)

Jupyter Notebooks: For interactive data analysis and result visualization.

## **Results**

![image](https://github.com/DavidCastro88/SalesTimeSeriesPrediction/assets/91480088/b7e93b5f-aa0a-468c-ae22-4b3a147fb05c)

It was found that the best model was the SARIMA (0,1,11)x(1,1,2)[12] only ma(1,3,11),AR(1),MA(2), with which A MAPE of 7.360% and an R2 of 0.8 were obtained, showing an adequate forecast for the patterns of the series. With this model we readjust all the values ​​of the series and forecast for the next 12 months ranging from May 2024 to April 2025.

![image](https://github.com/DavidCastro88/SalesTimeSeriesPrediction/assets/91480088/c422c3ed-4add-411b-bedf-f8f9f60d80ae)




