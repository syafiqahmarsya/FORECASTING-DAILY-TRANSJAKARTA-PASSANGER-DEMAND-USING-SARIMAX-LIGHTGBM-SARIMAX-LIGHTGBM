# Daily TransJakarta Passenger Forecasting

## Project Overview
This project compares three time-series forecasting approaches to predict daily Bus Rapid Transit (BRT) passenger counts for TransJakarta: SARIMAX, LightGBM, and a hybrid SARIMAX–LightGBM model.

The objective is to evaluate forecasting performance and examine whether combining statistical time-series modeling with machine learning improves prediction accuracy.

## Business Problem
Reliable passenger forecasts can support public transportation planning, capacity management, and operational decision-making. Daily passenger demand may vary due to recurring temporal patterns, national holidays, Eid al-Fitr, and fare promotions.

## Dataset
- **Period:** January 1, 2023 – July 31, 2025
- **Frequency:** Daily
- **Target variable:** Daily number of TransJakarta BRT passengers
- **Exogenous variables:** National holidays, Eid-related dates, and fare promotions
- **Data source:** PPID PT Transportasi Jakarta [insert source link]

## Methodology
1. Data preparation and exploratory data analysis (EDA)
2. Time-series modeling using SARIMAX
3. Feature engineering and forecasting with LightGBM
4. Residual-based hybrid modeling using SARIMAX and LightGBM
5. Hyperparameter optimization and time-series cross-validation
6. Evaluation using MAE, RMSE, and MAPE

## Tools & Technologies
- Python
- R
- pandas and NumPy
- LightGBM
- Optuna
- Statistical time-series modeling
- Data visualization

## Evaluation
The models are evaluated using Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and Mean Absolute Percentage Error (MAPE). Lower values indicate better performance for all three metrics.

| Model | MAE | RMSE | MAPE |
|---|---:|---:|---:|
| SARIMAX | [final result] | [final result] | [final result] |
| LightGBM | [final result] | [final result] | [final result] |
| Hybrid SARIMAX–LightGBM | 19,676.98 | 24,932.70 | 4.8664% |

*Replace all metrics with the results from the final, consistent experiment before publication.*

## Key Findings
- Compared statistical, machine-learning, and hybrid forecasting approaches on the same passenger-demand forecasting task.
- Evaluated the contribution of holiday, Eid-related, and fare-promotion variables.
- Examined prediction errors during periods with unusual passenger-demand patterns.
- Assessed whether residual modeling improved forecasting performance over the standalone models.

## Limitations
Forecasting errors can increase during unusual events or sudden changes in passenger demand. The findings apply to the dataset period and experimental setup used in this project; they do not guarantee the same performance in future periods.

## Author
Syafiqah Marsya Kholiyadi
Mathematics graduate | Interested in Data Analytics, Statistical Modeling, and Forecasting
