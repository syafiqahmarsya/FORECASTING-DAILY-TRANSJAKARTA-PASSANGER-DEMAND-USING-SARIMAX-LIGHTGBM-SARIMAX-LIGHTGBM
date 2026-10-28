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
- **Data source:** PPID PT Transportasi Jakarta [(https://ppid.transjakarta.co.id/)]

## Methodology
1. Data preparation and exploratory data analysis (EDA)
2. Time-series modeling using SARIMAX
3. Feature engineering and forecasting with LightGBM
4. Residual-based hybrid modeling using SARIMAX and LightGBM
5. Hyperparameter optimization and time-series cross-validation
6. Evaluation using MAE, RMSE, and MAPE

## Tools 
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
| SARIMAX | 24.918,89 | 32.161,33 | 6,1053% |
| LightGBM | 27.439,20 | 48.278,42 | 7,8895% |
| Hybrid SARIMAX–LightGBM | 22.364,07 | 30.489,99 | 5,6688% |

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
<img width="833" height="1230" alt="image" src="https://github.com/user-attachments/assets/5da1f682-7aa2-4b43-8f08-02c137df6f9a" />
