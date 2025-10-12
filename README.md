#  Football Players Performance Analysis  
### _Exploring Offensive Performance Across Europe’s Top 5 Leagues (2023–2024)_

![R](https://img.shields.io/badge/Made%20with-R-blue?logo=r&logoColor=white)
![PCA](https://img.shields.io/badge/Method-PCA%20%7C%20ACOBI%20%7C%20Robust%20Regression-lightblue)
![Status](https://img.shields.io/badge/Status-Completed-success)
![License](https://img.shields.io/badge/License-MIT-green)

---

##  Overview

This project analyzes the offensive performance of football players from the **Top 5 European leagues** : 
**Premier League**, **La Liga**, **Serie A**, **Bundesliga**  and **Ligue 1**, during the **2023–2024 season**.  

We investigate how **age** and **technical variables** influence players' performance using  
**Robust Principal Component Analysis (PCA)**, **Binary Correspondence Analysis (ACOBI)**, and **Robust Regression**.

The project was completed as part of the **STAT-S401 course** at the Université Libre de Bruxelles (ULB).

---

##  Dependencies

Install the required R packages before running the script:


install.packages(c(
  "dplyr",
  "MASS",
  "robustbase",
  "FactoMineR"
))



## Methodology

The project follows a rigorous methodology combining exploratory, robust, and multivariate statistical techniques:

### 1) Exploratory Data Analysis 
- Distribution of players by **age group** and **league**
- Identification of key performance variables (Goals, Assists, xG, Minutes, etc.)

### 2) Outlier Detection
- Application of the **Robust Mahalanobis Distance (MCD)** to identify abnormal observations
- Visualization of influential points and potential anomalies

### 3️) Robust Principal Component Analysis (PCA)
- Conducted via `PcaHubert()` to reduce dimensionality while resisting the influence of outliers
- Identification of the main latent factors driving offensive performance

### 4️) Binary Correspondence Analysis (ACOBI)
- Studied the association between **league** and **age category**
- Highlighted the differences in player age profiles across European leagues

### 5) Robust Regression (`rlm`)
- Modeled **Goals (Gls)** as a function of **Age**, **Assists**, **Minutes**, and **League**
- Used M-estimators to limit the impact of extreme values on coefficient estimation

---

##  Key Insights

- **Age ≠ Performance:**  
Age alone does not fully explain offensive performance — both young and experienced players can reach similar efficiency levels.

-**League Trends:**  
- **Ligue 1** tends to attract **younger** players.  
- **La Liga** and **Serie A** favor **experienced** and **older** profiles.  
- **Premier League** maintains a **balanced** mix, combining intensity and tactical maturity.  

- **Importance of Robust Methods:**  
Due to the presence of numerous outliers, classical methods would produce biased results.  
Using robust approaches like **MCD**, **PCAHubert**, and **RLM** ensures more reliable interpretations.

---

## Project Structure
top5_performance/
│
├── top5_performance.R       
├── rapport.pdf
├── data.csv                  
└── README.md  

## Data Source

**Dataset:** [All Football Players Stats in Top 5 Leagues 23/24](https://www.kaggle.com/datasets/orkunaktas/all-football-players-stats-in-top-5-leagues-2324)  
**Author:** Orkun Aktas – *Kaggle, 2024*  

---

## Authors

- **Rayane Tfeili**  
- **Robin Aerts**  
- **Darren Bayala**  
- **Assia Ben Hamou**  
- **Anouar Laloy**

*Supervised by Prof. Catherine Dehon, Université Libre de Bruxelles (ULB)*  

---

##  References

1. **CIES Football Observatory (2021)** – *Distribution of playing time by age across Europe’s top leagues.*  
2. **Bollier A.C. et al. (2022)** – *Association Between Professional Football Participation and Long-term Health Outcomes.*  
3. **Todorov & Filzmoser (2022)** – *rrcov: Scalable Robust Estimators with High Breakdown Point.*  
