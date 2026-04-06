Interactive dashboard built with R Shiny to analyze 3,000 electric vehicle 
records across 25 variables — covering battery performance, CO2 emissions, 
range, resale value, and regional distribution.

 **Live Demo:** https://santi27.shinyapps.io/estadistica_descriptiva/

---

##  Features

- **KPI cards** — avg range (374 km), battery health (85%), CO2 saved, resale value
- **Distributions** — histograms and density plots per variable
- **Composition** — donut chart and stacked bar by vehicle type and region
- **Geographic map** — vehicle distribution across Asia, Australia, Europe and North America
- **Cross-tab analysis** — frequency tables between categorical variables
- **Combined chart** — bar + trend line (range vs battery capacity over time)
- **Scatter & correlation** — CO2 saved vs autonomy by vehicle type
- **Dynamic filters** — region, vehicle type, use case, and year range (2015–2024)

---

##  Tech stack

| Tool | Use |
|------|-----|
| R + Shiny | App framework & interactivity |
| ggplot2 | Data visualizations |
| dplyr | Data wrangling |
| shinyapps.io | Cloud deployment |

---

##  Dataset

**Electric Vehicle Analytics** — Yadav, Kaggle  
3,000 observations · 25 variables  
Variables include: Range_km, Battery_Capacity_kWh, Vehicle_Type, 
Region, CO2_Saved, Resale_Value, Maintenance_Cost, Battery_Health

---

##  Run locally
```r
# Install dependencies
install.packages(c("shiny", "ggplot2", "dplyr"))

# Run the app
shiny::runApp("app.R")
```

---

*Built by Santiago · Colombia 🇨🇴 · Open to remote Data Analyst roles*
