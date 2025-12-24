#loading the dataset
data=read.csv(file.choose(),header=TRUE)
View(data)
summary(data)
#Checking missing value and outlier
is.na(data)
numeric_data <- data[sapply(data, is.numeric)]
outlier_check <- function(x) {
 q1 <- quantile(x, 0.25)
 q3 <- quantile(x, 0.75)
 iqr <- q3 - q1
 lower <- q1 - 1.5 * iqr
 upper <- q3 + 1.5 * iqr
 return(which(x < lower | x > upper))
}
outlier_indices <- lapply(numeric_data, outlier_check)
for (var in names(outlier_indices)) {
 cat("\nOutliers in", var, "at rows:", outlier_indices[[var]], "\n")
}
#Trend Analysis
fig <- plot_ly(data)
# Add traces
fig <- fig %>%
 add_trace(
 x = ~Year,
 y = ~`AI.Software.Revenue.in.Billions.`,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'AI Software Revenue (Billions)'
 ) %>%
 add_trace(
 x = ~Year,
 y = ~`Global.AI.Market.Value.in.Billions.`,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Global AI Market Value (Billions)'
 ) %>%
 add_trace(
 x = ~Year,
 y = ~`Marketers.Believing.AI.Improves.Email.Revenue`,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Marketers Believing AI Improves Email Revenue'
 )
# Update layout
fig <- fig %>%
 layout(
 title = 'Trend Analysis of AI Software Revenue, Global AI Market Value, and Marketing Belief in AI',
 xaxis = list(title = 'Year', showgrid = TRUE),
 yaxis = list(title = 'Value / Percentage', showgrid = TRUE),
 legend = list(title = list(text = 'Metrics')),
 template = 'plotly_white',
 width = 1000,
 height = 600
 )
# Show the plot
fig
# Reshape data to long format
data_long <- data %>%
 pivot_longer(
 cols = -Year,
 names_to = "Metric",
 values_to = "Value"
 )
# Custom color vector (21 colors)
custom_colors <- c(
 "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
 "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
 "#393b79", "#8c6d31", "#e6550d", "#9c9ede", "#636363",
 "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
 "#ffff33"
)
# Create smoothed lines with custom tooltip
figure <- plot_ly(
 data = data_long,
 x = ~Year,
 y = ~Value,
 type = 'scatter',
 mode = 'lines',
 color = ~Metric,
 colors = custom_colors,
 hovertemplate = paste(
 "<b>Metric:</b> %{text}<br>",
 "<b>Year:</b> %{x}<br>",
 "<b>Value:</b> %{y}<extra></extra>"),
 text = ~Metric,
 line = list(shape = "spline") # Smooth lines
)
# Interactive filter (legend click)
figure <- figure %>%
 layout(
 title = "Smoothed Trends of AI Metrics with Interactive Filtering",
 xaxis = list(title = "Year", showgrid = TRUE),
 yaxis = list(title = "Value", showgrid = TRUE),
 legend = list(title = list(text = "Click to filter by Metric")),
 template = "plotly_white",
 width = 1000,
 height = 600
 )
# Show the plot
figure
#Actual AI Adoption and Global Expectation of AI Adoption versus AI software revenue Increase
data <- data.frame(
 Year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
 AI_Adoption_percentage = c(10,20,30,35,35,35,40,63),
 AI_Software_Revenue = c(10.1, 14.69, 22.59, 34.87, 51.27, 70.94, 94.41, 126.0)
)
# Plot: Bubble chart combining both metrics
figure <- plot_ly(
 data = data,
 x = ~Year,
 y = ~AI_Adoption_percentage,
 type = 'scatter',
 mode = 'markers',
 marker = list(
 size = ~AI_Software_Revenue, # Bubble size = revenue
 sizemode = 'area',
 sizeref = 2 * max(data$AI_Software_Revenue) / 100^2, # Auto scale
 sizemin = 5
 ),
 text = ~paste0(
 "<b>Year:</b> ", Year, "<br>",
 "<b>AI Adoption:</b> ", AI_Adoption_percentage, "%<br>",
 "<b>Revenue:</b> $", AI_Software_Revenue, "B"
 ),
 hoverinfo = 'text'
 )
# Layout
figure <- figure %>%
 layout(
 title = "AI World",
 xaxis = list(title = "Year", showgrid = TRUE),
 yaxis = list(title = "AI Adoption (%)", showgrid = TRUE),
 legend = list(title = list(text = "AI Metrics")),
 template = "plotly_white"
 )
# Show plot
figure
data <- data.frame(
 Year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
 Global_expectation_for_AI_Adoption_Percentage = c(40, 47, 54, 61, 68, 73, 78, 82),
 AI_Software_Revenue = c(10.1, 14.69, 22.59, 34.87, 51.27, 70.94, 94.41, 126.0)
)
# Plot: Bubble chart combining both metrics
figure <- plot_ly(
 data = data,
 x = ~Year,
 y = ~Global_expectation_for_AI_Adoption_Percentage,
 type = 'scatter',
 mode = 'markers',
 marker = list(
 size = ~AI_Software_Revenue, # Bubble size = revenue
 sizemode = 'area',
 sizeref = 2 * max(data$AI_Software_Revenue) / 100^2, # Auto scale
 sizemin = 5
 ),
 text = ~paste0(
 "<b>Year:</b> ", Year, "<br>",
 "<b>AI Adoption:</b> ", Global_expectation_for_AI_Adoption_Percentage, "%<br>",
 "<b>Revenue:</b> $", AI_Software_Revenue, "B"
 ),
 hoverinfo = 'text'
)
# Layout
figure <- figure %>%
 layout(
 title = "AI World",
 xaxis = list(title = "Year", showgrid = TRUE),
 yaxis = list(title = "Global expectation for AI Adoption (%)", showgrid = TRUE),
 legend = list(title = list(text = "AI Metrics")),
 template = "plotly_white"
 )
# Show plot
figure

#Jobs created and eliminated by AI
fig <- plot_ly(data = data) %>%
 add_trace(
 x = ~Year,
 y = ~Estimated.Jobs.Eliminated.by.AI..millions....,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Jobs Eliminated'
 ) %>%
 add_trace(
 x = ~Year,
 y = ~Estimated.New.Jobs.Created.by.AI..millions....,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'New Jobs Created'
 ) %>%
 add_trace(
 x = ~Year,
 y = ~Estimated.Revenue.Increase.from.AI..trillions.USD.,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Revenue Increase from AI'
 ) %>%
 add_trace(
 x = ~Year,
 y = ~Net.Job.Loss.in.the.US,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Net Job Loss'
 ) %>%
 layout(
 title = 'Estimated Jobs Created and Eliminated by AI',
 xaxis = list(title = 'Year', showgrid = TRUE),
 yaxis = list(title = 'Estimated Values', showgrid = TRUE),
 legend = list(title = list(text = 'Metrics')),
 template = 'plotly_white',
 width = 1000,
 height = 600
 )
# Show the plot
Fig

#Jobs at High Risk
install.packages("tidyverse")
library(tidyverse)
long_data <- data %>%
 select(
 Year,
 `Jobs.at.High.Risk.of.Automation...Manufacturing`,
 `Jobs.at.High.Risk.of.Automation...Transportation...Storage....`,
 `Jobs.at.High.Risk.of.Automation...Wholesale...Retail.Trade`
 ) %>%
 pivot_longer(
 cols = -Year,
 names_to = "Metrics",
 values_to = "High_Risk_Percentage"
 )
# Plot the line chart
ggplot(long_data, aes(x = Year, y = High_Risk_Percentage, color = Metrics)) +
 geom_line(size = 1) +
 geom_point(size = 2) +
 scale_x_continuous(breaks = unique(long_data$Year)) +
 scale_y_continuous(name = "High Risk Percentage (%)") +
 labs(
 title = "Jobs At High Risks for AI Automation",
 color = "Metrics"
 ) +
 theme_minimal(base_size = 14) +
 theme(
 plot.title = element_text(face = "bold", hjust = 0.5),
 legend.position = "right"
 )

#Companies and Organization Implementation
# Create the plot
fig <- plot_ly(data, x = ~Year) %>%
 add_lines(y = ~Companies.Prioritizing.AI.in.Strategy, name = 'Companies Prioritizing AI in
Strategy', mode = 'lines+markers') %>%
 add_lines(y = ~Organizations.Believing.AI.Provides.Competitive.Edge, name = 'Organizations
Believing AI Provides Competitive Edge', mode = 'lines+markers') %>%
 add_lines(y = ~Organizations.Planning.to.Implement.AI, name = 'Organizations Planning to
Implement AI', mode = 'lines+markers') %>%
 add_lines(y = ~Organizations.Using.AI, name = 'Organizations Using AI', mode = 'lines+markers')
# Apply layout with the new title
% layout(
 title = 'Companies and Organizations Implementation',
 xaxis = list(title = 'Year', showgrid = TRUE),
 yaxis = list(title = 'Percentage (%)', showgrid = TRUE),
 legend = list(title = list(text = '<b>Metrics</b>'))
 )
# Show the plot
fig

#AI in Healthcare

# Load the plotly library
library(plotly)
# Create the plot with dual y-axis
fig <- plot_ly()
# First trace: AI Contribution to Healthcare (Billions) -> Left Y-axis
fig <- fig %>%
 add_trace(
 data = data,
 x = ~Year,
 y = ~AI.Contribution.to.Healthcare.in.Billions.,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'AI Contribution to Healthcare (Billions)',
 yaxis = 'y1'
 )
# Second trace: Medical Professionals Using AI for Diagnosis (%) -> Right Y-axis
fig <- fig %>%
 add_trace(
 data = data,
 x = ~Year,
 y = ~Medical.Professionals.Using.AI.for.Diagnosis,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Medical Professionals Using AI (%)',
 yaxis = 'y2'
 )
# Update layout with two y-axes
fig <- fig %>%
 layout(
 title = 'AI in Healthcare: Financial Growth & Professional Adoption',
 xaxis = list(title = 'Year', showgrid = TRUE),
 yaxis = list(
 title = 'AI Contribution to Healthcare (Billions USD)',
 showgrid = TRUE
 ),
 yaxis2 = list(
 title = 'Medical Professionals Using AI (%)',
 overlaying = 'y',
 side = 'right',
 showgrid = FALSE
 ),
 legend = list(title = list(text = 'Metrics')),
 width = 1000,
 height = 600,
 template = 'plotly_white'
 )
# Display the plot
fig

#AI VOICE ASSISTANTS
# Load the plotly library
library(plotly)
# Create the plot with dual y-axis
fig <- plot_ly()
# First trace: Voice Assistants (billions of devices) -> Left Y-axis
fig <- fig %>%
 add_trace(
 data = data,
 x = ~Year,
 y = ~Digital.Voice.Assistants..billions.of.devices.,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Voice Assistants (Billions)',
 yaxis = 'y1'
 )
# Second trace: Americans using Voice Assistants (%) -> Right Y-axis
fig <- fig %>%
 add_trace(
 data = data,
 x = ~Year,
 y = ~Americans.Using.Voice.Assistants....,
 type = 'scatter',
 mode = 'lines+markers',
 name = 'Americans Using Voice Assistants (%)',
 yaxis = 'y2'
 )
# Update layout with two y-axes
fig <- fig %>%
 layout(
 title = 'Voice Assistants Trend Analysis of AI Adoption',
 xaxis = list(title = 'Year', showgrid = TRUE),
 yaxis = list(
 title = 'Number of Devices (Billions)',
 showgrid = TRUE
 ),
 yaxis2 = list(
 title = 'Americans Using Voice Assistants (%)',
 overlaying = 'y',
 side = 'right',
 showgrid = FALSE
 ),
 legend = list(title = list(text = 'Metrics')),
 width = 1000,
 height = 600,
 template = 'plotly_white'
 )
# Display the plot
fig


#Correlation(heatmap)
install.packages("plotly")
library(plotly)
correlation_matrix=cor(data,use = "complete.obs")
plot_ly(
 z = correlation_matrix,
 x = colnames(correlation_matrix),
 y = rownames(correlation_matrix),
 type = "heatmap",
 colorscale = "RdBu", # Red for negative, Blue for positive
 zmin = -1, zmax = 1 # Fix scale to highlight extremes
) %>%
 layout(
 title = "Correlation Matrix",
 xaxis = list(tickangle = -45, automargin = TRUE),
 yaxis = list(automargin = TRUE)
 )
 #correlation matrix
 df_cleaned <- data %>% select(-Year)
correlation_matrix=cor(df_cleaned,use = "complete.obs")
ggcorrplot(correlation_matrix,lab=TRUE,hc.order=TRUE,type="lower",colors=c("purple","white","gre
en"),title="Correlation Matrix")
ggcorrplot(correlation_matrix,
 method = "circle", # or "square" if you prefer
 type = "lower", # only show lower triangle
 lab = TRUE, # show correlation values
 lab_size = 3, # smaller text
 colors = c("purple", "white", "green"), # custom color scale
 title = "Correlation Matrix",
 tl.cex = 8, # axis text size
 tl.srt = 45) # rotate axis labels

 #PCA on training set
 train_data <- data[data$Year <= 2023, ]
test_data <- data[data$Year > 2023 & data$Year <= 2025, ]
numeric_train <- train_data[sapply(train_data, is.numeric)]
numeric_train <- numeric_train[, apply(numeric_train, 2, function(x) var(x) > 0)]
pca_train <- prcomp(numeric_train, scale. = TRUE)
summary(pca_train)

#Projecting test set into PCA space
numeric_test <- test_data[, colnames(numeric_train)] # Ensure columns match
numeric_test_scaled <- scale(numeric_test,
 center = pca_train$center,
 scale = pca_train$scale)
# Now project into PC1 space
pc1_test_scores <- as.numeric(as.matrix(numeric_test_scaled) %*%
pca_train$rotation[, 1])

#Forecast for test set
pc1_ts_train <- ts(pc1_train_scores, start = 2018)
model_pc1 <- auto.arima(pc1_ts_train)
# Forecast for 2 steps (since the test set is 2 years: 2024 & 2025)
forecast_test <- forecast(model_pc1, h = 2)


#Comparison between actual and forecasted PC1 score
comparison <- data.frame(
 Year = 2024:2025,
 Actual_PC1 = pc1_test_scores,
 Forecasted_PC1 = as.numeric(forecast_test$mean)
)
# Calculate error
comparison$Error <- comparison$Actual_PC1 - comparison$Forecasted_PC1
comparison$AbsError <- abs(comparison$Error)
print(comparison)

#Retraining data with arima for 2018-2025 and forecasting for 2026-2028:
# 1. Extract PC1 scores from full data (2018–2025)
numeric_data <- data[sapply(data, is.numeric)]
# Perform PCA again on full dataset
pca_full <- prcomp(numeric_data, scale. = TRUE)
pc1_full_scores <- pca_full$x[, 1] # PC1 scores for 2018–2025
# 2. Convert to time series
pc1_ts_full <- ts(pc1_full_scores, start = 2018)
# ADF test
library(tseries)
adf_result <- adf.test(pc1_ts_full)
print(adf_result)
# 3. Fit ARIMA model on full PC1 series
library(forecast)
model_pc1_full <- auto.arima(pc1_ts_full)
# 4. Forecast next 3 years (2026, 2027, 2028)
forecast_pc1_future <- forecast(model_pc1_full, h = 3)
# 5. Plot forecast
plot(forecast_pc1_future, main = "Forecasted PC1 Scores (2026–2028)")
summary(model_pc1_full)
checkresiduals(model_pc1_full)

#Adjustment and Refinement for forecasting:
# Step 1: Get PC1 loadings from pca_full
loadings <- pca_full$rotation[, 1]
# Step 2: Extract variable names
var_created <- "Estimated.New.Jobs.Created.by.AI..millions...."
var_eliminated <- "Estimated.Jobs.Eliminated.by.AI..millions...."
# Step 3: Get loading, mean, and SD for each variable
loading_created <- loadings[var_created]
loading_eliminated <- loadings[var_eliminated]
mean_created <- mean(numeric_data[[var_created]])
sd_created <- sd(numeric_data[[var_created]])
mean_eliminated <- mean(numeric_data[[var_eliminated]])
sd_eliminated <- sd(numeric_data[[var_eliminated]])
# Step 4: Reconstruct forecasts
forecast_created <- (future_pc1_values * loading_created * sd_created) + mean_created
forecast_eliminated <- (future_pc1_values * loading_eliminated * sd_eliminated) +
mean_eliminated
# Step 5: View final forecast table
forecast_results <- data.frame(
 Year = 2026:2028,
 Forecasted_Jobs_Created = round(forecast_created, 3),
 Forecasted_Jobs_Eliminated = round(forecast_eliminated, 3)
)
print(forecast_results)


