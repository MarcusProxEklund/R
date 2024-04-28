# Load in neccessary libraries ----------------------------------------
library(tidyverse)
library(car)
library(ggfortify)
library(caret)
library(pxweb)
library(Metrics)

# load data -----------------------------------------------------------
file_path <- "C:/Users/marcu/WinCode/ec_utbildning/R/kunskapskontroll_r/car_ads_data_02.csv"
raw_car_data <- read.csv(file_path)

# quick check on the data ---------------------------------------------
View(raw_car_data)
summary(raw_car_data)
str(raw_car_data)
sum(is.na(raw_car_data))
dim(raw_car_data)

## cleaning and transforming data -------------------------------------
# Removing all rows with NA in Pris
# removing all rows with Pris under 20 000 and over 500 000
# removing beginning and trailing whitespaces
# turning string "NA" into real NA
# removing thousand separator whitespace in Miltal
# turning Miltal, Modellår, HK and Pris to integers
# turning Modell, Bränsle, Växellåda, Biltyp, Drivning, Färg and Region to factors
# turning Datum.i.trafik to Date
# creating 2 new columns, Ålder and Dager_i_trafik, based on Modellår and Datum.i.trafik
# removing columns Modellår, Motorstorlek and Datum.i.trafik
# removing all remaining rows with a missing value
car_df <- raw_car_data |>
    dplyr::filter(!is.na(Pris)) |>
    dplyr::filter(Pris >= 20000 & Pris <= 500000) |>
    mutate(across(everything(), str_trim)) |>
    mutate(across(everything(), ~ na_if(.x, "NA"))) |>
    mutate(Miltal = str_replace_all(Miltal, "\\D", "")) |>
    mutate(across(c("Miltal", "Modellår", "HK", "Pris"), as.integer)) |>
    mutate(across(c("Modell", "Bränsle", "Växellåda", "Biltyp", "Drivning", "Färg", "Region"), as.factor)) |>
    mutate(across("Datum.i.trafik", as.Date)) |>
    dplyr::filter(year(Datum.i.trafik) >= 1999) |>
    mutate(Ålder = as.integer(year(Sys.Date()) - Modellår), Dagar_i_trafik = as.integer(Sys.Date() - Datum.i.trafik)) |>
    select(!c(Modell, Modellår, Motorstorlek, Datum.i.trafik)) |>
    na.omit() |>
    as_tibble()
    

sum(is.na(car_df))
summary(car_df)
str(car_df)
View(car_df)
names(car_df)
head(car_df)
dim(car_df)

# Grouping the brands with fewer than 50 observations into "Other"
car_df <- rows_update(car_df, car_df |> 
                        group_by(Märke) |>
                        mutate(count = n()) |>
                        dplyr::filter(count < 50) |>
                        mutate(Märke = "Other") |>
                        select(-count),
                    by = "Id")


table(car_df$Märke)
str(car_df)

car_df <- car_df |>
    mutate(across("Märke", as.factor))

dim(car_df)

# splitting data into train and test set -------------------------------
set.seed(16)

car_df <- car_df |> mutate(row_nbr = row_number())

train_df <- car_df |> sample_frac(.70)
test_df  <- anti_join(car_df, train_df, by = "row_nbr")

train_df <- train_df |> select(-Id, -row_nbr)
test_df <- test_df |> select(-Id, -row_nbr)

dim(train_df)
dim(test_df)


# initial linear model ------------------------------------------------
model_01 <- lm(Pris ~ ., data = train_df)

print(model_01)
summary(model_01)
coef(model_01)

par(mar = c(1, 1, 1, 1))

dev.off()
ggplot(model_01, aes(.resid)) +
    geom_histogram(bins = 50)

autoplot(model_01, which = c(1, 2, 3, 6))

ggplot(model_01, aes(.hat, .stdresid)) +
    geom_point(aes(size = .cooksd), na.rm = TRUE) +
    stat_smooth(method = "loess", na.rm = TRUE) +
    xlab("Leverage") +
    ylab("Standardized Residuals") +
    ggtitle("Residual vs Leverage Plot") +
    scale_size_continuous("Cook's Distance", range = c(1, 5))


# Running forward subset selection --------------------------------------
regfit_fwd <- regsubsets(Pris ~ ., data = train_df, nvmax = 11, method = "forward")
summary(regfit_fwd)

reg_summary <- summary(regfit_fwd)

names(reg_summary)

# Plotting subset evaluation with RSS and Adjusted Rsqr ------------------
par(mfrow = c(1, 2))

plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg_summary$adjr2)
points(which.max(reg_summary$adjr2), reg_summary$adjr2[11], col = "red", cex = 2, pch = 20)

coef(regfit_fwd, 8)

# second model with fewer variables ------------------------------------
model_02 <- lm(
    Pris ~ Märke +
        Växellåda +
        Miltal +
        Biltyp +
        HK +
        Ålder +
        Dagar_i_trafik,
    data = train_df
)

model_02_summary <- summary(model_03)
model_02_summary

names(model_02_summary)
model_02_summary$adj.r.squared

# comparing the two models ---------------------------------------------
results <- data.frame(
    Model = c("Model 1", "Model 2"),
    Adj_R_squared = c(
        summary(model_01)$adj.r.squared,
        summary(model_02)$adj.r.squared
    ),
    BIC = c(BIC(model_01), BIC(model_02))
)
results

# Cross validation ---------------------------------------------------
train_control <- trainControl(method = "cv", number = 10)

cv_model_01 <- train(
    Pris ~ ., data = train_df, method = "lm", trControl = train_control
)

print(cv_model_01)
summary(cv_model_01)


# compare RMSE of crossval of model_01 with model_01 on test data -----
names(cv_model_01)
cv_model_01$results$RMSE

test_pred_m1 <- predict(model_01, newdata = test_df)
rmse(test_df$Pris, test_pred_m1)

# Inference -----------------------------------------------------------

summary(model_01)

confint(model_01)
names(train_df)


# New data that we want to predict https://www.blocket.se/annons/1400697363
new_data_raw <- data.frame(
    Märke = "BMW",
    Bränsle = "Bensin",
    Växellåda = "Manuell",
    Miltal = 14328,
    Biltyp = "Sedan",
    Drivning = "Tvåhjulsdriven",
    HK = 170,
    Färg = "Blå",
    Region = "Göteborg",
    Ålder = year(Sys.Date()) - 2004,
    Dagar_i_trafik = Sys.Date() - as.Date("2004-02-25")
)

new_data <- new_data_raw |>
    mutate(across(c("Märke", "Bränsle", "Växellåda", "Biltyp", "Drivning", "Färg", "Region"), as.factor)) |>
    mutate(across(c("Miltal", "HK", "Ålder", "Dagar_i_trafik"), as.integer))

str(new_data)

# Correct price is 72 000
predict(model_01, newdata = new_data)

# Create CI & PI for predictions
confidence_intervals <- predict(model_01, newdata = new_data, interval = "confidence", level = 0.95)
prediction_intervals <- predict(model_01, newdata = new_data, interval = "prediction", level = 0.95)

confidence_intervals
prediction_intervals


## Collecting data from SCB ------------------------------------
d <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA")

# PXWEB query 
pxweb_query_list <-
    list("Region" = c("00"),
        "Agarkategori" = c("000"),
        "ContentsCode" = c("TK1001AB"),
        "Tid" = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))

# Download data 
px_data <-
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA",
                query = pxweb_query_list)

# Convert to data.frame
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as
pxweb_cite(px_data)

View(px_data_frame)
names(px_data_frame)
str(px_data_frame)

scb_df <- px_data_frame |>
    mutate(across(c("år", "Personbilar i trafik"), as.integer)) |>
    rename(personbilar_i_trafik = `Personbilar i trafik`) |>
    na.omit()

str(scb_df)
View(scb_df)

ggplot(scb_df, aes(år, personbilar_i_trafik)) +
    geom_line() +
    scale_y_continuous()

tail(scb_df$personbilar_i_trafik, n = 1) - scb_df$personbilar_i_trafik[1]

diff_21_22 <- max(scb_df$personbilar_i_trafik) - tail(scb_df$personbilar_i_trafik, n = 1)
prcnt <- diff_21_22 / max(scb_df$personbilar_i_trafik)
prcnt
