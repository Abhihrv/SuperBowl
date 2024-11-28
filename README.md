# Tweet Data Analysis and Sentiment Analysis

## Overview

This project analyzes Twitter data to identify mentions of various brands and advertisements during the Super Bowl event. It counts the number of tweets and retweets per brand, aggregates quarterly keywords, and performs sentiment analysis on brand-specific tweets. The goal is to understand public perception and engagement with different brands based on Twitter activity.

![alt text](/Infographic%20Image.png)

## Table of Contents

* Prerequisites
* Data Files Required
* Installation
* Script Explanation
    1. Data Loading and Preprocessing
    2. Brand and Advertisement Identification
    3. Counting Tweets and Retweets per Brand
    4. Quarterly Keywords Analysis
    5. Sentiment Analysis
* Output Files
* Notes
* License
* Author
* Acknowledgments

## Prerequisites

* R (version 3.6.0 or higher)
* RStudio (optional but recommended)

## R Packages

Ensure the following R packages are installed:

* `qdap`
* `tm`
* `syuzhet`

You can install them using:

```R
install.packages("qdap")
install.packages("tm")
install.packages("syuzhet")
```

## Data Files Required

1. **Tweet Data CSV**: Contains tweet data with columns such as text (tweet content) and lang (tweet language).

2. **Brand_Keywords.csv**: Contains a list of brands and their associated keywords.

    * Columns:
      * `Brand`: Name of the brand.
      * `Keywords`: Keywords associated with the brand.

3. **ad_names.csv**: Contains advertisement names and their associated keywords.

    * Columns:
      * `Ad`: Name of the advertisement.
      * `Keywords`: Keywords associated with the advertisement.
      * `Quarter`: The quarter during which the ad was aired (optional).

## Installation

1. Clone this repository or download the script file.
2. Place the script and all data files in the same directory.
3. Open the script in R or RStudio.

## Usage Instructions

1. Load the Script: Open the R script in RStudio.
2. Run the Script: Execute the script step by step or run it entirely.
3. Data Input: When prompted (due to `file.choose()` functions), select the appropriate CSV files:
    * First prompt: Select the Tweet Data CSV file.
    * Second prompt: Select the Brand_Keywords.csv file.
    * Third prompt: Select the ad_names.csv file.
4. Outputs: The script will process the data and generate output files and visualizations.

## Script Explanation

### 1. Data Loading and Preprocessing

* Load Tweet Data: Reads the tweet data CSV file into `tweet_data`.
* Filter English Tweets: Converts the `lang` column to a factor and filters for tweets where `lang == "en"`.
* Select Relevant Columns: Keeps only the necessary columns for analysis.

```R
# Read the data
tweet_data <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Convert language to factor and filter English tweets
tweet_data$lang <- factor(tweet_data$lang)
tweet_data <- subset(tweet_data, lang == "en")

# Select required columns
tweet_data <- tweet_data[, c(1, 2, 5, 16, 17, 24, 27, 57, 65, 66, 67, 75, 99, 303, 304, 305, 306, 307)]
```

### 2. Brand and Advertisement Identification

* **Load Brand Keywords**: Reads `Brand_Keywords.csv` into `Brand_Keywords`.
* **Create Dummy Variables for Brands**: For each brand, checks if its keywords are present in the tweet text and creates a new column indicating presence (1) or absence (0).
* **Load Advertisement Names**: Reads `ad_names.csv` into `ad_names`.
* **Create Dummy Variables for Ads**: Similar to brands, creates dummy variables for each advertisement.

```R
# Import Brand and Keywords
Brand_Keywords <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Create dummy variables for each brand
for (i in 1:nrow(Brand_Keywords)) {
  tweet_data[, paste0("Brand_", Brand_Keywords$Brand[i])] <- ifelse(
    grepl(Brand_Keywords$Keywords[i], tolower(tweet_data$text)),
    1,
    0
  )
}

# Import Ad Names and Keywords
ad_names <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Create dummy variables for each advertisement
for (i in 1:nrow(ad_names)) {
  tweet_data[, paste0("Ad_", ad_names$Ad[i])] <- ifelse(
    grepl(ad_names$Keywords[i], tolower(tweet_data$text)),
    1,
    0
  )
}
```

### 3. Counting Tweets and Retweets per Brand

* **Count Tweets per Brand**: Calculates the total number of tweets mentioning each brand.
* **Count Retweets per Brand**: Sums up the retweets for tweets mentioning each brand.

```R
# Initialize data frames
Number_of_Tweets <- data.frame("Brand Name" = character(), "Number of Tweets" = numeric(), stringsAsFactors = FALSE)
Number_of_Retweets <- data.frame("Brand Name" = character(), "Number of Retweets" = numeric(), stringsAsFactors = FALSE)

# Calculate counts
for (i in grep("^Brand_", names(tweet_data))) {
  brand_name <- sub("^Brand_", "", names(tweet_data)[i])
  Number_of_Tweets <- rbind(Number_of_Tweets, data.frame(
    "Brand Name" = brand_name,
    "Number of Tweets" = sum(tweet_data[[i]])
  ))
  Number_of_Retweets <- rbind(Number_of_Retweets, data.frame(
    "Brand Name" = brand_name,
    "Number of Retweets" = sum(tweet_data$retweet_count[tweet_data[[i]] == 1])
  ))
}
```

### 4. Quarterly Keywords Analysis

* **Aggregate Keywords by Quarter**: Groups advertisement keywords by the quarter they were aired.
* **Count Tweets per Quarter**: Counts the number of tweets that mention keywords associated with each quarter.

```R
# Install and load qdap package
install.packages("qdap")
library(qdap)

# Aggregate keywords by quarter
Quarterly_Keywords <- aggregate(ad_names$Keywords, by = list(ad_names$Quarter), FUN = function(x) paste(x, collapse = "|"))
names(Quarterly_Keywords) <- c("Quarter", "Keywords")

# Count tweets per quarter
Quarterly_Keywords$Tweet_Count <- sapply(Quarterly_Keywords$Keywords, function(kws) {
  sum(grepl(kws, tolower(tweet_data$text)))
})

# Save to CSV
write.csv(Quarterly_Keywords, file = "Quarterly_Tweets.csv", row.names = FALSE)
```

### 5. Sentiment Analysis

* **Clean Tweet Text**: Preprocesses the tweet text to remove noise (punctuation, numbers, stopwords, URLs, extra whitespace).
* **Create Corpora for Each Brand**: Generates a text corpus for tweets mentioning each selected brand.
* **Perform Sentiment Analysis**: Uses the `syuzhet` package to calculate sentiment scores and categorize tweets as positive, negative, or neutral.

```R
# Load required libraries
library(tm)
library(syuzhet)

# Define brands for sentiment analysis
brands <- c("Marvel", "Budlight", "HBO_Budweiser", "Doritos", "Burger_King", "Verizon")

for (brand in brands) {
  # Extract tweets mentioning the brand
  brand_tweets <- tweet_data$text[tweet_data[[paste0("Brand_", brand)]] == 1]
  
  # Create corpus and clean text
  corpus <- Corpus(VectorSource(brand_tweets))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Convert to plain text
  clean_text <- sapply(corpus, as.character)
  
  # Perform sentiment analysis
  sentiments <- get_nrc_sentiment(clean_text)
  sentiment_values <- get_sentiment(clean_text)
  
  # Categorize tweets
  positive_tweets <- clean_text[sentiment_values > 0]
  negative_tweets <- clean_text[sentiment_values < 0]
  neutral_tweets  <- clean_text[sentiment_values == 0]
  
  # Optionally, save or display the results
  # ...
}
```

## Output Files

* **Number_of_Tweets.csv**: Contains the number of tweets mentioning each brand.
* **Number_of_Retweets.csv**: Contains the number of retweets for each brand.
* **Quarterly_Tweets.csv**: Contains the number of tweets per quarter based on advertisement keywords.
* **Sentiment Analysis Results**: The sentiment analysis results can be saved or visualized as needed.

## Notes

* **Data Consistency**: Ensure that the column indices used in the script match the structure of your tweet data CSV file. Adjust the column selections if necessary.
* **Keywords Accuracy**: The effectiveness of brand and advertisement identification depends on the accuracy and completeness of the keywords provided in `Brand_Keywords.csv` and `ad_names.csv`.
* **Sentiment Analysis Limitations**: Lexicon-based sentiment analysis may not accurately capture sarcasm, context, or idiomatic expressions common in social media language.

## Author

Abhijay Sharma

## Acknowledgments

* **qdap Package**: Used for text aggregation and manipulation.
* **tm Package**: Used for text mining and preprocessing.
* **syuzhet Package**: Used for sentiment analysis.
* **Twitter API**: Tweets data obtained via Twitter API.
* **OpenAI's GPT-4**: Assisted in generating and refining this README.
