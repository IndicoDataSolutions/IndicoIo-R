indicoio
========

R client library for [indico's API](http://indico.io).

## Installation

```r
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

library(devtools)
devtools::install_github("IndicoDataSolutions/IndicoIo-R")
```

API Keys + Setup
----------------
For API key registration and setup, checkout our [quickstart guide](http://docs.indico.io/v2.0/docs/api-keys).

Full Documentation
------------
Detailed documentation and further code examples are available at [indico.reame.io](http://indico.readme.io/v2.0/docs/python).

Supported APIs:
------------

- Positive/Negative Sentiment Analysis
- Political Sentiment Analysis
- Image Feature Extraction
- Facial Emotion Recognition
- Facial Feature Extraction
- Language Detection
- Text Topic Tagging
-

Examples
--------
```r
library(indicoio)
```

### Positive/Negative Sentiment Analysis

```r
emotion <- sentiment("Thanks everyone for the birthday wishes!!
                      It was a crazy few days ><")
emotion
cat(sprintf("This text has %s tonality",
             ifelse(emotion > 0.5, "positive", "negative")))
```

### Political Sentiment Analysis

```r
affilation <- political("I am so proud to stand here today
                         as Prime Minister of four nations
                         in one United Kingdom.")
affilation
most.like <- names(affilation[which.max(unlist(affilation))])
least.like <- names(affilation[which.min(unlist(affilation))])
cat(sprintf("This text is most like %s and least like %s",
            most.like, least.like))

```

### Language Detection

```r
languages <- language("Monday: Delightful with mostly sunny skies.
                            Highs in the low 70s.")
languages
most.possible <- sort(unlist(languages), decreasing = TRUE)[1:2]
cat(sprintf("Detected %s language with probability %0.4f.\n",
            names(most.possible)[1], most.possible[1]))
cat(sprintf("Next possible is %s with probability %0.4f.",
            names(most.possible)[2], most.possible[2]))
```

### Text Tagging

```r
categories <- text_tags("Monday: Delightful with mostly sunny skies.
                   Highs in the low 70s.")
categories
most.possible <- sort(unlist(categories), decreasing = TRUE)[1:2]
cat(sprintf("Detected category \%s with probability \%0.4f.\\n",
            names(most.possible)[1], most.possible[1]))
cat(sprintf("Next possible is \%s with probability \%0.4f.",
            names(most.possible)[2], most.possible[2]))
```


### Face Emotion Detection

```r

## Example 1
library(base64enc)
img <- file(filepath, "rb", raw=TRUE)
data <- base64encode(img)

emotion <- face_emotion(data)

most.possible <- sort(unlist(emotion), decreasing = TRUE)[1:2]
cat(sprintf("Detected '%s' emotion with probability %0.4f.\n",
            names(most.possible)[1], most.possible[1]))
cat(sprintf("Next possible is '%s' emotion with probability %0.4f.",
            names(most.possible)[2], most.possible[2]))
```

### Face Features Detection

```r
features <- face_features(data)
length(features)
```

### Image Features Detection

```r
features <- image_features(data)

length(features)
min(unlist(features))
max(unlist(features))
sum(unlist(features))
```
Batch API
---------------
---------
Each `indicoio` function has a corresponding batch function for analyzing many examples with a single request. Simply pass in a list of inputs and receive a list of results in return.

```r
sentiment_scores = sentiment(c('Sample text', 'More sample text'), api_key='********'')
```
