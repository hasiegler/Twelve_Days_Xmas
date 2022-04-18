Creating The 12 Days of Christmas Poem
================
Henry Siegler

``` r
library(dplyr)
library(glue)
library(stringr)
library(purrr)
library(english)
```

``` r
xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")
```

### Overview

Consider the poem [The Twelve Days of
Christmas](https://internetpoem.com/anonymous/the-twelve-days-of-c-poem/).

The data below contains the crucial information about the gifts in the
song.

``` r
xmas
```

    ##    Day Day.in.Words Gift.Item       Verb Adjective       Location
    ## 1    1        first partridge       <NA>      <NA> in a pear tree
    ## 2    2       second      dove       <NA>    turtle           <NA>
    ## 3    3        third       hen       <NA>    french           <NA>
    ## 4    4       fourth      bird       <NA>   calling           <NA>
    ## 5    5        fifth      ring       <NA>    golden           <NA>
    ## 6    6        sixth     goose   a-laying      <NA>           <NA>
    ## 7    7      seventh      swan a-swimming      <NA>           <NA>
    ## 8    8       eighth      maid  a-milking      <NA>           <NA>
    ## 9    9        ninth      lady    dancing      <NA>           <NA>
    ## 10  10        tenth      lord  a-leaping      <NA>           <NA>
    ## 11  11     eleventh     piper     piping      <NA>           <NA>
    ## 12  12      twelfth   drummer   drumming      <NA>           <NA>

The goal is to use this data to recreate the entire [The Twelve Days of
Christmas](https://internetpoem.com/anonymous/the-twelve-days-of-c-poem/)
poem.

### Plurals

Note that the gifts are listed in singular: for example, on day five the
narrator receives “five golden rings”, but the entry in the dataset for
the gift on day five simply says “ring”.

``` r
# Function that takes a noun and makes it plural
# Arguments -- gift -- A string or vector of strings
# Return -- A string or vector of strings with the pluralized words

pluralize_gift <- function(gift){

  if (str_detect(gift, "y$")) {
    gift <- gift %>% 
      str_replace("y$", "ies")
  }
  
  else if (str_detect(gift, "oose$")) {
    gift <- gift %>% 
      str_replace("oose$", "eese")
  }
  else{
    gift <- gift %>% 
      str_replace("$", "s")
  }

return(gift)

}
```

### Test the Function

Try your function out on the smaller and then larger gift dataset.

``` r
pluralize_gift(xmas$Gift.Item)
```

    ##  [1] "partridges" "doves"      "hens"       "birds"      "rings"     
    ##  [6] "gooses"     "swans"      "maids"      "ladys"      "lords"     
    ## [11] "pipers"     "drummers"

``` r
purrr::map_chr(xmas$Gift.Item, pluralize_gift)
```

    ##  [1] "partridges" "doves"      "hens"       "birds"      "rings"     
    ##  [6] "geese"      "swans"      "maids"      "ladies"     "lords"     
    ## [11] "pipers"     "drummers"

### Creating sentences

The function `make_phrase`takes as input the necessary information, and
returns a phrase.

``` r
make_phrase <- function(num, num_word, item, verb, adjective, location) {
  
  ## Step 1: Replace NAs with blank strings
  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")
  
  ## Step 2: If the day is larger than 1, the items need pluralized! 
  if (num > 1) {
    item <- pluralize_gift(item)
  }
  
  ## Step 4:" Glue all of the pieces together! 
  str_c(num_word, " ", adjective, " ", item, " ", verb, " ", location)
}

#Test the function
make_phrase(num = 10, 
            num_word = "ten", 
            item = "lord", 
            verb = "a-leaping", 
            adjective = "", 
            location = "")
```

    ## [1] "ten  lords a-leaping "

### Test Your Function

Test the function out on the `xmas` data, by making a new variable
containing the daily phrases.

``` r
xmas2 <- xmas %>%
  mutate(day.num = as.character(english::english(Day)
                                ), 
    Full.Phrase = pmap_chr(
      list(num = Day,
           num_word = day.num,
           item = Gift.Item,
           verb = Verb,
           adjective = Adjective,
           location = Location),
      make_phrase
      )
  )
```

``` r
#This is what the new columns look like
xmas2 %>% 
  select(day.num, Full.Phrase) %>% 
  head(n = 3)
```

    ##   day.num                    Full.Phrase
    ## 1     one one  partridge  in a pear tree
    ## 2     two             two turtle doves  
    ## 3   three            three french hens

### Final Step – Iteration

The function called `sing_line()` takes as input:

-   A dataset

-   A number indicating which day to sing about

-   The name of a column in the dataset that contains the phrases for
    each day.

``` r
sing_line <- function(dataset, line, phrase_col){
  
  ## Step 1: Setup the intro line
  
  num_word <- as.character(ordinal(line))
  
  intro <- glue::glue("On the {num_word} day of Christmas, my true love sent to me:")
  
  ## Step 2: Sing the gift phrases
    phrases <- dataset %>% 
      pull( {{phrase_col}} )
  
  phrases <- phrases[line:1]

  
  if (line > 1) {
    phrases[line] <- str_replace(phrases[line], "one", "and a")
  }
  
  else {
    phrases[line] <- str_replace(phrases[line], "one", "a")
  }
  
  phrases <- str_squish(phrases)
  
  phrases <- str_to_title(phrases)
  
  ## put it together
  
  all_lines <- str_c(phrases, collapse = "\n")
  
  all_lines <- glue("{intro}\n{all_lines}")
  
  return(all_lines)
  
}
```

### A Small Test

``` r
xmas2 %>% 
  sing_line(line = 2, phrase_col = Full.Phrase)
```

    ## On the second day of Christmas, my true love sent to me:
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree

### Use the Function to Recreate the Entire Poem

``` r
map_chr(1:12, ~ sing_line(xmas2, .x, Full.Phrase)) %>%
  str_c(collapse = "\n\n") %>%
  cat()
```

    ## On the first day of Christmas, my true love sent to me:
    ## A Partridge In A Pear Tree
    ## 
    ## On the second day of Christmas, my true love sent to me:
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the third day of Christmas, my true love sent to me:
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the fourth day of Christmas, my true love sent to me:
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the fifth day of Christmas, my true love sent to me:
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the sixth day of Christmas, my true love sent to me:
    ## Six Geese A-Laying
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the seventh day of Christmas, my true love sent to me:
    ## Seven Swans A-Swimming
    ## Six Geese A-Laying
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the eighth day of Christmas, my true love sent to me:
    ## Eight Maids A-Milking
    ## Seven Swans A-Swimming
    ## Six Geese A-Laying
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the ninth day of Christmas, my true love sent to me:
    ## Nine Ladies Dancing
    ## Eight Maids A-Milking
    ## Seven Swans A-Swimming
    ## Six Geese A-Laying
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the tenth day of Christmas, my true love sent to me:
    ## Ten Lords A-Leaping
    ## Nine Ladies Dancing
    ## Eight Maids A-Milking
    ## Seven Swans A-Swimming
    ## Six Geese A-Laying
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the eleventh day of Christmas, my true love sent to me:
    ## Eleven Pipers Piping
    ## Ten Lords A-Leaping
    ## Nine Ladies Dancing
    ## Eight Maids A-Milking
    ## Seven Swans A-Swimming
    ## Six Geese A-Laying
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
    ## 
    ## On the twelfth day of Christmas, my true love sent to me:
    ## Twelve Drummers Drumming
    ## Eleven Pipers Piping
    ## Ten Lords A-Leaping
    ## Nine Ladies Dancing
    ## Eight Maids A-Milking
    ## Seven Swans A-Swimming
    ## Six Geese A-Laying
    ## Five Golden Rings
    ## Four Calling Birds
    ## Three French Hens
    ## Two Turtle Doves
    ## And A Partridge In A Pear Tree
