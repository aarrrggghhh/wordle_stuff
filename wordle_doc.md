# Fun with the Wordle word list
Aner Moss

## Wordle

[Wordle](https://www.nytimes.com/games/wordle/index.html) is a popular
English word game published by the New York Times.

Every day, players get 6 attempts to guess a 5 letter word. After each
guess, the player is provided feedback. If a letter they guessed exists
in that day’s word, but is not in the correct position - it is colored
*yellow*. If the player correctly guessed a letter **in the correct
position**, it is colored *green*.

While there are many five letter words in the English language, Wordle
apparently uses only a 2,315 word list from which it picks the daily
word. The game will, however, accept guesses from a larger list of about
13,000 words (this allows the player to try out a wider range of
combinations in order to gain information about what the daily word is).
[^1]

One fun aspect of the game is the way it interfaces with probability and
language. We can poke at it to see if we can find ways to play that
improve our likelihood of success, which is what I’m about to do
(somewhat simplistically) below.

If you’re interested in a more comprehensive (and fantastic),
information theory based analysis of the game, you can look
[here](https://www.3blue1brown.com/lessons/wordle/). My rather
simplistic analysis grew from a more intuitive attempt to practice using
R to investigate the game.

## First thoughts and setting up

Since the goal of the game is to guess the daily word in the least
number of attempts, I decided to use the official possible word list to
figure out how many times each letter appeared in each of the different
positions within the limited subset of actual possible solutions.

I started by grabbing
[this](https://github.com/Kinkelin/WordleCompetition/blob/main/data/official/shuffled_real_wordles.txt)
word list, and importing into R (assuming the file is in the working
directory).

``` r
path <- paste0(getwd(),"/shuffled_real_wordles.txt")
wordlist <- read.table(path)
```

Then, I gave our only column a fitting name, and added columns for the
letter in each of the five positions for each word (plus changed
everything to uppercase):

``` r
library(tidyverse)
```

``` r
names(wordlist) <- "word"
wordlist <- wordlist |>
  separate(word, into = c("x1","x2","x3","x4","x5"), sep = c(1:5), remove = FALSE) |>   
  mutate(across(everything(), toupper))
```

Finally, I sorted the list alphabetically:

``` r
wordlist <- wordlist[order(wordlist$word),]
```

and got something that looks like this:

          word x1 x2 x3 x4 x5
    1426 ABACK  A  B  A  C  K
    942  ABASE  A  B  A  S  E
    511  ABATE  A  B  A  T  E
    400  ABBEY  A  B  B  E  Y
    115  ABBOT  A  B  B  O  T
    782  ABHOR  A  B  H  O  R

## Creating the letter frequency table

In my first attempt at counting how many times each letter appears in
each of the different positions, I used the table() function to do the
counting for me, and the base heatmap() function to visualize the
results, but reframe() and ggplot() ended up being more flexible, and
convenient. Since the wordlist isn’t tidy data, I started tidying by
pivoting the list longer:

``` r
wlist_tidy <- wordlist |> pivot_longer(x1:x5, names_to = "position")
```

Then I used reframe() to calculate how many times each letter appeared
in each position:

``` r
tidy_freq <- wlist_tidy |>
  group_by(value) |>
  reframe(x1 = sum(position == "x1"), x2 = sum(position == "x2"),
          x3 = sum(position == "x3"), x4 = sum(position == "x4"),
          x5 = sum(position == "x5"))
```

Which generated the frequency table by letter position:

    # A tibble: 26 × 6
       value    x1    x2    x3    x4    x5
       <chr> <int> <int> <int> <int> <int>
     1 A       141   304   307   163    64
     2 B       173    16    57    24    11
     3 C       198    40    56   152    31
     4 D       111    20    75    69   118
     5 E        72   242   177   318   424
     6 F       136     8    25    35    26
     7 G       115    12    67    76    41
     8 H        69   144     9    28   139
     9 I        34   202   266   158    11
    10 J        20     2     3     2     0
    # ℹ 16 more rows

Now we can use ggplot to plot a heatmap, allowing us to get some insight
about letter frequencies. To plot this with ggplot we need a tidy table
again. We can use melt() from the reshape package, or just manually
pivot_longer() again, and then plot using geom_tile():

``` r
molten_t_freq <- pivot_longer(tidy_freq, x1:x5, names_to = "position", values_to = "count")
ggplot(molten_t_freq, aes(position, value, fill = count)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 0) +
  theme_minimal()
```

![](wordle_doc_files/figure-commonmark/Letter%20frequency%20heatmap-1.png)

We can immediately see somewhat interesting results… for one - each
position has some letters that are clearly the most prevalent in it -
the most common first letter is S. A, O and R are the most frequent
second letters, A and I the most frequent third letters, and E is the
most frequent letter in positions 4 and 5, with Y coming in a close
second in the fifth position.

We can calculate the probability of getting a green in the first
position by dividing the number of times a letter appears in that
position in the word list by the total number of words. So for S, the
most common letter in the first position we’d get:

``` r
pull(tidy_freq[which(tidy_freq$value == "S"),"x1"] / sum(tidy_freq$x1))
```

    [1] 0.1580994

Or, just under 16%

# How to choose the best opening word

Armed with the letter frequency table, and the probabilities of each
letter choice generating a green, we can now create a function to
calculate the number of greens each word in the list is **expected** to
generate, by adding together the probabilities of each letter in the
word generating a green:

``` r
exp_greens <- function(x) {
  sum(tidy_freq[which(tidy_freq$value == substr(x, 1, 1)),2],
      tidy_freq[which(tidy_freq$value == substr(x, 2, 2)),3],
      tidy_freq[which(tidy_freq$value == substr(x, 3, 3)),4],
      tidy_freq[which(tidy_freq$value == substr(x, 4, 4)),5],
      tidy_freq[which(tidy_freq$value == substr(x, 5, 5)),6]) / 2315
}
```

If we add a column to the (original, non-tidy) wordlist with the
expected greens values,

``` r
wordlist <- wordlist %>% mutate(exp_greens = sapply(.$word, exp_greens))
```

we can then list the top 10 words with the highest expected greens
values:

``` r
head(wordlist[order(-wordlist$exp_greens),], n = 10)
```

          word x1 x2 x3 x4 x5 exp_greens
    1652 SLATE  S  L  A  T  E  0.6207343
    337  SAUCE  S  A  U  C  E  0.6095032
    393  SLICE  S  L  I  C  E  0.6086393
    1770 SHALE  S  H  A  L  E  0.6060475
    1104 SAUTE  S  A  U  T  E  0.6038877
    2267 SHARE  S  H  A  R  E  0.6017279
    1062 SOOTY  S  O  O  T  Y  0.6012959
    996  SHINE  S  H  I  N  E  0.5969762
    1673 SUITE  S  U  I  T  E  0.5965443
    1981 CRANE  C  R  A  N  E  0.5952484

Of the words that can be a possible solution in the game - SLATE is
likeliest to generate greens. It’s expected to generate about 0.6 greens
when played. If we could play any combination of letters (one can’t, the
word has to be on the playable guess list), we’d choose the most common
letter in each position and play the “word” SAAEE. That would generate
an expected green value of

``` r
exp_greens("SAAEE")
```

    [1] 0.7425486

which is quite a lot better than SLATE.

This analysis is quite simplistic because it only takes into
consideration greens. We can, likely, take a few more tentative steps
before having to fully dive into information theory, entropy and bits.

# Word affinities

One way to go about this is to somehow calculate a measure of how close,
or similar any two words are. Then we can look for the word that is the
“most close” to the largest number of possible answers to the puzzle,
and perhaps discover more intricate insights.

I chose a very loose scoring method here - for any two words, if they
shared a letter in the same position (== green) that would score 2
points, and if they shared letters, but **not** in the same position (==
yellow) that would add 1 point to the score. Under this setup the
maximum score would be 10 (words could only score a 10 with themselves),
and the minimum score would be 0 (for words that shared no letters at
all).

I set off by creating a large matrix with all the words from the word
list along each of the axes (all values would be NAs to start):

``` r
affMatrix <- matrix(nrow = nrow(wordlist),
                    ncol = nrow(wordlist),
                    dimnames = list(wordlist$word, wordlist$word))
```

and then write a function to calculate the affinity score between any
two words:

``` r
affScore <- function(x, y) {
    first <- strsplit(x, "")[[1]]             # split the words into letter vectors
    second <- strsplit(y, "")[[1]]
    score <- 0                                # score starts at 0
    
# first we'll loop through the words to find matches in identical 
# positions and if we do increment score by 2 points.
# We also remove those letters from the words, so that we don't "double" score anything later.

    for(i in 1:length(first)) {
        ifelse(first[i] == second[i], {
            score <- score + 2
            first[i] <- NA
            second[i] <- NA
        }, 0)
    }

# Now we need to look for matching letters that are NOT in identical positions.
# So for every letter in the first word, we loop through the remaining letters
# in the second word (we reset earlier matches to NA) looking for a match.
# If we find one, we reset THOSE letters to NA so we don't match on them again,
# and increment the score by 1.    

    for(k in 1:length(first)) {
        for(l in 1:length(first)) {
            ifelse(first[k] == second[l], {
                score <- score + 1
                first[k] <- NA
                second[l] <- NA
            }, 0)
        }
    }
  score
}
```

To populate the word affinity matrix with affinity scores, we need to
run the function on every pair of words (we’ll run the function on the
lower triangle of this symmetric matrix).

**Warning** - even running the function on only half of the matrix still
requires ~ 2.7 million operations - so this takes a while.

``` r
for(i in 1:nrow(wordlist)) {
    k <- 1
    while(k < i) {
        affMatrix[i,k] <- affScore(dimnames(affMatrix)[[1]][i], dimnames(affMatrix)[[2]][k])
        k <- k + 1
    }
}
```

Phew! Here’s the top left corner of our matrix:

``` r
affMatrix[1:10,1:10]
```

          ABACK ABASE ABATE ABBEY ABBOT ABHOR ABIDE ABLED ABODE ABORT
    ABACK    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ABASE     6    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ABATE     6     8    NA    NA    NA    NA    NA    NA    NA    NA
    ABBEY     4     5     5    NA    NA    NA    NA    NA    NA    NA
    ABBOT     4     4     5     6    NA    NA    NA    NA    NA    NA
    ABHOR     4     4     4     4     6    NA    NA    NA    NA    NA
    ABIDE     4     6     6     5     4     4    NA    NA    NA    NA
    ABLED     4     5     5     6     4     4     6    NA    NA    NA
    ABODE     4     6     6     5     5     5     8     6    NA    NA
    ABORT     4     4     5     4     7     6     4     4     6    NA

For the next step, we’ll want the top triangle of the matrix populated
as well, so we mirror the bottom triangle:

``` r
affMatrix[upper.tri(affMatrix)] <- t(affMatrix)[upper.tri(t(affMatrix))]
```

Finally, we can take the incredibly rough and tumble metric of just
summing up the affinity scores for each word, to find the words with the
“most closeness” to all other words. These words will have the greatest
likelihood of generating greens and yellows against all possible puzzle
answers.

``` r
affinityScores <- rowSums(affMatrix, na.rm = TRUE)
head(affinityScores[order(-affinityScores)], n = 10)
```

    STARE AROSE RAISE ARISE SLATE SANER SNARE IRATE CRATE STALE 
     5403  5330  5327  5326  5325  5299  5296  5277  5242  5224 

Notice, that SLATE only comes in fifth under this metric. R is a far
more common letter than L in the solution list, so trading out a few
additional greens for more yellows has made STARE a more attractive
candidate under this admittedly clunky “refinement”.

Regardless, whether you AROSE, continue to ARISE, are growing
increasingly IRATE with this process, or choose to SNARE the SANER word
option, these are all solid choices for your opening Wordle guess on
tomorrow’s puzzle.

I’ll leave the conversation about optimizing the **second guess** to
another life.

[^1]: One sample source -
    <https://puzzlecottage.com/word-lists/wordle-word-list>
