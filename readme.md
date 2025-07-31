# shinymemorytest 

`shinymemorytest` lets you run a rapid study–test recognition
experiment in a web browser with no Shiny boiler-plate.  
Supply a small data frame that lists your stimuli,
click **Start**, and the package handles stimulus timing, response
collection, scoring, and data export.


## Key features

* Multi-block design – any number of study/test blocks, presented in order. 
* Precise stimulus timing with the `later` event loop – no `Sys.sleep()` hacks.
* Fully responsive Shiny UI; works locally or on a Shiny Server.
* Immediate return of a tidy data frame (`block`, `item`, `response`, `correct`).
* One-line launch: `run_experiment(stimuli_df)`. 


## Installation

```r
# install.packages("devtools")
devtools::install_github("mark-andrews/shinymemorytest")
```

## Example script

```r
library(tibble)

stimuli_df <- tribble(
  ~block, ~type, ~item,
  # -------- Block 1: Fruit ---------------------------------------
  1, "study", "apple",
  1, "study", "banana",
  1, "study", "cherry",
  1, "study", "date",
  1, "study", "elderberry",
  1, "study", "fig",
  1, "study", "grape",
  1, "study", "kiwi",
  1, "test", "banana", # old
  1, "test", "elderberry", # old
  1, "test", "mango", # new
  1, "test", "peach", # new
  1, "test", "grape", # old
  1, "test", "plum", # new
  1, "test", "fig", # old
  1, "test", "coconut", # new

  # -------- Block 2: Animals ------------------------------------
  2, "study", "cat",
  2, "study", "dog",
  2, "study", "mouse",
  2, "study", "rabbit",
  2, "study", "horse",
  2, "study", "sheep",
  2, "study", "goat",
  2, "study", "pig",
  2, "test", "dog", # old
  2, "test", "sheep", # old
  2, "test", "zebra", # new
  2, "test", "otter", # new
  2, "test", "cat", # old
  2, "test", "bear", # new
  2, "test", "goat", # old
  2, "test", "panda", # new

  # -------- Block 3: Colours ------------------------------------
  3, "study", "red",
  3, "study", "blue",
  3, "study", "green",
  3, "study", "yellow",
  3, "study", "orange",
  3, "study", "purple",
  3, "study", "brown",
  3, "study", "pink",
  3, "test", "red", # old
  3, "test", "navy", # new
  3, "test", "blue", # old
  3, "test", "teal", # new
  3, "test", "green", # old
  3, "test", "beige", # new
  3, "test", "yellow", # old
  3, "test", "maroon" # new
)

results <- run_experiment(stimuli_df,
  display_duration = 800, # in ms
  ISI              = 800
)
```
