<!-- badges: start -->

[![R-CMD-check](https://github.com/MUCollective/multiverse/workflows/R-CMD-check/badge.svg?branch=dev)](https://github.com/MUCollective/multiverse/actions)
[![codecov](https://codecov.io/gh/MUCollective/multiverse/branch/master/graph/badge.svg?token=LsJtjiw42J)](https://codecov.io/gh/MUCollective/multiverse)
[![CRAN
status](https://www.r-pkg.org/badges/version/multiverse)](https://cran.r-project.org/package=multiverse)
![Download
count](https://cranlogs.r-pkg.org/badges/last-month/multiverse)

<!-- badges: end -->

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(ggplot2)
library(purrr)
library(broom)
library(gganimate)
library(ggdist)
library(multiverse)
#> Loading required package: knitr
#> 
#> Attaching package: 'multiverse'
#> The following object is masked from 'package:tidyr':
#> 
#>     expand
```

# Multiverse: An R package for creating multiverse analysis

In any end-to-end analysis there likely exists points in the analysis
where researchers have to make a decision between two or more equally
defensible choices (e.g., different ways of excluding outliers,
different data transformations). In a multiverse analysis, researchers
identify all the possible decision points for an analysis, determine
alternative analysis steps at each decision point, implements them all,
and then report the outcomes of all analyses resulting from all possible
choice combinations.

However, declaring several alternative analysis paths can be tricky,
often requiring custom control flows such as nested for-loops and
multiple if-else statements. The **goal** of `multiverse` is to allow
users to create a multitude of end-to-end analyses in a concise and
easily interpretable manner. `multiverse` enables this by providing both
an embedded Domain-Specific Language (DSL) as well as an API to interact
with and extract results from a multiverse analysis, which can then be
neatly wrapped within a larger analysis in R.

For more background on what a *multiverse analysis* is, please refer to
the works of Steegen et al., who first put forth the concept of
[multiverse
analysis](https://journals.sagepub.com/doi/pdf/10.1177/1745691616658637),
and Simonsohn et al., who put forth a similar notion called the
[Specification curve
analysis](https://repository.upenn.edu/cgi/viewcontent.cgi?article=1314&context=marketing_papers).

The `multiverse` documentation predominantly follows the tidyverse
syntax.

## Installation

You can install the latest development version from GitHub with these R
commands:

``` r
install.packages("devtools")
devtools::install_github("mucollective/multiverse")
```

## Overview

In this document, we will provide you with details on how to quickly get
started with this package. Please refer to the following vignettes for
further information on:

-   How `multiverse` can be used in different environments such as in
    RMarkdown and RScripts. See `vignette("multiverse-in-rmd")`.
-   How alternate analysis paths are declared in `multiverse` using
    branch, and how `multiverse` processes the user-declared
    (`multiverse` DSL) code to create multiple end-to-end executable R
    analysis code. See `vignette("branch")`.
-   How conditions and dependencies can be declared in `multiverse`;
    conditions can be used to state when two steps in a multiverse are
    incompatible with one another. See `vignette("conditions")`.
-   How multiverse results can be extracted and visualised. . See
    `vignette("visualising-multiverse")`.

We also implement a series of end-to-end multiverse implementations
using this package to demonstrate how it might be used (which can be
found in the
[vignettes](https://mucollective.github.io/multiverse/articles/)):

-   Steegen et al.’s [original multiverse
    analysis](https://journals.sagepub.com/doi/pdf/10.1177/1745691616658637)
    of the paper [The fluctuating female vote: Politics, religion, and
    the ovulatory cycle](https://doi.org/10.1177/0956797612466416); can
    also be found **below**.
-   Simonsohn et al.’s [specification curve
    analysis](http://dx.doi.org/10.2139/ssrn.2694998) of the paper
    [Female hurricanes are deadlier than male
    hurricanes](https://doi.org/10.1073/pnas.1402786111)
-   Dragicevic et al.’s mini-paper [Adding Inferential Information to
    plots using Resampling and
    Animations](https://explorablemultiverse.github.io/examples/dance/)
    (from [Increasing the transparency of research papers with
    explorable multiverse
    analyses](https://hal.inria.fr/hal-01976951/document) )
-   Dragicevic et al.’s mini-paper [Re-evaluating the efficiency of
    Physical
    Visualisations](https://explorablemultiverse.github.io/examples/frequentist/)
    (from [Increasing the transparency of research papers with
    explorable multiverse
    analyses](https://hal.inria.fr/hal-01976951/document) )

## Example analysis

In this document, we outline an initial approach to conducting a
multiverse analysis in R. We will show how our package can be used to
perform the multiverse analysis outlined by Simohnsohn et al. in
[Specification Curve: Descriptive and Inferential Statistics on All
Reasonable Specifications](http://dx.doi.org/10.2139/ssrn.2694998) where
they reanalysed the study titled [Female hurricanes are deadlier than
male hurricanes](https://doi.org/10.1073/pnas.1402786111).

### Background: The Data

The dataset used by Jung et al., in their study [Female hurricanes are
deadlier than male hurricanes](https://doi.org/10.1073/pnas.1402786111),
contained information on 94 hurricanes from a list published by National
Oceanic and Atmospheric Administration (NOAA). For each storm, the
authors compiled information on the year (`year`), number of deaths
(`deaths`), minimum pressure (`pressure`), maximum wind speed at time of
landfall (`wind`), dollar amount of property damages (`damage`) and
hurricane severity or category of the storm (category). Nine independent
coders were asked to rate the names of the hurricanes on a two-item
11-point scale (1 = more masculine; 11 = more feminine), and the
`femininity` of each name was computed as the mean of these two items.

We first load the raw data and store it as a tibble. The data is
provided with the package and can be loaded using the `data("hurricane)`
command.

``` r
data("hurricane")
hurricane_data <- hurricane %>%
    # rename some variables
    rename(
        year = Year,
        name = Name,
        dam = NDAM,
        death = alldeaths,
        female = Gender_MF,
        masfem = MasFem,
        category = Category,
        pressure = Minpressure_Updated_2014,
        wind = HighestWindSpeed
    ) %>%
    # create new variables
    # which are relevant later on
    mutate(
        post = ifelse(year>1979, 1, 0),
        zcat = as.numeric(scale(category)),
        zpressure = -scale(pressure),
        zwind = as.numeric(scale(wind)),
        z3 = as.numeric((zpressure + zcat + zwind) / 3)
    )
```

The data look like this:

``` r
hurricane_data %>%
  head()
#>   year     name  masfem MinPressure_before pressure female category death
#> 1 1950     Easy 5.40625                958      960      0        3     2
#> 2 1950     King 1.59375                955      955      0        4     4
#> 3 1952     Able 2.96875                985      985      0        1     3
#> 4 1953  Barbara 8.62500                987      987      1        1     1
#> 5 1953 Florence 7.87500                985      985      1        1     0
#> 6 1954    Carol 8.53125                960      960      1        3    60
#>   wind   dam Elapsed.Yrs Source post       zcat  zpressure       zwind
#> 1  125  2380          63    MWR    0  0.8281862  0.2017975 -0.02006244
#> 2  134  7220          63    MWR    0  1.7661320  0.4513891  0.27257241
#> 3  125   210          61    MWR    0 -1.0477054 -1.0461607 -0.02006244
#> 4   75    78          60    MWR    0 -1.0477054 -1.1459973 -1.64581157
#> 5  115    21          60    MWR    0 -1.0477054 -1.0461607 -0.34521226
#> 6  115 24962          59    MWR    0  0.8281862  0.2017975 -0.34521226
#>           z3
#> 1  0.3366404
#> 2  0.8300312
#> 3 -0.7046428
#> 4 -1.2798381
#> 5 -0.8130261
#> 6  0.2282571
```

### A single data set analysis: one possible analysis among many

The original analysis removed the two hurricanes with the highest death
toll as outliers. To test their hypothesis that hurricanes with more
feminine names result in more deaths, the authors fit a negative
binomial model using the number of deaths as the response variable (due
to some issues with implementing the negative binomial model in R, we
approximate it by fitting a poisson model instead). For predictors, they
use `femininity`, `damages`, standardised value of pressure
(`zpressure)`, interaction between `femininity` and `damages`, and the
interaction between `femininity` and `zpressure`.

The following code block contains the steps involved in implementing the
original analysis:

``` r
df.filtered = hurricane_data %>% 
  filter(name != "Katrina" & name != "Audrey") %>%
  mutate(zpressure = -scale(pressure))

fit = glm(
  death ~ masfem * dam + masfem * zpressure,
  data = df.filtered,
  family = "poisson"
)
```

The result below indicates that there is a small but positive effect of
`masfem` (femininity of the name of a hurricane) on `deaths`, when
controlled for damages. This appears to support the original hypothesis.

``` r
tidy(fit) %>%
  filter(term != "(Intercept)") %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointinterval(aes(x = estimate, y = term, xmin = estimate + qnorm(0.025)*std.error, xmax = estimate + qnorm(0.975)*std.error)) +
  theme_minimal()
```

![](README_files/one_universe_result-1.png)

However, the original analysis involved at least four decisions (A-D),
and at each decision point alternative choices may have led to a
different result. These decisions are highlighted in the figure below:

![An implementation of the original (single-universe) analysis in
R.](man/figures/00-default-analysis.png)

Several subsequent studies, each proposing a different analysis
strategy, found no presence of such an effect, suggesting that the
original finding may have been a result of a idiosyncratic combination
of analysis choices. Data analysis can often involve several decisions
involving two or more options. In most statistical analysis, these
decisions are taken by the researcher based on some reasonable
justification. However, for several decisions, there can be more than
one reasonable option to choose from. A multiverse analysis makes all
such decisions explicit and conducts the complete analysis for all
combinations of options (of each decision). Below, we use this analysis
as an example of how a single analysis can be extended to a multiverse
analysis.

## Multiverse specification

`multiverse` provides flexible functions which can be used to easily
multiplex over alternative analysis steps, and perform a multiverse
analysis. To describe both the features of multiverse and to sketch out
how an analyst might progressively create a multiverse from the bottom
up, we describe how to modify the traditional, single-universe analysis
from the previous figure in to a multiverse analysis.

The first step is to load the library and define a *new multiverse*,
which is the variable `M`. We will use this multiverse object to create
a set of universes, each representing a different way of analysing our
data.

``` r
#load the library
library(multiverse)

#create multiverse object
M = multiverse()
```

### Two ways to building a multiverse

Through the `multiverse` DSL, users are specifying multiple analysis
paths at the same time. The DSL cannot be executed directly in an R
environment or R code chunk and needs to be declared, processed and
executed in a special environment. To be more precise, `multiverse`
takes the user declared code, parses and rewrites the code into multiple
versions of valid R code, each corresponding to an unique analysis path
in the multiverse. For more information on this processing step, see
vignette(branch)

To get around these limitations, we need to declare this (multiverse
DSL) code “inside a multiverse object”. The `multiverse` package
facilitates this through some boilerplate code:

-   *multiverse code chunks*: allows users to declare multiverse code in
    a dedicated code chunk, and is more consistent with the interactive
    programming interface of RStudio.
-   the `inside()` function: allows users to declare multiverse code in
    RScripts (or withn regular R code blocks).

**Note** that the `inside` function is more suited for a script-style
implementation. When using the interactive programming interface of
RStudio, user should use `multiverse code chunks`.

#### Multiverse code blocks

RMarkdown [supports languages other than
R](https://bookdown.org/yihui/rmarkdown/language-engines.html) and these
languages have dedicated code blocks. We extend this by providing
[*multiverse code
blocks*](https://mucollective.github.io/multiverse/articles/multiverse-in-rmd.html)
which can be used instead of the regular `r` code block to write code
inside a multiverse object (see \\link{multiverse-in-rmd} for more
details on using the multiverse code blocks with RMarkdown). A
*multiverse code block* is a custom engine designed to work with the
`multiverse` package, to implement the multiverse analyses. This allows
you to write more concise code and is more consistent with the
interactive programming interface of RStudio. Below we show how code can
be implemented using the *multiverse code block:*

    ```{multiverse default-m-1, inside = M}
    # here we just create the variable `df` in the multiverse
    df = hurricane_data

    # here, we perform a `filter` operation in the multiverse
    df.filtered = df %>%
      filter(branch(death_outliers,
          "no_exclusion" ~ TRUE,
          "most_extreme" ~ name != "Katrina",
          "two_most_extreme" ~ !(name %in% c("Katrina", "Audrey"))
    ))
    ```

The code within the `filter` function call is written in the
`multiverse` DSL and cannot be executed directly in R. For now, ignore
what the `branch` function does as we will discuss about this in more
detail in the next section. When this code is written and executed
inside a *multiverse code block*, it allows the multiverse library to
process and compile it to three different analyses.

We provide the ability to declare multiverse code block as an *Addin* in
RStudio. Users can click on *Addins* toolbar menu in RStudio (see the
image below). This would create a multiverse code block at the location
of the cursor in the document.

<img src="vignettes/multiverse-addins.png" width="90%" />

Alternately, users can insert a multiverse code block using a keyboard
shortcut. Users can create a keyboard shortcut to declare a multiverse
code block inside a RMarkdown document through the following steps:

-   Tools > Addins > Browse Addins… > Keyboard Shortcuts
-   Next, in the filter input field, type *multiverse*. You will see one
    result with “Insert multiverse code chunk” as the name.
-   Click on the Shortcut field and press Cmd+Option+M (on Mac OS) or
    Ctrl+Shift+Alt+M (on Windows).
-   Click “Apply” and exit the dialog box

Please refer to \\link{multiverse-in-rmd} for more details on using the
multiverse code blocks with RMarkdown. The vignette also contains
information on steps for debugging some of the common problems in
assigning keyboard shortcuts.

#### `inside()`

Alternatively, when working with RScripts (or in a regular `r` code
block), users can make use of the `inside()` function to write code
inside a multiverse object. `inside()` takes in two arguments:

1.  the multiverse object, M; and
2.  the code for the analysis (including branches). Note that if you are
    passing multiple expressions, they should be enclosed within `{}`.

Note that `inside()` is primarily designed for script style programming.
If a user is working with an RScript, the previous code can be declared
“inside the multiverse object” using the `inside()` function as follows:

``` r
# here we just create the variable `df` in the multiverse
inside(M, df = hurricane_data)

# here, we perform two `mutate` operations in the multiverse.
# although they could have been chained, this illustrates 
# how multiple variables can be declared together using the `{}`
inside(M, {
  df.filtered = df %>%
    filter(branch(death_outliers,
        "no_exclusion" ~ TRUE,
        "most_extreme" ~ name != "Katrina",
        "two_most_extreme" ~ !(name %in% c("Katrina", "Audrey"))
  ))
})
```

In the rest of this vignette, we will use **multiverse code blocks** to
specify the multiverse. Please refer to the vignette
(`vignette("multiverse-in-rmd")`) for more details on **declaring
multiverse analyses in both RMarkdown and RScripts**

### Declaring alternative analysis

After you’ve specified the appropriate boilerplate which is necessary to
use the `multiverse` DSL, the next step is to define our possible
alternate analysis paths. The multiverse package includes functions that
aim to make it easy to declare multiple alternate choices at each
analysis decision point. We do this by enabling analysts to declare code
using syntax which is as close to that of a single universe analysis as
possible. Consider these first few lines from the transformation code in
the single analysis above:

``` r
df.filtered = hurricane_data %>% 
  filter(name != "Katrina" & name != "Audrey")
```

Here, the researchers are faced with the decision of which hurricanes to
exclude as outliers. They decide to exclude the two hurricanes which
have caused the most deaths. However, this decision is arbitrary. Why
not include all hurricanes? Why not exclude only the one with most
deaths? Thus we could have three possible ways of removing outliers
based on extreme number of deaths:

-   No exclusion
-   Remove one most extreme hurricane
-   Remove two most extreme hurricanes

To create a multiverse that includes these three possible processing
options, we use the `branch()` function. The `branch()` function accepts
three or more arguments. The first argument defines a *parameter* (here
`death outliers`) which is used as an identifier for the decision point.
The subsequent arguments, which we refer to as *options*, define the
different choices that a researcher can make at that decision point of
their analysis; these follow the syntax
`<option_name> ~ <option_definition>`. The `<option_name>` part is
intended to allow naming the branches with meaningful names to help the
user keep track of choices (in the multiverse specification below,
“no_exclusion”, “most_extreme”, “two_most_extreme” are used as option
names). However, names can be omitted; if omitted, the entire syntax for
performing that operation will be treated as the name for that
particular option.

Putting it all together, a decision point in a multiverse analysis can
thus be declared as:

    ```{multiverse branch_definition, inside = M}
    # here we just create the variable `df` in the multiverse
    df = hurricane_data

    # here, we perform a `filter` operation in the multiverse
    df.filtered = df %>%
      filter(branch(death_outliers,
          "no_exclusion" ~ TRUE,
          "most_extreme" ~ name != "Katrina",
          "two_most_extreme" ~ !(name %in% c("Katrina", "Audrey"))
    ))
    ```

The `multiverse` library then takes this user-declared syntax in the
multiverse DSL and and compiles it into three separate, executable R
expressions as shown in the figure below:

![branch() allows declarations of alternative ways of performing an
analysis step which is multiplexed over, to create the
multiverse](man/figures/02-branch.png)

More details on the `branch()` function can be found in the
corresponding `vignette(branch)`.

## Interfacing with the multiverse

Once you add the code to the multiverse, it automatically processes the
code to identify the `parameters` and the corresponding `options` that
have been defined for each parameter.

Once the code has been added, the `multiverse` object will have the
following attributes:

1.  `parameters`, which is a list of parameters

``` r
parameters(M)
#> $death_outliers
#> $death_outliers[[1]]
#> [1] "no_exclusion"
#> 
#> $death_outliers[[2]]
#> [1] "most_extreme"
#> 
#> $death_outliers[[3]]
#> [1] "two_most_extreme"
```

1.  `conditions`, which is a list of conditions (we’ll define this
    later)

2.  `expand` returns a table where each row corresponds to a single
    analysis path (i.e., a single universe). This view provides the user
    with the information of which choices have resulted in the analysis
    path, along with the entire unevaluated code expression
    corresponding to each analysis. Analysts can use this table to
    explore multiverse specifications with all the tools available in R
    and RStudio for exploring data tables.

``` r
expand(M)
#> # A tibble: 3 × 5
#>   .universe death_outliers   .parameter_assignment .code            .results
#>       <int> <chr>            <list>                <list>           <list>  
#> 1         1 no_exclusion     <named list [1]>      <named list [1]> <env>   
#> 2         2 most_extreme     <named list [1]>      <named list [1]> <env>   
#> 3         3 two_most_extreme <named list [1]>      <named list [1]> <env>
```

1.  `code`, which is the code that the user passes to the multiverse to
    conduct a multiverse analysis. However, we do not execute this code
    and it is stored unevaluated. The user can interactively edit and
    rewrite this code, and can execute it for the current analysis or
    the entire multiverse using dedicated functions.

``` r
code(M)
#> $branch_definition
#> {
#>     df = hurricane_data
#>     df.filtered = df %>% filter(branch(death_outliers, "no_exclusion" ~ 
#>         TRUE, "most_extreme" ~ name != "Katrina", "two_most_extreme" ~ 
#>         !(name %in% c("Katrina", "Audrey"))))
#> }
```

1.  `extract_variables(M, <variable names>)` extracts the supplied
    variable from the results of each analysis path, returning a table
    similar to the output of `expand(M)`, but with new columns for each
    variable that has been extracted. This would allow an analyst to,
    for example, extract summary statistics or even entire data tables
    from all universes simultaneously. These columns can easily be
    turned into long format data tables using the `tidyverse` packages
    and then visualized using the `ggplot2` package

``` r
extract_variables(M, df.filtered)
#> # A tibble: 3 × 6
#>   .universe death_outliers   .parameter_assign… .code    .results df.filtered
#>       <int> <chr>            <list>             <list>   <list>   <list>     
#> 1         1 no_exclusion     <named list [1]>   <named … <env>    <df [92 × …
#> 2         2 most_extreme     <named list [1]>   <named … <env>    <df [92 × …
#> 3         3 two_most_extreme <named list [1]>   <named … <env>    <df [92 × …
```

## Building up a complete analysis

Subsequent branch calls will progressively expand the multiverse, by
enumerating all possible combinations. We will now expand our multiverse
to include other possible decision points in the analysis where
reasonable alternatives could have been chosen. For instance, the
researchers could have:

1 used a binary indicator for whether a hurricane’s name was female,
instead of the 11-point rating of how feminine the name of a hurricane
was

2 log transformed the `damage` variable, as it is a positive only value
with a long right-tail

This would result in $3 \\\\times 2 \\\\times 2 = 12$ analysis paths.

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = masfem, damage = identity(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = masfem, damage = log(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = female, damage = identity(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = female, damage = log(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = masfem, damage = identity(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = masfem, damage = log(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = female, damage = identity(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = female, damage = log(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = masfem, damage = identity(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = masfem, damage = log(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = female, damage = identity(dam))
```

``` r
df.filtered <- df.filtered %>%
    mutate(femininity = female, damage = log(dam))
```

    ```{multiverse label = variable_definitions, inside = M}`r''`
    df.filtered <- df.filtered %>%
        mutate(
            femininity = branch(femininity_calculation,
              "masfem" ~ masfem,
              "female" ~ female
            ),
            damage = branch(damage_transform,
              "no_transform" ~ identity(dam),
              "log_transform" ~ log(dam)
            )
        )
    ```

The next step in this multiverse analysis is to estimate the effect of
`femininity` on `deaths` using linear regression. There are multiple
decisions involved at this step. We take this opportunity to introduce
certain scenarios that an analyst may encounter while attempting to
create a multiverse, that may be difficult to discover but are supported
by the library.

### Reusing analysis parameters in `branch()`

One way of implementing this regression analysis may be a poisson model
with the number of deaths caused by the hurricane as the response
variable. Recall that this is specified as:

``` r
fit = glm(death ~ masfem * dam + masfem * zpressure, data = df.filtered, family = "poisson")
```

Although this is a reasonable choice for count data, one can also argue
for a log-linear regression model instead, as count data such as deaths
tend to be approximately log-normally distributed.

Specifying this in a multiverse analysis may require changes in two
different locations in the code: the specification of the `deaths`
dependent variable and the value of the `family` argument. However,
these are not two separate decisions, but rather a consequence of the
same analysis parameter: the choice of model. Often, a single analysis
parameter will require the analyst to change the code in more than one
location. To represent these semantics, multiverse allows us to re-use
the same analysis parameter in multiple `branch()` statements, so long
as each `branch()` uses the exact same set of analysis options. In a
branch on a previously defined parameter, option names must be the same,
but the code for each option can be different. Thus, we represent the
consequences of the choice of model with a single analysis parameter:
model. We insert two branch() statements using this parameter, one to
set the variable transformation and one to set the family.

    ```{multiverse label = variable_definitions, inside = M}`r''`
    fit <- glm(branch(model,
            "linear" ~ log(death+1),
            "poisson" ~ death
        ) ~ femininity * damage + femininity * zpressure,
        family = branch(model, 
            "linear" ~ gaussian, 
            "poisson" ~ poisson
        ), data = df.filtered)
    ```

### Specifying conditions in the multiverse analysis

Another decision that was made at this step was the choice of
predictors, which includes the interaction between `femininity` and
`damage`, and between `femininity` and `zpressure.` Here, the predictors
`damage` and `zpressure` are used as measures of the storm severity,
with the interaction between femininity and damage indicating whether
the main effect is stronger in more “destructive” or “severe” storms.
Yet again, there are other reasonable approaches to study the primary
effect which may include *no interaction term* or only include
interactions between femininity and variables such as pressure, wind or
category in conjunction with the interaction between femininity and
damage.

In a multiverse analysis, there may arise such dependencies between two
or more analysis parameters that make certain analysis paths
inconsistent, or even impossible. In other words, the applicability of
some analysis options may be conditional on a previous, upstream
decision. By default, multiverse assumes all combinations of options are
valid. However, it provides a flexible way to specify that an analysis
option is incompatible with previously-specified analysis options. These
dependencies can be specified using the `%when%` operator followed by a
boolean expression, right after the option name. Here, interaction term
between `femininity` and `zpressure` only make sense in the presence of
the interaction involving `damage`, so we use a `%when%` expression in
the definition of the `other_predictors` analysis parameter.

This results in the following multiverse specification (for now, let’s
ignore the previous decision on choice of models):

    ```{multiverse label = variable_definitions, inside = M}`r''`
    fit <- glm(death ~ 
              branch(main_interaction,
                  "no" ~ femininity + damage,
                  "yes" ~ femininity * damage
              ) + branch(other_predictors,
                  "none" ~ NULL,
                  "pressure" %when% (main_interaction == "yes") ~ femininity * zpressure,
                  "wind" %when% (main_interaction == "yes") ~ femininity * zwind,
                  "category" %when% (main_interaction == "yes") ~ femininity * zcat,
                  "all" %when% (main_interaction == "yes") ~ femininity * z3,
                  "all_no_interaction" %when% (main_interaction == "no") ~ z3
              ), family = "poisson", data = df)
    ```

For more details on how this can be done, as well as other details on
conditions, please refer to vignette(conditions).

## Executing multiverse code

As with code chunks in a typical computational notebook, users can
execute a multiverse code chunk in the interactive editor in RStudio.
When a user executes a single code chunk, multiverse internally
transforms the input from that code chunk into one unevaluated
expression (R’s internal representation of an abstract syntax tree) for
each unique combination of analysis options and immediately executes the
default analysis: the analysis path obtained by taking the first
analysis option at each decision point. The default analysis is executed
in the current active R environment (i.e. the same environment that
regular R code blocks are executed in — the R Global Environment). Thus,
the result of the default analysis is always accessible to the user. The
output of an executed code chunk (text or visualization) is displayed
immediately below it, mimicking notebook code chunks

The following code chunk illustrates this behavior. It fits the
regression model that has been described in the previous two sections,
and prints the summary of the regression model. The result will be
printed below:

    ```{multiverse label = default-m-3, inside = M}`r''`
    fit = glm(branch(model, "linear" ~ log(death + 1), "poisson" ~ death) ~ 
              branch(main_interaction,
                  "no" ~ femininity + damage,
                  "yes" ~ femininity * damage
              ) + branch(other_predictors,
                  "none" ~ NULL,
                  "pressure" %when% (main_interaction == "yes") ~ femininity * zpressure,
                  "wind" %when% (main_interaction == "yes") ~ femininity * zwind,
                  "category" %when% (main_interaction == "yes") ~ femininity * zcat,
                  "all" %when% (main_interaction == "yes") ~ femininity * z3,
                  "all_no_interaction" %when% (main_interaction == "no") ~ z3
              ) + branch(covariates, "1" ~ NULL, "2" ~ year:damage, "3" ~ post:damage), 
              family = branch(model, "linear" ~ "gaussian", "poisson" ~ "poisson"),  
              data = df)

    broom::tidy(fit)
    ```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -1.05      0.345    -3.05   3.03e- 3
2 femininity   -0.0143    0.215    -0.0666 9.47e- 1
3 damage        0.414     0.0414   10.0    3.25e-16
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.06      0.348     -3.04    0.00313
2 femininity  -0.00159   0.225     -0.00710 0.994  
3 damage       0.135     1.36       0.0996  0.921  
4 damage:year  0.000140  0.000684   0.205   0.838  
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.05       0.349     -3.00  3.48e- 3
2 femininity  -0.0269     0.228     -0.118 9.06e- 1
3 damage       0.417      0.0450     9.27  1.13e-14
4 damage:post -0.00469    0.0271    -0.173 8.63e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.449    -2.32   2.26e- 2
2 femininity  -0.0140     0.216    -0.0648 9.49e- 1
3 damage       0.413      0.0565    7.31   1.18e-10
4 z3           0.00559    0.152     0.0367 9.71e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -1.04      0.452     -2.30     0.0240
2 femininity  -0.000625  0.227     -0.00276  0.998 
3 damage       0.123     1.38       0.0893   0.929 
4 z3           0.00936   0.154      0.0606   0.952 
5 damage:year  0.000145  0.000693   0.210    0.834 
```

``` r
fit = glm(log(death + 1) ~ femininity + damage + z3 + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -1.04       0.452    -2.30   2.37e- 2
2 femininity  -0.0266     0.230    -0.116  9.08e- 1
3 damage       0.416      0.0601    6.92   7.20e-10
4 z3           0.00387    0.154     0.0252 9.80e- 1
5 damage:post -0.00465    0.0273   -0.170  8.65e- 1
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic     p.value
  <chr>                <dbl>     <dbl>     <dbl>       <dbl>
1 (Intercept)        -0.637     0.505      -1.26 0.211      
2 femininity         -0.719     0.660      -1.09 0.279      
3 damage              0.356     0.0660      5.40 0.000000563
4 femininity:damage   0.0956    0.0847      1.13 0.262      
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + year:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.627     0.508      -1.23     0.221
2 femininity        -0.723     0.664      -1.09     0.279
3 damage            -0.106     1.37       -0.0772   0.939
4 femininity:damage  0.0990    0.0857      1.15     0.251
5 damage:year        0.000232  0.000687    0.337    0.737
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + NULL + post:damage, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)       -0.637      0.508    -1.25   0.213     
2 femininity        -0.719      0.664    -1.08   0.282     
3 damage             0.357      0.0703    5.08   0.00000210
4 femininity:damage  0.0952     0.0858    1.11   0.270     
5 damage:post       -0.00118    0.0272   -0.0434 0.965     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + NULL,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -0.760      0.826   -0.920  0.360   
2 femininity            -0.0619     1.00    -0.0618 0.951   
3 damage                 0.373      0.109    3.41   0.000994
4 zpressure             -0.0553     0.293   -0.189  0.851   
5 femininity:damage      0.0102     0.131    0.0777 0.938   
6 femininity:zpressure   0.310      0.343    0.905  0.368   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic p.value
  <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.756     0.831      -0.910    0.365
2 femininity           -0.0641    1.01       -0.0636   0.949
3 damage               -0.0323    1.38       -0.0235   0.981
4 zpressure            -0.0573    0.294      -0.195    0.846
5 femininity:damage     0.0129    0.132       0.0982   0.922
6 femininity:zpressure  0.311     0.345       0.901    0.370
7 damage:year           0.000203  0.000688    0.295    0.768
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zpressure + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic p.value
  <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)          -0.759      0.831    -0.913  0.364  
2 femininity           -0.0620     1.01     -0.0615 0.951  
3 damage                0.375      0.112     3.35   0.00119
4 zpressure            -0.0541     0.294    -0.184  0.855  
5 femininity:damage     0.00903    0.132     0.0685 0.946  
6 femininity:zpressure  0.310      0.345     0.900  0.371  
7 damage:post          -0.00315    0.0273   -0.115  0.908  
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.580     0.648     -0.895 0.373   
2 femininity         -0.845     0.801     -1.06  0.294   
3 damage              0.349     0.0861     4.05  0.000112
4 zwind               0.0282    0.198      0.143 0.887   
5 femininity:damage   0.112     0.104      1.08  0.285   
6 femininity:zwind   -0.0822    0.248     -0.331 0.742   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.564     0.653      -0.865    0.390
2 femininity        -0.853     0.805      -1.06     0.292
3 damage            -0.115     1.39       -0.0828   0.934
4 zwind              0.0312    0.199       0.157    0.875
5 femininity:damage  0.116     0.106       1.10     0.274
6 femininity:zwind  -0.0843    0.250      -0.337    0.737
7 damage:year        0.000232  0.000696    0.333    0.740
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zwind + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.581      0.652    -0.891  0.375   
2 femininity        -0.845      0.805    -1.05   0.297   
3 damage             0.350      0.0900    3.89   0.000199
4 zwind              0.0279     0.199     0.141  0.888   
5 femininity:damage  0.112      0.106     1.06   0.293   
6 femininity:zwind  -0.0823     0.250    -0.329  0.743   
7 damage:post       -0.00153    0.0275   -0.0554 0.956   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic    p.value
  <chr>                <dbl>     <dbl>     <dbl>      <dbl>
1 (Intercept)        -1.29      0.694     -1.86  0.0668    
2 femininity          0.106     0.867      0.123 0.903     
3 damage              0.444     0.0918     4.83  0.00000580
4 zcat               -0.304     0.222     -1.37  0.175     
5 femininity:damage  -0.0146    0.113     -0.129 0.897     
6 femininity:zcat     0.398     0.274      1.45  0.150     
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -1.25      0.706    -1.78      0.0789
2 femininity         0.106     0.871     0.122     0.903 
3 damage             0.00130   1.46      0.000889  0.999 
4 zcat              -0.293     0.227    -1.29      0.199 
5 femininity:damage -0.0118    0.114    -0.103     0.918 
6 femininity:zcat    0.401     0.276     1.45      0.150 
7 damage:year        0.000220  0.000726  0.303     0.762 
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * zcat + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic   p.value
  <chr>                <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)       -1.29       0.700    -1.84   0.0686   
2 femininity         0.105      0.872     0.121  0.904    
3 damage             0.445      0.0969    4.60   0.0000149
4 zcat              -0.305      0.225    -1.36   0.178    
5 femininity:damage -0.0150     0.114    -0.132  0.895    
6 femininity:zcat    0.397      0.276     1.44   0.154    
7 damage:post       -0.00142    0.0278   -0.0509 0.960    
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + NULL, family = "gaussian",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -0.945      0.769    -1.23  0.222   
2 femininity         -0.228      0.938    -0.243 0.808   
3 damage              0.398      0.102     3.89  0.000194
4 z3                 -0.145      0.270    -0.536 0.593   
5 femininity:damage   0.0304     0.123     0.247 0.805   
6 femininity:z3       0.249      0.328     0.760 0.449   
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + year:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic p.value
  <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept)       -0.919     0.776      -1.18     0.240
2 femininity        -0.236     0.943      -0.250    0.803
3 damage            -0.0993    1.40       -0.0707   0.944
4 z3                -0.137     0.272      -0.504    0.616
5 femininity:damage  0.0345    0.124       0.278    0.782
6 femininity:z3      0.249     0.330       0.755    0.453
7 damage:year        0.000248  0.000699    0.355    0.724
```

``` r
fit = glm(log(death + 1) ~ femininity * damage + femininity * z3 + post:damage,
    family = "gaussian", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -0.946       0.774    -1.22   0.225   
2 femininity        -0.229       0.944    -0.242  0.809   
3 damage             0.398       0.106     3.76   0.000313
4 z3                -0.145       0.272    -0.533  0.595   
5 femininity:damage  0.0302      0.124     0.244  0.808   
6 femininity:z3      0.249       0.330     0.755  0.452   
7 damage:post       -0.000558    0.0275   -0.0203 0.984   
```

``` r
fit = glm(death ~ femininity + damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 3 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)   -3.53     0.187     -18.9  7.16e-80
2 femininity     0.475    0.0552      8.61 7.53e-18
3 damage         0.695    0.0181     38.4  0       
```

``` r
fit = glm(death ~ femininity + damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.50      0.186       -18.8  7.91e-79
2 femininity   0.406     0.0586        6.93 4.26e-12
3 damage       1.56      0.232         6.72 1.84e-11
4 damage:year -0.000433  0.000116     -3.73 1.95e- 4
```

``` r
fit = glm(death ~ femininity + damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.42     0.186      -18.4  1.79e-75
2 femininity    0.279    0.0605       4.61 4.04e- 6
3 damage        0.720    0.0184      39.2  0       
4 damage:post  -0.0423   0.00510     -8.28 1.21e-16
```

``` r
fit = glm(death ~ femininity + damage + z3 + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term        estimate std.error statistic   p.value
  <chr>          <dbl>     <dbl>     <dbl>     <dbl>
1 (Intercept)  -3.64      0.190     -19.1  1.07e- 81
2 femininity    0.442     0.0568      7.78 7.06e- 15
3 damage        0.714     0.0194     36.9  2.38e-297
4 z3           -0.0777    0.0299     -2.60 9.37e-  3
```

``` r
fit = glm(death ~ femininity + damage + z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term         estimate std.error statistic  p.value
  <chr>           <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept) -3.62      0.190       -19.1  3.58e-81
2 femininity   0.367     0.0601        6.11 1.02e- 9
3 damage       1.61      0.231         6.96 3.48e-12
4 z3          -0.0822    0.0293       -2.81 4.99e- 3
5 damage:year -0.000448  0.000116     -3.87 1.09e- 4
```

``` r
fit = glm(death ~ femininity + damage + z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term        estimate std.error statistic  p.value
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)  -3.53     0.189      -18.7  1.26e-77
2 femininity    0.246    0.0616       3.99 6.58e- 5
3 damage        0.739    0.0195      38.0  0       
4 z3           -0.0780   0.0286      -2.73 6.31e- 3
5 damage:post  -0.0424   0.00510     -8.32 8.54e-17
```

``` r
fit = glm(death ~ femininity * damage + NULL + NULL, family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 4 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)         -1.73     0.294      -5.90 3.61e- 9
2 femininity          -2.08     0.368      -5.65 1.61e- 8
3 damage               0.511    0.0303     16.9  9.52e-64
4 femininity:damage    0.261    0.0376      6.94 3.88e-12
```

``` r
fit = glm(death ~ femininity * damage + NULL + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -1.81      0.296        -6.09 1.12e- 9
2 femininity        -2.00      0.371        -5.41 6.38e- 8
3 damage             1.19      0.233         5.13 2.91e- 7
4 femininity:damage  0.248     0.0381        6.52 7.20e-11
5 damage:year       -0.000338  0.000114     -2.96 3.09e- 3
```

``` r
fit = glm(death ~ femininity * damage + NULL + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 5 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.94     0.304       -6.39 1.62e-10
2 femininity         -1.80     0.377       -4.79 1.67e- 6
3 damage              0.564    0.0321      17.6  4.58e-69
4 femininity:damage   0.216    0.0389       5.56 2.76e- 8
5 damage:post        -0.0372   0.00511     -7.27 3.51e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.52       0.527     -6.67  2.56e-11
2 femininity           -0.0905     0.576     -0.157 8.75e- 1
3 damage                0.735      0.0617    11.9   1.04e-32
4 zpressure            -0.502      0.116     -4.31  1.62e- 5
5 femininity:damage     0.00990    0.0662     0.149 8.81e- 1
6 femininity:zpressure  0.598      0.120      4.97  6.81e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + year:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                  estimate std.error statistic  p.value
  <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)          -3.78      0.542       -6.97  3.12e-12
2 femininity            0.181     0.590        0.307 7.59e- 1
3 damage                1.59      0.248        6.44  1.22e-10
4 zpressure            -0.542     0.118       -4.61  4.08e- 6
5 femininity:damage    -0.0283    0.0681      -0.416 6.78e- 1
6 femininity:zpressure  0.642     0.121        5.29  1.22e- 7
7 damage:year          -0.000416  0.000116    -3.59  3.30e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * zpressure + post:damage,
    family = "poisson", data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term                 estimate std.error statistic  p.value
  <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)           -3.81     0.547      -6.98  2.95e-12
2 femininity             0.268    0.594       0.451 6.52e- 1
3 damage                 0.799    0.0644     12.4   2.30e-35
4 zpressure             -0.520    0.119      -4.36  1.31e- 5
5 femininity:damage     -0.0452   0.0685     -0.660 5.10e- 1
6 femininity:zpressure   0.613    0.123       4.99  5.91e- 7
7 damage:post           -0.0376   0.00512    -7.34  2.16e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.66      0.346     -4.80  1.55e- 6
2 femininity         -2.15      0.410     -5.25  1.53e- 7
3 damage              0.502     0.0390    12.9   8.06e-38
4 zwind               0.0285    0.0730     0.390 6.97e- 1
5 femininity:damage   0.272     0.0448     6.06  1.34e- 9
6 femininity:zwind   -0.0679    0.0794    -0.855 3.93e- 1
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic      p.value
  <chr>                 <dbl>     <dbl>     <dbl>        <dbl>
1 (Intercept)       -1.75      0.349       -5.01  0.000000555 
2 femininity        -2.06      0.413       -4.99  0.000000604 
3 damage             1.17      0.235        4.97  0.000000671 
4 zwind              0.0219    0.0733       0.299 0.765       
5 femininity:damage  0.257     0.0454       5.66  0.0000000155
6 femininity:zwind  -0.0560    0.0798      -0.701 0.483       
7 damage:year       -0.000329  0.000115    -2.88  0.00403     
```

``` r
fit = glm(death ~ femininity * damage + femininity * zwind + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -1.85     0.352      -5.26  1.45e- 7
2 femininity         -1.89     0.416      -4.56  5.20e- 6
3 damage              0.552    0.0401     13.7   5.36e-43
4 zwind               0.0364   0.0738      0.493 6.22e- 1
5 femininity:damage   0.229    0.0457      5.01  5.31e- 7
6 femininity:zwind   -0.0683   0.0802     -0.852 3.94e- 1
7 damage:post        -0.0370   0.00512    -7.24  4.46e-13
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.38      0.385     -8.78  1.71e-18
2 femininity         -0.443     0.445     -0.994 3.20e- 1
3 damage              0.711     0.0415    17.1   6.93e-66
4 zcat               -0.428     0.0556    -7.69  1.45e-14
5 femininity:damage   0.0621    0.0473     1.31  1.89e- 1
6 femininity:zcat     0.423     0.0616     6.87  6.37e-12
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.65      0.395       -9.26  2.03e-20
2 femininity        -0.192     0.453       -0.425 6.71e- 1
3 damage             1.81      0.252        7.18  6.92e-13
4 zcat              -0.466     0.0560      -8.32  9.07e-17
5 femininity:damage  0.0260    0.0485       0.536 5.92e- 1
6 femininity:zcat    0.430     0.0612       7.02  2.16e-12
7 damage:year       -0.000536  0.000121    -4.43  9.48e- 6
```

``` r
fit = glm(death ~ femininity * damage + femininity * zcat + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.74      0.395     -9.48   2.63e-21
2 femininity        -0.0549    0.453     -0.121  9.04e- 1
3 damage             0.788     0.0433    18.2    6.42e-74
4 zcat              -0.468     0.0564    -8.29   1.09e-16
5 femininity:damage  0.00178   0.0485     0.0368 9.71e- 1
6 femininity:zcat    0.417     0.0616     6.78   1.21e-11
7 damage:post       -0.0430    0.00529   -8.12   4.61e-16
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + NULL, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 6 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)        -3.25      0.439      -7.40 1.34e-13
2 femininity         -0.545     0.493      -1.11 2.69e- 1
3 damage              0.701     0.0497     14.1  4.09e-45
4 z3                 -0.440     0.0867     -5.08 3.82e- 7
5 femininity:damage   0.0682    0.0547      1.25 2.13e- 1
6 femininity:z3       0.460     0.0926      4.97 6.70e- 7
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + year:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term               estimate std.error statistic  p.value
  <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.49      0.449       -7.77  7.87e-15
2 femininity        -0.306     0.502       -0.610 5.42e- 1
3 damage             1.56      0.245        6.36  2.08e-10
4 z3                -0.480     0.0874      -5.49  4.05e- 8
5 femininity:damage  0.0340    0.0561       0.605 5.45e- 1
6 femininity:z3      0.493     0.0929       5.31  1.11e- 7
7 damage:year       -0.000416  0.000116    -3.58  3.49e- 4
```

``` r
fit = glm(death ~ femininity * damage + femininity * z3 + post:damage, family = "poisson",
    data = df.filtered)
broom::tidy(fit)
# A tibble: 7 × 5
  term              estimate std.error statistic  p.value
  <chr>                <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
2 femininity        -0.146     0.504      -0.290 7.71e- 1
3 damage             0.772     0.0517     14.9   1.97e-50
4 z3                -0.481     0.0886     -5.43  5.69e- 8
5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

Even though the variable `fit` was defined in a multiverse code block,
since the default analysis is executed in the active R environment, the
version corresponding to the default analysis can be accessed directly
in R:

``` r
broom::tidy(fit)
#> # A tibble: 7 × 5
#>   term              estimate std.error statistic  p.value
#>   <chr>                <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)       -3.59      0.451      -7.97  1.53e-15
#> 2 femininity        -0.146     0.504      -0.290 7.71e- 1
#> 3 damage             0.772     0.0517     14.9   1.97e-50
#> 4 z3                -0.481     0.0886     -5.43  5.69e- 8
#> 5 femininity:damage  0.00719   0.0563      0.128 8.98e- 1
#> 6 femininity:z3      0.484     0.0938      5.17  2.38e- 7
#> 7 damage:post       -0.0386    0.00515    -7.48  7.27e-14
```

Analysts can change which analysis path is executed by default. Inline
code output of a single analysis path (and the ability to select that
path) is meant to support the familiar trial and error workflow of data
analysts and to aid debugging.

## Executing the entire multiverse

To execute all unique analysis paths in the multiverse, an analyst can
call `execute_multiverse(M)` . We support local parallelization with an
optional cores argument indicating the number of cpu cores to use. The
multiverse object can also be easily adapted to use with existing
parallel computing packages in R, such as
[future](https://cran.r-project.org/web/packages/future/index.html), to
run analyses across computing clusters.

## Debugging and Diagnosing Errors

For the default analysis, as it executes in the R environment, users
have access to the same set of debugging utilities that R provides. When
the user executes the entire multiverse, the library outputs the error
message, a traceback—an object containing the entire call stack that
caused the error—and the index of the corresponding analysis path in
which the error was encountered. The execution of the remaining analysis
paths in the multiverse are not halted if any errors are encountered.
The traceback is helpful to identify the location of the error, as often
R expressions return unidentifiable error messages.
`execute_universe(<universe ID>)` (universe ID are found in the table
output by `expand()`, see below) allows analysts to execute a particular
analysis path and reproduce errors encountered in the execution of that
specific path.

![expand() provides an overview of the complete decision tree of the
specified multiverse, with each row corresponding to the set of
decisions creating the a particular analysis
path](man/figures/06-expand.png)

## Conclusion

In this document, we covered details to help you quickly get started
with this package. Please refer to the following vignettes for further
information on:

-   How `multiverse` can be used in different environments such as in
    RMarkdown and RScripts. See `vignette("multiverse-in-rmd")`.
-   How alternate analysis paths are declared in `multiverse` using
    branch, and how `multiverse` processes the user-declared
    (`multiverse` DSL) code to create multiple end-to-end executable R
    analysis code. See `vignette("branch")`.
-   How conditions and dependencies can be declared in `multiverse`;
    conditions can be used to state when two steps in a multiverse are
    incompatible with one another. See `vignette("conditions")`.
-   How multiverse results can be extracted and visualised. . See
    `vignette("visualising-multiverse")`.

We also implement a series of other end-to-end multiverse
implementations using this package to demonstrate how it might be used:

-   Simonsohn et al.’s [specification curve
    analysis](http://dx.doi.org/10.2139/ssrn.2694998) of the paper
    [Female hurricanes are deadlier than male
    hurricanes](https://doi.org/10.1073/pnas.1402786111)
-   Dragicevic et al.’s mini-paper [Adding Inferential Information to
    plots using Resampling and
    Animations](https://explorablemultiverse.github.io/examples/dance/)
    (from [Increasing the transparency of research papers with
    explorable multiverse
    analyses](https://hal.inria.fr/hal-01976951/document) )
-   Dragicevic et al.’s mini-paper [Re-evaluating the efficiency of
    Physical
    Visualisations](https://explorablemultiverse.github.io/examples/frequentist/)
    (from [Increasing the transparency of research papers with
    explorable multiverse
    analyses](https://hal.inria.fr/hal-01976951/document) )
