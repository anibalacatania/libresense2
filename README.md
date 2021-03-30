
# LibreSense

Sensory analysis application.

This is the first attempt at building open-source software for capturing sensory
data. In this first step, we are trying to build an application for
Quantitative Descriptive Analysis (QDA).

We presented this project at LatinR 2018 in Buenos Aires, Argentina.

<http://47jaiio.sadio.org.ar/sites/default/files/LatinR_9.pdf> \[ES\]

<https://www.youtube.com/watch?v=DqrVVRA7riA&feature=youtu.be> \[ES\]

LibreSense contains two applications:

-   The application for capturing panelist data -`run_panel`- the leader
    of the sensory panel can modify the name and number of the samples
    and also the descriptors.
-   The results visualization application -`run_board`-.

In case you want to collaborate, check the issues.

## Installation

You can install the development version of `{libresense}` from
[GitHub](https://github.com/) with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github(
  "anibalacatania/LibreSense", subdir = "libresense", dependencies = TRUE
)
```

## Example

### Setting the Attributes to Evaluate

To set which attributes -and their type- to evaluate by the panelists,
this should be performed in a `csv` file, as exemplified in
[atributos.csv](atributos.csv).

    #>                Nombre Valores
    #> 1 Intensidad de color Numeric
    #> 2              Aromas    Text
    #> 3             Sabores    Text
    #> 4              Frutal   Check
    #> 5              Floral   Check
    #> 6            Herbáceo   Check

This `csv` file should contain two columns:

-   Nombre: The names of the attribute to be evaluated.
-   Valores: The types of the attribute to be evaluated, must be one of
    `{"Numeric", "Check", "Text"}`

### Setting the Products to Evaluate

To set which products to evaluate by the panelists, this should be
performed in a `csv` file, as exemplified in
[productos.csv](productos.csv).

    #>       Copa
    #> 1 Herencia
    #> 2      Uno
    #> 3  Eredita
    #> 4    Nieto

This `csv` file should contain at least one column, the name of the
first column will be the name of the product to be evaluated, for
example `"Copa"`. The values of this first column, will be the products
to be evaluated. Additional columns will not be used by `{libresense}`,
but might be useful for the panel leader.

### Running the App

#### Running the Panel

Let’s assume we are using the two files presented above (download them
in the same folder as running the R session), to run the panel app, from
an R console type:

``` r
libresense::run_panel(
  products_file = "productos.csv",
  attributes_file = "atributos.csv",
  answers_dir = "Answers/"
)
```

##### Using an Experimental Design

LibreSense allows an experimental design for the panel. This design file
must contain a column named `Muestra` which will hold the order in which
each product should be evaluated by each panelist, i.e., the first rows
will be for the first panelist, and so on. For example, if we want to
create a Williams Latin Square design for our `"productos.csv"` file we
could run:

``` r
library("crossdes")
library("dplyr")
library("readr")

wls_design <- read_csv(                            # Read the products file.
  "productos.csv", col_types = cols()
) %>%
  nrow() %>%                                       # Get how many products to evaluate.
  williams()                                       # Create the Williams Latin Square design.
data.frame(Muestra = as.vector(t(wls_design))) %>% # Reformat it as a one-column data.frame.
  write_csv("diseno.csv")                          # Save it as a csv file.
read_csv("diseno.csv", col_types = cols())         # Print the design.
#> # A tibble: 16 x 1
#>    Muestra
#>      <dbl>
#>  1       1
#>  2       2
#>  3       4
#>  4       3
#>  5       2
#>  6       3
#>  7       1
#>  8       4
#>  9       3
#> 10       4
#> 11       2
#> 12       1
#> 13       4
#> 14       1
#> 15       3
#> 16       2
```

And finally, use this design for the panel:

``` r
libresense::run_panel(
  products_file = "productos.csv",
  attributes_file = "atributos.csv",
  answers_dir = "Answers/",
  design_file = "diseno.csv"
)
```

Once the panel evaluations have finished, in the `answers_dir` folder,
there will be a file named `"diseno.csv"` which will contain the order
in which each product was evaluated by each panelist.

#### Running the Board

Let’s assume we are using the two files presented above (download them
in the same folder as running the R session), to run the board app, from
an R console type:

``` r
libresense::run_board(answers_dir = "Answers/")
```
