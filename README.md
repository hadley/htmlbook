# htmlbook

<!-- badges: start -->

<!-- badges: end -->

htmlbook converts Quarto books to O'Reilly's [HTMLBook](http://oreillymedia.github.io/HTMLBook/) format.
It's far from perfect but was successfully on [R4DS](https://r4ds.hadley.nz) and [R packages](https://r-pkgs.org).

## Installation

You can install the development version of htmlbook from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hadley/htmlbook")
```

## Usage

First render your quarto book, and then call:

``` r
htmlbook::convert_book()
```

This will create a new `oreilly` directory containing the files you need to upload to [Atlas](https://atlas.oreilly.com).
You'll also need to manually update your `atlas.json` (either by hand or with Configure \| Files) to reflect the correct chapter ordering.

## Supported features

-   Overall structure including parts and appendices.
-   Figures and code blocks.
-   Cross-references.
-   Call outs.
-   Footnotes.
