# prism2R: Read GraphPad Prism Files Into R
## Brief background

The prism2R package is heavily influenced by Yue Jiang's package called [pzfx](https://cran.r-project.org/web/packages/pzfx/ "pzfx package"). I imagine if you are looking at this package you would have most probably used his before. The main (only) reason for a new package is that with Version 10 GraphPad introduced a new file type called `.prism` that is completely different and incompatible with how the old XML-based `.pzfx` file type was parsed.

`.prism` is essentially a zip archive with a quite complicated folder structure that separates the contents into analyses, data, graphs, info, layouts, misc and a document.json that contains the necessary information about how the various bits are connected together. Instead of an XML document type, most useful information is now contained in JSON files, whereas the actual data tables are conveniently now stored in `.csv`s.

I had intended to only write a parser for the `.prism` file type given that [pzfx](https://cran.r-project.org/web/packages/pzfx/ "pzfx package") already does a wonderful job of reading in (*and writing out to*) `.pzfx` files but I made some specific changes to how the data is read into R, most notably that it reads the data tables in as a [tibble](https://tibble.tidyverse.org/ "tibble package") (mainly for the way tibbles print nicely to the console), and so I wrote this for the `.pzfx` files as well.

## Installation

The best way is to use the [devtools](https://devtools.r-lib.org/ "devtools package") package and install from github. It seems to understand the dependencies and build requirements. There have been issues reported with other, more lightweight package managers like `remotes` or `pak`.

```{r, eval=FALSE}
devtools::install_github("biomiha/prism2R", build_vignettes = TRUE)
library(prism2R)
```

## How it works

There are currently two main user-visible functions, the `get_prism_tables` and the `read_prism` function. Both take either a `.prism` or a `.pzfx` file as input, work out how to parse the data based on the file extension and then return either a vector of table names in the case of `get_prism_tables` or the actual data in the case of the `read_prism` function.

```{r}
demo_file <- system.file("extdata", "demo_dataset.prism", package = "prism2R")
get_prism_tables(demo_file)
```

The `read_prism` function also takes a `sheet` argument. This can either be an integer (the index of the sheet or table that you want to read in), a character string (the name of the data table that you want to read in) or `NA` (the default), in which case all tables in the file will be read in. If the value of the `sheet` argument is provided, it needs to only be lenght one, i.e. you can only read in one or all data tables.

*One note of caution with tables specified as character strings - if there are multiple tables with the same name (fthat is allowed in Prism, or some reason), then you will get an error message reminding you that multiple sheets have been selected.*

There are quite easy ways to read in multiple (but not all) data sheets into a list, that do not to be handled separately in the package function (see below).

```{r}
# Sheet provided as integer
read_prism(demo_file, sheet = 1)
```

```{r}
# Sheet provided as character string
read_prism(demo_file, sheet = "XY: Entering replicate data")
```

```{r}
# Sheet provided as NA (default) - reads all the tables
read_prism(demo_file, sheet = NA)
```

&nbsp;


### In case you want to read in multiple tables but not all

In case you want to read in multiple tables but do not want to read in all of them, let's say there are many in the file but you only want to look at 2, you could just use `lapply` or `purrr::map` (if you prefer that) and specify the sheet to read in.

```{r}
# I want to read in sheets 1 and 3
selected_sheets <- c(1, 3)
my_list <- lapply(selected_sheets, FUN = function(x) read_prism(prism_file = demo_file, sheet = x))
names(my_list) <- get_table_names(demo_file)[selected_sheets]
my_list
```

### Get the saved analysis parameters
The newer `.prism` file type allows access to all the analysis parameters, be that for linear or non-linear regression, or other analysis types. Analysis data tables in Prism usually include multiple (quite often hidden) tabs, that are however parseable. By design the `get_analysis_tables` function will return only the main (in Prism called the preferred tab) that contains the model parameters and coefficients. Other model data is accessible in raw format using the `archive` package as explained below. 

```{r}
# Get analysis tables from a Prism file (if they exist)
get_analysis_tables(demo_file)
```


## Some additional notes

### Writing out to Prism files

Personally, I don't think the package needs a function to write dataframes to Prism files. If, for whatever reason, that needs to be done, then my favourite way of doing that is to export the data into a .csv file and use Graphpad's import function to import the file into Prism directly. That way you can also choose what type of data table most suitably represents your data.

In addition, the [pzfx](https://cran.r-project.org/web/packages/pzfx/ "pzfx package") package includes a `write_pzfx` function that can output to a .pzfx file. I think replicating that to export to a .prism would currently be more hassle than it is worth.

### Parsing other metadata contained in Prism files

As outlined above the new `.prism` format does actually allow parsing of other analysis data, not just the model parameters tables, but also e.g. line fit values that are plotted in the Graphs section, as well as other parts of the analysis workflow. 
Unfortunately, like most of this information in the old `.pzfx` files, the graphs even in a `.prism` file are still stored in a proprietary binary format and therefore inaccessible. That being said, some aspects like lines from a curve fitting analysis are available, albeit buried in internal `.csv` files. I provide an outline how this can be accessed below. Arguably the output is very messy but could perhaps be of use to some people. To access all internal data in raw format you can try:

```{r}
files_in_archive <- archive::archive(demo_file)
files_in_archive <- tibble::rowid_to_column(files_in_archive) # will add rowids, which are required later

csv_files <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = "data.csv", x = path))

# Output all .csv files (including the data tables that can be parsed) in raw format in a list with no names
all_data_raw <- lapply(csv_files$rowid, function(x) {
  read_csv(file = archive::archive_read(archive = demo_file, file = x), col_names = FALSE)
})
head(all_data_raw, n = 3) # First 3 as an example
```
Notice that some data tables contain exactly 1000 rows. These are the curve fit values, although the correct table headers will be missing and will need to be assigned manually. 

### And finally...

There are some internal data transformations with the new format that you wouldn't expect that happen behind the scenes in Prism itself, e.g. data tables of the type Y_CV (with coefficients of variation) are actually represented as percentages rather than raw values and Prism somehow transforms them to numbers before showing them to you as the user in a table. I have had to do replicate that during the parsing process. Different table types require different transformations (most actually don't require any at all) and I've tried to cover all different data types but if you find a bug, please drop me a message on [the package github issues page](https://github.com/Biomiha/prism2R/issues "github issues").
