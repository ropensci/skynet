
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skynet <img src="man/figures/logo.png" align="right" />

[![Build
Status](https://travis-ci.org/ropensci/skynet.svg?branch=master)](https://travis-ci.org/ropensci/skynet)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/skynet)](https://cran.r-project.org/package=skynet)
![](https://cranlogs.r-pkg.org/badges/skynet?color=brightgreen)
[![Coverage
status](https://codecov.io/gh/ropensci/skynet/branch/master/graph/badge.svg)](https://codecov.io/github/ropensci/Skynet?branch=master)
[![](https://badges.ropensci.org/214_status.svg)](https://github.com/ropensci/onboarding/issues/214)

# Overview

The rationale behind Skynet, is to provide researchers with a unifying
tool overcoming some of the challenges faced when dealing with the
Bureau of Transport Statistics, DB1B and T100 data. The DB1B data
consists of 2 sets of files, Coupon and Ticket. They can be both
downloaded at
<https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=289> and
<https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=272>
respectively while the T100 data can be found here
<https://www.transtats.bts.gov/Tables.asp?DB_ID=111>.

## Note

To comply with R syntax guidelines, we changed to a clearer function
naming from version 1.2.0. Deprecated functions are still present, but
will be removed for the next versions.

## Note on importing from other data sources

We are constantly working on new functions that allow importing data
from different data sources. However, as we can’t cover them all at
least for now, in case you would like to work with a database which is
not covered by skynet, simply create a data.frame with the following
variables:

`itin_id, mkt_id, seq_num, origin_mkt_id, origin, year, quarter,
dest_mkt_id, dest, trip_break, op_carrier, distance, gateway, roundtrip,
itin_yield, passengers, itin_fare, bulk_fare, distance_full`

For more information on the variables, please visit
<https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=289> and
<https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=272>.

Skynet allows that some of this variables have a 0 or NA value, however,
if you’re working with a specific dataset which doesn’t allow an easy
conversion to our format, please feel free to create an issue so we can
look into it. Please make sure to include at least one small example of
a csv file with the data you’re trying to import.

## Installation

You can install skynet from github with:

``` r
# install.packages("devtools")
devtools::install_github("FilipeamTeixeira/skynet")
```

## Import Data

To import data, simply type `import_db1b()` or `import_t100()` including
the path to your desired file.  
**Note**: The Coupon file should take the first argument while the
Ticket file should take the second argument.

``` r
 library(skynet)
 import_db1b("folder/Coupon 2016Q1.csv", "folder/Ticket 2016Q1.csv")
 import_t100("folder/T100_2016.csv")
```

The BTS DB1B data consists of 2 sets of files, `Coupon` and `Ticket`.
They can be both downloaded at
<https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=289> and
<https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=272>
respectively.

Despite being possible to download the complete zipped file, which
includes all variables, due to its size, we recommend selecting the
following set.

| Coupon                     | Ticket             |
| :------------------------- | :----------------- |
| Itinerary ID               | Itinerary ID       |
| Market ID                  | Roundtrip          |
| Sequence Number            | Itinerary Yield    |
| Origin City Market ID      | Passengers         |
| Origin                     | Itinerary Fare     |
| Year                       | Bulkfare Indicator |
| Quarter                    | Distance           |
| Destination City Market ID |                    |
| Destination                |                    |
| Trip Break                 |                    |
| Operating Carrier          |                    |
| Distance                   |                    |
| Gateway                    |                    |

Since version 1.0.2 that the import method changed being the
`netimport()` function no longer available. When importing from the
prezipped DB1B file, just add the argument `zip = TRUE` to the
`import_db1b()` function. This does not apply to the T100 file which can
be simply imported by typing `import_t100()`. In order to save space, it
is possible as well to import the prezipped file, and convert it to a
smaller file with only the necessary variables, with the function
`convert_raw()`.

## Example

To generate a directed network, please type:

    library(skynet)
    # For DB1B data
    import_db1b("folder/Coupon_2011Q1.csv", "folder/Ticket_2011Q1.csv")
    make_net_dir(OD_2011Q1, disp = TRUE, alpha = 0.05)
    
    # For T100 data
    import_t100("folder/T100_2011.csv")
    make_net_dir(T100_2011Q1, disp = TRUE, alpha = 0.05)

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
