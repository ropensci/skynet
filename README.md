
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skynet

![Build
Status](https://travis-ci.org/FilipeamTeixeira/skynet.svg?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/skynet)](https://cran.r-project.org/package=skynet)
![](https://cranlogs.r-pkg.org/badges/grand-total/skynet?color=brightgreen)

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

## Installation

You can install skynet from github with:

``` r
# install.packages("devtools")
devtools::install_github("FilipeamTeixeira/skynet")
```

## Example

To generate a directed network, please type:

    library(skynet)
    # For DB1B data
    import_db1b("folder/Coupon_2011Q1.csv", "folder/Ticket_2011Q1.csv")
    make.netDir(OD_2011Q1, disp = TRUE, alpha = 0.05)
    
    # For T100 data
    import_t100("folder/T100_2011.csv")
    make.netDir(T100_2011Q1, disp = TRUE, alpha = 0.05)
