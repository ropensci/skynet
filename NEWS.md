# skynet 1.6.0

* `net_map()` draws great circle routes on a projected map (Albers for North America, Equal Earth otherwise; needs the suggested sf package), with map limits taken from the plotted airports
* `net_map()` shows a carrier legend, groups smaller carriers as "Other" (`carriers` argument) and sizes airports by traffic
* `net_map(interactive = TRUE)` returns a zoomable leaflet map with route and airport tooltips
* `net_map()` now works with metro networks

# skynet 0.9.0

* Added a `NEWS.md` file to track changes to the package.

# skynet 0.9.2

* Added tests
* Added Sample Data
* Fixed Imports on Description file

# skynet 0.9.3

* Fixed "no visible binding for global variable" issue
* Replaced disparity filter from semnet package by own package
* Small performace improvements
* Corrected spelling

# skynet 0.9.4

* Added T-100 import
* Corrected issue with general import for international option

# skynet 0.9.7

* Added new map function, now automatically printing different carriers with different colors.
* Improved import functions
* Importing from prezipped file, no longer requires extra function.

# skynet 0.9.7

* Changed way itin_fare was calculated for Directed, Undirected and Metro Networks. Now it uses price per mile and distance between stops to generate that info.

# skynet 0.9.9

* netImport now imports T100 market and segment files.
* netPath airlines renamed to carrier.
* updated vignettes.

# skynet 1.0

* netMetro has been replaced by argument in `netDir()` and `netUnd()`.

# skynet 1.0.1

* Possible to include carriers for undirected networks.
* Possible to filter non-scheduled flights.
* Ground Transport is now included as a carrier.
* Metro Network can be plotted.
* Improved way of calculating airport passenger frequency.
* Minor bug fixes.

# skynet 1.0.2

* New import functions. Now there are separate functions to import csv files from both DB1B and T100 databases.
* New bootnet function to bootstrap networks.

# skynet 1.0.3

* Minor adjustments
* Improved readability

# skynet 1.0.4

* Improved ReadMe file
* Fixed website
* Added extra comments and help information

# skynet 1.1.0

* Changed way files are imported. Now Coupon should take the first argument and Ticket the second.
* Minor adjustmenst to the help files.

# skynet 1.2.0

* Major function naming changes to match syntax etiquette
* Skynet S3 class added

# skynet 1.2.1

* Year and quarter added to skynet object
* Fixed site

# skynet 1.2.2

* Removed convert_raw as it is easier to import using the zip = TRUE argument and select the format to be saved to.

# skynet 1.2.3

* Now it is possible to import directly files from the BTS website with the download_db1b() function.

# skynet 1.3

* We have fixed some bugs and added the download_t100 function, making it finally possible to import both T100 and DB1B datasets without having to navigate to the BTS website.

# skynet 1.3.2

* We've added the possibility to import On-time performance data.

# skynet 1.3.6

* Added dplyr 1.0.0 compatibility

# skynet 1.3.7

* Improved test time

# skynet 1.3.9

* Fixed links
* Added download failed message

# skynet 1.4.1

* Fixed issue where download_t100 requires query to be encoded.

# skynet 1.4.2

* Skips test for download_t100 as BTS is transitioning to new download methodology.

# skynet 1.4.3

* Brings back download_t100.

# skynet 1.5.0

* Replaced deprecated `dplyr::funs()`/`mutate_all()` with `across()`.
* Replaced deprecated igraph functions (`graph.strength()`, `get.data.frame()`, `get.vertex.attribute()`, `subgraph.edges()`,
  `average.path.length()`, `delete.edges()`, `delete.vertices()`, `has.multiple()`, `degree.distribution()`).
* `net_map()` uses `linewidth` and `annotation_borders()` (ggplot2 >= 4.0.0).
* `download_t100()` no longer depends on RCurl; RCurl removed from Imports.
* Silenced `geom_smooth()` formula message in `fit_power()`.
* Requires R >= 4.1.0, dplyr >= 1.1.0, igraph >= 2.0.0, ggplot2 >= 4.0.0.

# skynet 1.5.1

* `make_net_path()` no longer multiplies passengers by the number of legs in multi-leg itineraries.
* `make_net_und(merge = FALSE)` now keeps parallel edges, and keeps `op_carrier` when `carrier = TRUE`.
* `import_t100()` respects `auto = FALSE` for segment files.
* `download_t100_int()` queries the international tables instead of the domestic ones.
* `net_map()` filters airport points by their own frequency.
