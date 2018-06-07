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
