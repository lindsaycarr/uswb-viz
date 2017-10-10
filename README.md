# US-Waterbudgets

Code for a US water budget data visualization.

Initial Setup:

Clone this repository.

Open the included `uswb-viz.Rproj`

If you don't have it, install [`vizlab`](https://github.com/USGS-VIZLAB/vizlab)

Check to make sure you have required packages listed in `viz.yaml` installed.  

Note: To get the `svglite` package, you will need to install it from GitHub using `devtools::install_github("jread-usgs/svglite@svgnano")`.

do:
```r
library(vizlab)
createProfile()
createMakefiles()
```

then make sure you have the proper packages and versions installed with:
```r
vizlab::checkVizPackages()
```
and use `install.packages()` or `devtools::install_github()` to update

`createProfile()` may not be necessary if you've worked on vizlab before.

Go to the "build" tab in Rstudio. You should see a "Build All" option. Click it!

If there are errors, logs with errors are in `.hurricane-irma/vizlab/make/log`

Once you are able to complete the build, open `target/index.html` to see the finished product locally.
