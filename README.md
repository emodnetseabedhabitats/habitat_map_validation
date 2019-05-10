## EMODnet Data Validation Tool
#### Alun Jones
#### 10th May 2019

<br>

This page gives an overview to the new EMODnet Data Validation Tool developed with Shiny and R, specifically looking at why features were added, and how the validation functions work, what they check for, and what they don't.

### Opening screen

1.  **Zip upload**

    The first step in using the app is to upload a zipped collection of shapefile documents. At present the upper limit for zip file size is 30MB, or twice the size of the DE000001 test package. Currently however the DE000001 test package takes approximately 20 minutes to run completely, so it would likely be worth assessing the possible maximum size of file uploaded and changing this setting appropriately to avoid excessive stress on the server.

2.  **DEF selection**

    This option was included based on the idea that anyone validating there data will likely know which DEF they're aiming to fulfill, and so functions can be tailored accordingly. During testing however there were problems when supplied DEF was obviously different to data DEF, so the app now detects DEF (as it did in Graeme's previous version), and notifies when input DEF is different to detected DEF. It may be worth considering removing this option entirely to avoid confusion.

3.  **Plotting type**

    I've experimented with a number of plotting options, and found that `plotly` allows for some visualisation options that I feel are particularly useful, namely the ability to hide layers, zoom, and highlight the locations of errors. This is admittedly slow with large files, which is why I've also included (as the default) a basic plotting option using `ggplot2`.

4.  **Validate button**

    This just prevents shiny from doing any analysis until all the options are selected, and makes implementation of things like progress bars slightly easier.

### Validation

#### Progress indicator

This indicates what proportion of total **code** has been run, rather than what proportion of total **time** has elapsed. I've tried to set it up to give as close an indiation as possible to time remaining, but the bar will inevitably run through the first half or so more quickly, and the latter half more slowly. I've used code progress rather than shiny being in a busy state as often switching between tabs or altering plots triggered 'busy'. <br>

#### Background functions

The function `id.def()` from `validation.R` runs in the background when `Validate` is clicked, to ensure that the specified DEF matches the data. In cases where this is not true, the **detected** DEF is used in validation, and a warning is issued that it does not match the supplied DEF. <br>

#### Mapping

As mentioned previously, there are two different mapping options, namely `basic` and `interactive`, using `ggplot2` and `plotly` respectivly. The `ggplot2` implementation is fairly basic and faster than the interactive version. <br>

The interactive plots allow you to show and hide layers and the location of errors, and to zoom and superimpose simple maps to give a spatial context to the data. <br>

#### Spatial validation

Spatial validation is conducted by the `validation.R` functions `crs.check()` and `geom.test()`.

-   `crs.check()` simply compares the shapefile proj4string and EPSG definitions to the expected values, namely `'+proj=longlat +datum=WGS84 +no_defs'` and `4326` respectivly, and returns a message if these are correct, or if one or both are incorrect

-   `geom.test()` runs a `st_is_valid()` check from the package `sf` on the shapefile, returning the results as a table, indicating which polygon has which error and at which location. This information is also plotted under `Validity errors` on the mapping tab <br>

#### Overlap validation

Validation that overlaps are correct is conducted by the `validation.R` functions `overlap.test()` and `intersect.test()`.

-   `overlap.test()` checks for **exact overlaps**, and reports when exactly overlapping polygons do not share an identical `POLYGON` field value. This function also looks at all polygons sharing `POLYGON` field values, and reports if these are **not** overlapping exactly. This check is conducted using `st_equals()` from `sf`.

-   `intersect.test()` checks for **partial overlaps** in polygons, and reports which polygon partially overlaps which other polygons. In the case of a 'Study Area DEF' shapefile, this function also reports if there is more than one feature present in the file. This check is conducted using `st_overlaps()` from `sf`. I had originally intended to use `st_relate()` and search for polygon relationships using DE9-IM strings, however this proved to be much slower in the case of large files. <br>

#### Dataset validation

Validation of the dataset, i.e. that all mandatory fields are present and of the correct format and class, are conducted using the `validation.R` function `field.checks()`. This function takes the information presented in the `colval` dataframe from `validation.R`, and uses it to assess in the following order:

1.  Whether the GUI field contains a single, unique value
2.  Whether essential DEF fields are present
3.  Whether the DEF fields present are the correct class
4.  Whether essential DEF fields are complete (i.e. lack missing values)
5.  Whether DEF fields are formatted unexpectedly (e.g. if GUI is not formatted as two letters and six numbers)

Where a DEF field is not mandatory but present in the uploaded file, errors are only returned when the data does not match the expected format or class. Fields present in the data but not in the DEF are returned with the message *"This field is not present in the DEF and may be discarded upon upload"*. <br>

### Comments and suggestions

Let me know if you have any comments or suggestions about bugs, special cases in the data that are not being flagged, features that should be added or removed, or any suggestions for improvements to the look, user-experience, or speed of the app.

<br> <br> <br>
