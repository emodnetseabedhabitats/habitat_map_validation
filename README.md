## EMODnet Data Validation Tool
#### Alun Jones
#### 1st May 2020

<br>

This page gives an overview to the new [EMODnet Habitat Map Validation Tool](https://emodnetseabedhabitats.shinyapps.io/MapValidationTool/) developed with Shiny and R, specifically looking at why features were added, and how the validation functions work, what they check for, and what they don't. <br>

### Opening screen

#### 1. **Zip upload**
For more efficient processing and memory limitations, the tool requires users to upload a zipped collection of shapefile documents. The contents of the zip file should be one individual habitat map or study area with all associated shapefiles documents. For example:

1. `FR003015_TH.zip` habitat map:
    * `FR003015_TH.dbf`
    * `FR003015_TH.prj`
    * `FR003015_TH.shp`
    * `FR003015_TH.shx`
    * `FR003015_TH.xml`
2. `FR003015_StudyArea.zip` study area:
    * `FR003015_SA.dbf`
    * `FR003015_SA.prj`
    * `FR003015_SA.shp`
    * `FR003015_SA.shx`
    * `FR003015_SA.xml`

At present the upper limit for zip file size is 30MB, and rocessing time will vary depending on the size of the uploaded maps. Please avoid uploading files beyond this limit as this may put excessive stress on the server. <br>
    
#### 2. **DEF selection**
Users should select the Data Exchange Format (DEF) relevant to the habitat map being validated, which will then perform the necessary validation checks tailored to the specific DEF. The tool will notify users if the selected DEF is different from the one detected in the uploaded habitat map. <br>
    
#### 3. **Plotting type**
A number of plotting options were explored, but it was found the interactive `plotly` package allows for visualisation options which are particularly useful, namely the ability to hide layers, zoom, and highlight the locations of errors. This functionality can be particularly slow with large files, so a default option for static, basis plotting option was included using `ggplot2`. <br>

#### 4. **Validate button**
Once all of the above criteria have been met, users simply run the tool by hitting the `Validate` button. <br>
    
### Validation
#### Progress indicator

This indicates what proportion of total **code** has been run, rather than what proportion of total **time** has elapsed. The tool has been set up to give as close an indication as possible to time remaining, but the bar will inevitably run through the first half or so quicker, and the latter half slower. Code progress has been used rather than shiny being in a busy state as often switching between tabs or altering plots triggers `busy`. <br>

#### Background functions
The function `id.def()` from `validation.R` runs in the background when `Validate` is clicked, to ensure that the specified DEF matches the data. In cases where this is not true, the **detected** DEF is used in validation, and a warning is issued that it does not match the supplied DEF. <br>

#### Shapefile mapping
As mentioned previously, there are two different mapping options, namely `basic` and `interactive`, using `ggplot2` and `plotly` respectivly. The `ggplot2` implementation is fairly basic and faster than the interactive version. <br>

The interactive plots allow you to show and hide layers and the location of errors, and to zoom and superimpose simple maps to give a spatial context to the data. Users are provided with specific interactive options for visualising data:
   * Single click a legend entry to hide/show that layer.
   * Double click a legend entry for a displayed layer to show only that layer.
   * Double click a legend entry for a hidden layer to display all layers.
   * Double click the plot area to reset plot zoom and extent.
   * Scroll to zoom.

Final outputs will look similar to below:  
![alt text](https://github.com/emodnetseabedhabitats/habitat_map_validation/blob/master/FR003015_shapefileMapping.PNG "Shapefile mapping report")

#### Spatial validation

Spatial validation is conducted by the `validation.R` functions `crs.check()` and `geom.test()` and provides users information on the geometry check performed. <br>
    * `crs.check()` simply compares the shapefile proj4string and EPSG definitions to the expected values, namely `'+proj=longlat +datum=WGS84 +no_defs'` and `4326` respectivly, and returns a message if these are correct, or if one or both are incorrect <br>
    * `geom.test()` runs a `st_is_valid()` check from the package `sf` on the shapefile, returning the results as a table, indicating which polygon has which error and at which location. This information is also plotted under `Validity errors` on the mapping tab <br>

![alt text](https://github.com/emodnetseabedhabitats/habitat_map_validation/blob/master/FR003015_spatialValidation.PNG "Spatial validation report")

#### Overlap validation
Overlap validation is performed by the `validation.R` functions `overlap.test()` and `intersect.test()`.

-   `overlap.test()` checks for **exact overlaps**, and reports when exactly overlapping polygons do not share an identical `POLYGON` field value. This function also looks at all polygons sharing `POLYGON` field values, and reports if these are **not** overlapping exactly. This check is conducted using `st_equals()` from `sf`.

-   `intersect.test()` checks for **partial overlaps** in polygons, and reports which polygon partially overlaps which other polygons. In the case of a 'Study Area DEF' shapefile, this function also reports if there is more than one feature present in the file. This check is conducted using `st_overlaps()` from `sf`. The `st_relate()` function was originally intended to be used and search for polygon relationships using the DE9-IM strings, however this was much slower in the case of large files. <br> 

![alt text](https://github.com/emodnetseabedhabitats/habitat_map_validation/blob/master/FR003015_overlapValidation.PNG "Overlap validation report")

#### Dataset validation
Validation of the dataset, i.e. that all mandatory fields are present and of the correct format and class, are conducted using the `validation.R` function `field.checks()`. This function takes the information presented in the `colval` dataframe from `validation.R`, and uses it to assess in the following order:

1.  Whether the GUI field contains a single, unique value
2.  Whether essential DEF fields are present
3.  Whether the DEF fields present are the correct class
4.  Whether essential DEF fields are complete (i.e. lack missing values)
5.  Whether DEF fields are formatted unexpectedly (e.g. if GUI is not formatted as two letters and six numbers)

Where a DEF field is not mandatory but present in the uploaded file, errors are only returned when the data does not match the expected format or class. Fields present in the data but not in the DEF are returned with the message *"This field is not present in the DEF and may be discarded upon upload"*. <br>
![alt text](https://github.com/emodnetseabedhabitats/habitat_map_validation/blob/master/FR003015_datasetValidation.PNG "Dataset validation report")

### Comments and suggestions
Please let the EMODnet Seabed Habitats team know if you have comments or suggestions about bugs, special cases in the data that are not being flagged, features that should be added or removed, or any suggestions for improvements to the look, user-experience, or speed of the app.
<br> <br> <br>
