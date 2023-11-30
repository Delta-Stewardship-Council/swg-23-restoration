## Cleaned Data

The data are in the cloud storage > data > scraped_data > cnra > clean_data.

There is an overall key that links each project to a program, "program_project_key.csv". Each csv related to projects has a `project_id` and each csv related to programs has a `program_id`. The spatial geometries are split up over different geometry types (eg. point, linestring, etc) in a geopackage. These can be related to other project files using `ProjectNo_FK`, which is equivalent to `project_id`. Note that four projects did not contain spatial geometries. This is reflected in the "spatial_directory.csv".

The cleaned data has not been (spatially) filtered for the Bay-Delta. You may do this by first loading the extent the Restoration Group defined as the polygonal boundary of our study area. Then load `projects_spatial.gpkg`, one layer at a time. For each layer do a spatial subset, eg `projects_points[study_boundary,,op = st_intersects]`. Then, find all the unique `ProjectNo_FK` values, which can then be filtered with other CNRA cleaned files.

For more guidance on spatial processing, see Robin Lovelace's [Geocomputation with R](https://bookdown.org/robinlovelace/geocompr/spatial-operations.html).