# swg-23-restoration
2023 synthesis working group restoration project

## Directories

* admin_files - Scripts to help administer github project. Not scripts for specific tasks/sub-projects.

* documentation - Documentation for the Restoration Group project.

* data_processing - This directory contains code for the download and manipulation of data for the purposes of this project. It does not contain the data itself.

* sandbox - Demonstrative, sample, or experimental scripts relevant to the project. These scripts may or may not be complete, but have not been fully incorporated into a completed task/issue for the project. In other words, these scripts are at a "proposal stage."

## Workflow & Organization

### File Naming Scheme

* The prefix "funs_" indicates functions.
* The prefix "init_" (short for "initialize") indicates a script that creates the initial conditions for other scripts. This may include loading certain packages, setting file paths, etc.

### Data Storage

All data are stored off-repository on a cloud server. This cloud includes files used as inputs (eg. csv files that contain links for scraping, or pdfs of proposals), raw data (eg. those downloaded from the web), and synthetic data (eg. dissolved polygons for Bay-Delta boundaries). Machines running these scripts should have the cloud client software installed on the local machine. Please create a bespoke directory on the cloud for the data of each task/branch.

As the cloud client will create a customized directory on the local machine for the project data, there must be a way to identify the file path to the cloud data. This is resolved by storing the file-path to the cloud data on your local machine. This document that containes the file-path is called `paths.json` in our project. This file must be stored in the root directory of the repository clone on the local machine. This file is already listed in the `.gitignore`. 

JSON files are flat files (ie. plain text files) that contain information in a tree-like form (similar to a list in R). The content of `paths.json` will differ from person to person, but it should have a key-value pair for the cloud directory on the local machine in the following format:

```
{
"box_path":"/.../NCEAS - Restoration/data"
}
```

The `...` indicates the appropriate path. Notice, the forward slash `/` is the convention used on UNIX machines, and may appear differently on Windows machines.

All code that accesses data on the cloud server should first run `source("admin_scripts/init_load_paths.R")`. This script creates a variable that contains the file-path to the cloud directory on the local machine (using `paths.json`). Once the script runs, a new variable should exist in the environment called `path_data`. This file path can be further modified with sub-directories as needed, eg. `path_scrape <- file.path(path_data, "scrape")` creates a path to the cloud server's directory, then to the "data" directory, and finally to the "scrape" directory.
