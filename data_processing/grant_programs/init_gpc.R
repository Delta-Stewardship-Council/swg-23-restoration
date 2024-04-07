# Note, this is not the "main" gpc script. It only contains script that is
# recycled throughout this directory's scripts. To explore the purpose of this
# directory further, read the readme, or start with `gpc1...R`

# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

# Load libraries
load_libs(.libs = c(
  "tidyverse",
  "readxl",
  "sf",
  "digest"
))

# General Paths for this Directory ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create path to grant program coordinates directory
pth$gpc <- file.path(pth$data, "grant_program_coordinates")

# raw data
pth$raw <- file.path(pth$gpc, "raw")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# intermediate steps
pth$itm <- file.path(pth$gpc, "intermediate_steps")

# Create dir.
check_dir(pth$itm)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create final deliverables path
pth$out <- file.path(pth$gpc, "deliverables")

# Create dir.
check_dir(pth$out)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# gpc1 script paths ----
pth$gpc1 <- file.path(pth$itm, "gpc1")

pth$gpc1_gpkg <- file.path(pth$gpc1, "gpc1_spatial.gpkg")

# Create dir.
check_dir(pth$gpc1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# gpc2 script paths ----
pth$gpc2 <- file.path(pth$itm, "gpc2")

pth$gpc2_gpkg <- file.path(pth$gpc2, "gpc2_spatial.gpkg")

# Create dir.
check_dir(pth$gpc2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# gpc3 script paths ----
pth$gpc3 <- file.path(pth$itm, "gpc3")

pth$gpc3_gpkg <- file.path(pth$gpc3, "gpc3_spatial.gpkg")

# Create dir.
check_dir(pth$gpc3)

# load functions
source(file.path(pth$home, 
                 "data_processing", 
                 "grant_programs",
                 "funs_gpc.R"))