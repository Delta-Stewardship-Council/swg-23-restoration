## Ecoatlas Scraping Documentation

* funs are functions
* start with scrape1

## Cloud Structure

The primary directory is:
cloud > data > scraped_data > ecoatlas

* **region_types** is the first important directory. This contains a comprehensive set of files that helps you navigate the larger categories where EcoAtlas projects might fall under. Start with region_type_keys.csv

* **project_listings** is the second important directory. This is tied to the first scraping script which get a list of all possible projects of interest for this repository. It uses `source_list.csv` in the repository to identify the possible projects of interest. 

* **project_details** is where the actual data is stored.