# Validation data for openEO R back-end
The goal for this package is to provide an R script embedded into a Docker container that loads the validation data provided by [Sijoma/openeo-sentinel-referencedata](https://github.com/Sijoma/openeo-sentinel-referencedata).
With this Version all the Sentinel-2 reference can be downloaded and prepared for import into the openEO R-backend.

## Package structure
The folder `R/` contains the importer script, which will be executed on the container startup. In the best case this container is only run once. To be sure, you can also configure the container via environment variables and enable or disable the data sets individually. For the Docker part, this package contains a Dockerfile to create the image and a `docker-compose` file that will build a container based on this image with all the configurable environment variables.

## Installation
Download or check-out this Github project and change the environment variables with regard to your own settings (see section "Environment variables").
Switch into the root folder, where you downloaded this package to and run.
```
docker-compose up -d
```
The downloading and preparation for the openEO R back-end will take a lot of time, which also depends on your internet connection.

## Environment variables
| Key | Description | Required |
|--|--| -- |
| `COPERNICUS_USER` | Your user name with which you have registered at [scihub](https://scihub.copernicus.eu/) | [X] |
| `COPERNICUS_PWD` | The password for the user provided above | [X] |
| `DATA_ITALY` | `true` or `false` (default). Whether or not to download and preprocess the Sentinel-2 L2A reference data for Italy (2.2778 GB) |  |
| `DATA_SWITZERLAND` | `true` or `false` (default). For the Sentinel-2 L2A Switzerland data (2.336 GB) |  |
| `DATA_UGANDA` | `true` or `false` (default). For the Sentinel-2 L2A Uganda data (3.124 GB) |  |
| `DATA_ISLAND` | `true` or `false` (default). For the Sentinel-2 L2A Island data (4.026 GB) |  |
| `DATA_VENEZUELA` | `true` or `false` (default). For the Sentinel-2 L1C Venezuela data (3.017 GB) |  |
| `DATA_NETHERLANDS` | `true` or `false` (default). For the Sentinel-2 L2A Netherlands time series data set (35.154 GB) |  |
| `DOWNLOAD_DIR` | the temporary folder where to download the data to. Default: `/var/openeo/tmp/` | [x] |
| `DATA_DIR` | the folder where the readily processed data is stored to (like the data folder in the openEO R back-end). Default `/var/openeo/data/` | [x] |

You are best advised not to change `DOWNLOAD_DIR` and `DATA_DIR` unless you know what you do, because they are preset for the use with Docker and external directories.

## Advanced configuration
If you have already downloaded the data. Then you can manually put all the data into the `DOWNLOAD_DIR` under its zone or country name, e.g. `DATA_ITALY` goes into `DOWNLOAD_DIR`/italy and so on. But this requires some more configurations in the docker-compose file, because you need to map `/var/openeo/tmp/` to the folder on your own computer.




