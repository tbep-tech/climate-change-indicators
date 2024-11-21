# Climate Change Indicators Server

This directory contains the Docker configuration for running the Climate Change Indicators Shiny application with automated daily data updates.

## Prerequisites

-   Docker installed on your system

## Building the Docker Image

From the root of the climate-change-indicators repository:

``` bash
docker build -t climate-indicators ./server
```

## Running the Container

The container is designed to mount your local directories, allowing you to modify both the app and update scripts without rebuilding the image:

``` bash
docker run -d \
  -p 3838:3838 \
  -p 8787:8787 \
  -v $(pwd)/app:/srv/shiny-server/app \
  -v $(pwd)/data:/srv/shiny-server/app/data \
  -v $(pwd)/server:/srv/scripts \
  -e PASSWORD=rstudio \
  --name climate-indicators \
  climate-indicators
```

Replace: - `$(pwd)/app` with the absolute path to your app directory - `$(pwd)/data` with where you want the data to be stored locally - `$(pwd)/server` with the directory containing your update_data.R script

This setup allows you to: - Modify the app files locally and see changes immediately - Update the data update script without rebuilding - Persist data between container restarts - Develop and test changes without rebuilding the container

## Environment Setup

The container requires certain environment variables for Git authentication and operations. Create a `.env` file in the server directory with the following variables:

```bash
# Git Authentication
GITHUB_PAT=your_github_personal_access_token
GIT_USER=your_git_username
GIT_EMAIL=your_email@example.com
```

To create a GitHub Personal Access Token:
1. Go to GitHub Settings > Developer settings > Personal access tokens
2. Generate a new token with 'repo' scope
3. Copy the token and add it to your `.env` file

Note: The `.env` file is included in `.gitignore` to prevent exposing sensitive information.

## Development Workflow

1.  Make changes to files in your local `app` directory

    -   Changes will be immediately reflected in the running container
    -   Refresh your browser to see the updates

2.  Modify the update script in `server/update_data.R`

    -   Changes will be used in the next scheduled update

    -   Test changes immediately by running:

        ``` bash
        docker exec climate-indicators Rscript /srv/scripts/update_data.R
        ```

3.  Data updates will be stored in your local `data` directory

4.  Interactive Development with RStudio

    -   Access RStudio in the container at http://localhost:8787
    -   Default credentials: rstudio/rstudio
    -   Use RStudio for:
        -   Interactive debugging of update_data.R
        -   Testing new data processing functions
        -   Installing and testing new R packages

    Note: When installing new packages in the RStudio session, remember to:

    1.  Document the package in a comment in your code
    2.  Add it to the Dockerfile's `install2.r` command
    3.  Rebuild the image to make the change permanent

This development setup provides a full R development environment while maintaining reproducibility through the Dockerfile.

## Accessing the Application

Once the container is running, the Shiny app will be available at: - http://localhost:3838/app/

## Data Updates

The container is configured to automatically update data daily at midnight (UTC) using an R script. Updates include: - PRISM climate data (temperature and precipitation) - Sea level data - Sea Surface Temperature (SST) data

The update process includes comprehensive logging and error handling. Logs can be found in the container at `/var/log/climate_data_update.log`.

To manually trigger a data update:

``` bash
docker exec climate-indicators Rscript /srv/scripts/update_data.R
```

## Monitoring and Maintenance

View container logs:

``` bash
docker logs climate-indicators
```

View data update logs:

``` bash
docker exec climate-indicators cat /var/log/climate_data_update.log
```

Stop the container:

``` bash
docker stop climate-indicators
```

Remove the container:

``` bash
docker rm climate-indicators
```

## R Package Dependencies

The following R packages are automatically installed in the container: - bsicons - bslib - dplyr - glue - here - htmltools - leaflet - leaflet.extras2 - lubridate - prism - purrr - RColorBrewer - readr - sf - stringr - terra - tidyr - webshot2 - tbeptools (from GitHub: tbep-tech/tbeptools)
