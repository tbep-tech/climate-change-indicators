FROM rocker/geospatial:latest

# Install shiny server
RUN /rocker_scripts/install_shiny_server.sh

# Install system dependencies for Git
RUN apt-get update && apt-get install -y \
    git \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN install2.r --error \
    bsicons bslib curl dplyr gert glue here htmltools leaflet leaflet.extras2 lubridate \
    purrr RColorBrewer readr sf stringr terra tidyr webshot2

# Install GitHub packages
RUN installGithub.r marinebon/extractr
RUN R -e "remotes::install_github('tbep-tech/tbeptools@climate-change-indicators')"

# Create required directories with appropriate permissions
RUN mkdir -p /srv/shiny-server/app/data \
    /srv/scripts \
    /var/log \
    && chown -R shiny:shiny /srv/shiny-server \
    && chown -R shiny:shiny /var/log

# Set working directory
WORKDIR /srv/shiny-server

# Copy environment variables if .env exists
COPY --chown=shiny:shiny .env* /srv/shiny-server/.env*

# Switch to shiny user
USER shiny

# Add cron job for data updates
RUN echo "0 0 * * * /usr/bin/Rscript /srv/scripts/update_data.R >> /var/log/climate_data_update.log 2>&1" | crontab -

EXPOSE 3838

CMD ["/init"]
