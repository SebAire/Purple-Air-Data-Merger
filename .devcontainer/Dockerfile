# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN install.r remotes
COPY DESCRIPTION .
RUN Rscript -e "remotes::install_deps()"

# RUN R -e "install.packages(c('shiny', 'DT', 'stringr', 'tidyr', 'tibble', 'luridate', 'data.table', 'openair'))"

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R
