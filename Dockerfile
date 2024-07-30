# Base R Shiny image
FROM rocker/shiny

#WORKDIR /
# Install R dependencies
#RUN install.r remotes
#COPY DESCRIPTION .
#RUN Rscript -e "remotes::install_deps()"

# Other Method to directly install dependencies
RUN R -e "install.packages(c('shiny', 'DT', 'stringr', 'tidyr', 'tibble', 'luridate', 'data.table', 'openair'))"

#RUN PWD
# Make a directory in the container
RUN mkdir /home/shiny-app

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R
#FROM rocker/shiny:latest
#COPY ./app/* /srv/shiny-server/
#CMD ["/usr/bin/shiny-server"]
