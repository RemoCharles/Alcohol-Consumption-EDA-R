FROM rocker/shiny:3.5.1

RUN apt-get -y install libcurl4-gnutls-dev libxml2-dev libssl-dev sudo
RUN rm -r /srv/shiny-server
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN sed -i 's/3838/3838 0.0.0.0/' /etc/shiny-server/shiny-server.conf

RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('knitr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('pander', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('corrplot', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('randomForest', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rpart', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('caret', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('kwstat/corrgram')"

COPY /app /srv/shiny-server/ 
RUN sudo chown -R shiny:shiny /srv/shiny-server


EXPOSE 3838 
CMD ["/usr/bin/shiny-server.sh"]
