####### Dockerfile #######
FROM rocker/tidyverse:3.5.0

ENV DEBIAN_FRONTEND noninteractive


COPY git_config.sh /etc/cont-init.d/gitconfig
COPY set_theme.sh /etc/cont-init.d/theme

RUN apt-get update -qq \ 
&& apt-get -y --no-install-recommends install \
	jags \
    libgdal-dev \
    libxt-dev \
    libudunits2-dev \
    libnlopt-dev \
&& install2.r --error \
    --deps TRUE \
    DT \
    ggthemes \
    lme4 \
    plotly
