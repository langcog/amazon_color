####### Dockerfile #######
FROM rocker/tidyverse:3.5.0

ENV DEBIAN_FRONTEND noninteractive


COPY git_config.sh /etc/cont-init.d/gitconfig
COPY set_theme.sh /etc/cont-init.d/theme

RUN apt-get update -qq \ 
&& apt-get -y --no-install-recommends install \
	jags \
    libgdal-dev \
    libmagick++-dev \
    libnlopt-dev \
    libudunits2-dev \
    libxt-dev \
&& install2.r --error \
    --deps TRUE \
    cowplot \
    DT \
    english \
    ggthemes \
    ggrepel \
    lme4 \
    nnet \
    plotly \
&& installGithub.r \
    langcog/wordbankr@fec28549941a32368f8e75bdd204c2f43f1d33ad
