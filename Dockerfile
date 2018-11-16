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
    here \
    lme4 \
    nnet \
    plotly \
    viridis \
&& installGithub.r \
    langcog/wordbankr@fec28549941a32368f8e75bdd204c2f43f1d33ad \
    crsh/papaja@c55ba73cd4490083b0fb1a45df63c48bb5ba3eeb
