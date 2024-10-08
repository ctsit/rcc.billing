FROM redcapcustodian

WORKDIR /home/rocker

## install additional system libraries not included in redcapcustodian if necessary
# e.g. to add sftp support
# RUN apt update -y && apt install -y openssh-client

## install additional R libraries not included in redcapcustodian if necessary
RUN R -e "install.packages(c( \
    'tableHTML', \
    'argparse' \
))"

## Install our private rcc.ctsit package
## see: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token
## Also see https://docs.github.com/en/developers/apps/getting-started-with-apps/about-apps
ARG GITHUB_PAT=personal_access_token
## Stop using cache to ensure latest private package is always installed
## https://stackoverflow.com/a/49772666/7418735
ARG TIMESTAMP=1611761435
RUN R -e "devtools::install_github('ctsit/rcc.ctsit', auth_token = '$GITHUB_PAT')"

## e.g. pin to a specific version of an R package
# RUN R -e "devtools::install_github('OuhscBbmc/REDCapR', ref='c5bce6a')"

# build and install this package
ADD . /home/rocker/rcc.billing
RUN R CMD build rcc.billing
RUN R CMD INSTALL rcc.billing_*.tar.gz
RUN rm -rf rcc.billing

# Add non-package things
ADD . /home/rocker
RUN rm -rf .Rbuildignore
RUN rm -rf NAMESPACE
RUN rm -rf R
RUN rm -rf .dockerignore
RUN rm -rf DESCRIPTION
RUN rm -rf .Rhistory
RUN rm -rf Dockerfile

# Note where we are, what is there, and what's in the package dir
CMD pwd && ls -AlhF ./
