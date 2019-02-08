FROM ubuntu

RUN apt-get update \
 && apt-get install -y curl \
 && apt-get install -y sudo \
 && apt-get install -y git \
 && mkdir -p /nix /etc/nix \
 && chmod a+rwx /nix \
 && echo 'sandbox = false' > /etc/nix/nix.conf


RUN adduser reflex --home /home/reflex --disabled-password --gecos "" --shell /bin/bash
RUN adduser reflex sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER reflex
ENV USER reflex
WORKDIR /home/reflex
RUN echo "if [ -e /home/reflex/.nix-profile/etc/profile.d/nix.sh ]; then . /home/reflex/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer" >> .bashrc

RUN curl https://nixos.org/nix/install | sh
RUN git clone https://github.com/reflex-frp/reflex-platform

## RUN export installing_nix = true
## WORKDIR /home/reflex/reflex-platform/scripts
## RUN . /home/reflex/reflex-platform/scripts/common-setup.sh