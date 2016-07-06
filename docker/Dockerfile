FROM erlang:19.0

ARG HOST_USER_UID=1000
ARG HOST_USER_GID=1000

ADD *.sh /work/

RUN set -ex                                                 && \
                                                               \
    echo 'Creating notroot user and group from host'        && \
    groupadd -g $HOST_USER_GID notroot                      && \
    useradd -lm -u $HOST_USER_UID -g $HOST_USER_GID notroot && \
                                                               \
    echo 'Giving notroot permissions in /work'              && \
    chown -R notroot:notroot /work                          && \
                                                               \
    echo 'Installing python libraries'                      && \
    apt-get update                                          && \
    apt-get install -y --no-install-recommends                 \
      python-pip                                               \
      python-dev                                               \
      vim                                                   && \
                                                               \
    echo 'Installing jupyter'                               && \
    pip install jupyter                                     && \

    echo 'Installing kernel as notroot'                     && \
    runuser notroot -c "                                       \
      cd /work                                              && \
      ./install_script.sh                                      \
    "                                                       && \
                                                               \
    echo 'Removing unnecessary content'                     && \
    rm -rf /var/lib/apt/lists/*

CMD jupyter notebook --ip=0.0.0.0 --notebook-dir=/work/notebooks

