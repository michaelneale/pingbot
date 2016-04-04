FROM haskell:7.8 
# mwotton/meanpath-build

#RUN echo 43

RUN apt-get update
#RUN apt-get install -y ghc-7.8.3

RUN apt-get install -y tmux make
RUN apt-get install -y curl python

WORKDIR /app

#ENV PATH /opt/ghc/7.8.3/bin:/opt/cabal/1.20/bin/:$PATH

ENV CONSOLE_URL=http://localhost:8000/checks.json

CMD python -m SimpleHTTPServer >> /tmp/simple.log & \
    /bin/bash
