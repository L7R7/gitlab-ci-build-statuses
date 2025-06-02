FROM fpco/stack-build:lts-21.22 as dependencies
RUN mkdir -p /opt/build
WORKDIR /opt/build

COPY stack.yaml package.yaml /opt/build/

RUN stack build --system-ghc --dependencies-only

FROM fpco/stack-build:lts-21.22 as build

COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/
WORKDIR /opt/build
RUN stack build --system-ghc --allow-different-user --copy-bins --local-bin-path .

FROM ubuntu:24.04 as app
USER root
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" -y ca-certificates
RUN apt-get install -y libgmp-dev libgmp10
RUN apt-get install procps

RUN groupadd service
RUN mkdir /service
RUN useradd -d /service -g service service
RUN chown root:service /service && chmod 1770 /service
WORKDIR /service

COPY static /service/static

COPY --from=build /service/gitlab-ci-build-statuses-exe .
RUN chmod +x /service/gitlab-ci-build-statuses-exe
EXPOSE 8282

USER service
CMD ["/service/gitlab-ci-build-statuses-exe"]
