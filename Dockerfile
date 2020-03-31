FROM ubuntu:18.04
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

COPY gitlab-ci-build-statuses-exe .
CMD chmod +x /service/gitlab-ci-build-statuses-exe
EXPOSE 8282
EXPOSE 8181

USER service
ENTRYPOINT /service/gitlab-ci-build-statuses-exe