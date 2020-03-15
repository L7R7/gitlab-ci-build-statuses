FROM debian:stretch
USER root
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" -y ca-certificates
#         libgmp-dev

RUN groupadd service
RUN mkdir /service
RUN useradd -d /service -g service service
RUN chown root:service /service && chmod 1770 /service
WORKDIR /service

COPY /opt/build/.out/gitlab-ci-build-statuses-exe .
CMD chmod +x /service/gitlab-ci-build-statuses-exe
EXPOSE 8080

USER service
ENTRYPOINT /service/gitlab-ci-build-statuses-exe