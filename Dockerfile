FROM ubuntu:19.10
USER root
RUN apt-get update && apt-get upgrade -y \
    && apt-get install -y \
        ca-certificates \
        libgmp-dev

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