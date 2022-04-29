FROM alpine

RUN addgroup service
RUN mkdir /service
RUN adduser -D -h /service -G service service
RUN chown root:service /service && chmod 1770 /service
WORKDIR /service

COPY static /service/static

COPY gitlab-ci-build-statuses-exe .
CMD chmod +x /service/gitlab-ci-build-statuses-exe
EXPOSE 8282

USER service
ENTRYPOINT /service/gitlab-ci-build-statuses-exe
