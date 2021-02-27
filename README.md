# gitlab-ci-build-statuses

[![GitHub CI](https://github.com/l7r7/gitlab-ci-build-statuses/workflows/CI/badge.svg)](https://github.com/l7r7/gitlab-ci-build-statuses/actions)

Fetch the current statuses of the latest Gitlab CI pipelines for all default branches in a Gitlab group and show them on an HTML page.

## Usage

### Configuration

The application has to be configured via environment variables.
Some of them are mandatory, others are optional:

* `GITLAB_BASE_URL`: The base URL for the Gitlab instance you want to fetch the data from, e.g. `https://www.gitlab.com/`
* `GITLAB_GROUP_ID`: The ID of the group in Gitlab for which the build statuses to get.
* `GITLAB_API_TOKEN`: The Access Token for the Gitlab API
* `DATA_UPDATE_INTERVAL_SECS` (optional): Sets the interval in seconds that defines how often the cached data should be updated.
Default value is 60 seconds.
The higher the value, the lower the number of requests to the Gitlab API, the lower the value the less stale will the in-memory data be.
* `UI_UPDATE_INTERVAL_SECS` (optional): Sets the interval in seconds that defines how often the HTML page will refresh
  itself. Default value is 5 seconds.
* `LOG_LEVEL` (optional): Set the minimum log level for log output. Defaults to Info if the value is not set or can't be
  parsed. Possible values are `DEBUG`, `INFO`, `WARN`, `ERROR`.

The app won't start if not all mandatory configuration properties are set.
There will be a log message with the details before the application exits.

### Run it

The most straightforward way to use this is to run the Docker image that's provided.

    docker run -p 8282:8282 -e GITLAB_API_TOKEN=<...> -e GITLAB_BASE_URL=<...>> -e GITLAB_GROUP_ID=<...> l7r7/gitlab-ci-build-statuses:latest

### API

The app exposes the following endpoints:

* `/statuses`: Responds with an HTML page that shows the current statuses of the pipelines
* `/health`: Responds with a status indicating if the app is ready to serve requests
* `/metrics`: Returns [Prometheus](https://prometheus.io/) application metrics
