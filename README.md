# gitlab-ci-build-statuses

[![GitHub CI](https://github.com/l7r7/gitlab-ci-build-statuses/workflows/CI/badge.svg)](https://github.com/l7r7/gitlab-ci-build-statuses/actions)

Fetch the current statuses of the latest Gitlab CI pipelines for all default branches in a Gitlab group and show them on an HTML page.

## Usage

### Configuration

The application has to be configured via environment variables.
Some of them are mandatory, others are optional:

* `GCB_GITLAB_BASE_URL`: The base URL for the Gitlab instance you want to fetch the data from, e.g. `https://www.gitlab.com/`
* `GCB_GITLAB_GROUP_ID`: The ID of the group in Gitlab for which the build statuses to get.
* `GCB_GITLAB_API_TOKEN`: The Access Token for the Gitlab API
* `GCB_DATA_UPDATE_INTERVAL_SECS` (optional): Sets the interval in seconds that defines how often the cached data should be updated.
Default value is 60 seconds.
The higher the value, the lower the number of requests to the Gitlab API. The lower the value, the less stale will the in-memory data be.
* `GCB_UI_UPDATE_INTERVAL_SECS` (optional): Sets the interval in seconds that defines how often the HTML page will refresh itself.
Default value is 5 seconds.
* `GCB_PROJECT_CACHE_TTL_SECS` (optional): You can configure caching for the list of projects to reduce the load on the Gitlab API.
The provided value must be positive and sets the TTL of cached values in seconds.
By default, caching is disabled.
* `GCB_LOG_LEVEL` (optional): Set the minimum log level for log output.
Defaults to Info if the value is not set or can't be parsed.
Possible values are `DEBUG`, `INFO`, `WARN`, `ERROR`.

The app won't start if not all mandatory configuration properties are set.
There will be a log message with the details before the application exits.

### Run it

The most straightforward way to use this is to run the Docker image that's provided.

    docker run -p 8282:8282 -e GITLAB_API_TOKEN=<...> -e GITLAB_BASE_URL=<...>> -e GITLAB_GROUP_ID=<...> l7r7/gitlab-ci-build-statuses:latest

### API

The app exposes the following endpoints:

* `/statuses`: Responds with an HTML page that shows the current statuses of the pipelines.
This page will automatically refresh using the configured UI update interval.
You can use the query flag `norefresh` to disable that (this is probably only helpful for debugging)
* `/health`: Responds with a status indicating if the app is ready to serve requests
* `/metrics`: Returns [Prometheus](https://prometheus.io/) application metrics
