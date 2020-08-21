[![Build Status](https://travis-ci.com/L7R7/gitlab-ci-build-statuses.svg?branch=master)](https://travis-ci.com/L7R7/gitlab-ci-build-statuses)

# gitlab-ci-build-statuses

Fetch the current statuses of the latest Gitlab CI pipelines for all default branches in a Gitlab group and show them on an HTML page.

## Usage

### Configuration

The application has to be configured via environment variables.
Some of them are mandatory, others are optional:
* `GITLAB_BASE_URL`: The base URL for the Gitlab instance you want to fetch the data from, e.g. `https://www.gitlab.com/`
* `GITLAB_GROUP_ID`: The ID of the group in Gitlab for which the build statuses to get.
* `GITLAB_API_TOKEN`: The Access Token for the Gitlab API
* `DATA_UPDATE_INTERVAL_MINS` (optional): Sets the interval in minutes that defines how often the cached data should be updated.
Default value is 1 minute.
The higher the value, the lower the number of requests to the Gitlab API, the lower the value the less stale will the in-memory data be.
* `UI_UPDATE_INTERVAL_SECS` (optional): Sets the interval in seconds that defines how often the HTML page will refresh itself.
Default value is 5 seconds.

The app won't start if not all mandatory configuration properties are set.
There will be a log message with the details before the application exits.

### Run it

The most straightforward way to use this is to run the Docker image that's provided.

    docker run -p 8282:8282 l7r7/gitlab-ci-build-statuses:latest

You can set the environment variables using the `-e` flag.

### API
The app exposes the following endpoints:
* `/statuses`: Responds with an HTML page that shows the current statuses of the pipelines
* `/health`: Responds with a status indicating if the app is ready to serve requests
* `/metrics`: Returns [Prometheus](https://prometheus.io/) application metrics