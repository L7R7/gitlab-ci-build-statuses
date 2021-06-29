# gitlab-ci-build-statuses

[![build](https://github.com/l7r7/gitlab-ci-build-statuses/actions/workflows/build.yml/badge.svg)](https://github.com/l7r7/gitlab-ci-build-statuses/actions)

Fetch the current statuses of the latest Gitlab CI pipelines for all default branches in a Gitlab group and show them on an HTML page.  
Optionally, collect information about the pipeline jobs that are running at the moment and show them grouped by the runner that executes the job.

## Features

* Lightweight UI
    * Plain HTML and CSS, barely any JS involved
    * Refreshes automatically, so you always see the latest statuses
    * The favicon will show an indicator summarizing all statuses, so you can directly see if everything is fine (this is especially helpful if you pin the tab in your browser)
* Production-ready Docker container including Prometheus metrics, health endpoint, and configurable structured JSON logging
* Caching for the list of projects of the group to speed up the regular updates as well as reduce the load on the Gitlab API
* The pipelines will be determined in the following way:
    * Get all the projects for the given group
    * For all projects that have a default branch, try to get the latest pipeline run for the default branch
    * Determine the status of the pipeline and include it in the result

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
* `GCB_INCLUDE_SHARED_PROJECTS`(optional): Sets whether projects that are shared with the group should be included.
Possible values are `include` and `exclude`.
Default value is `include`.
* `GCB_PROJECT_CACHE_TTL_SECS` (optional): You can configure caching for the list of projects to reduce the load on the Gitlab API.
The provided value must be positive and sets the TTL of cached values in seconds.
Default value is `3600` (1h).
* `GCB_LOG_LEVEL` (optional): Set the minimum log level for log output.
Defaults to Info if the value is not set or can't be parsed.
Possible values are `DEBUG`, `INFO`, `WARN`, `ERROR`.
* `GCB_EXCLUDE_PROJECTS` (optional): Set a list of projects that should be excluded from the list.
Must be a comma-separated list of integers of project IDs.
Defaults to an empty list if the value is not set or can't be parsed.
If you specify projects that don't appear in the result from the API, a warning will be logged.
* `GCB_JOBS_VIEW` (optional): Enables/disables whether to fetch the data for the currently running pipeline jobs.
Possible values are `enabled` and `disabled`.
* Default value is `disabled`.
* `GCB_RUNNER_CACHE_TTL_SECS` (optional): You can configure caching for the list of online runners to reduce the load on the Gitlab API.
The provided value must be positive and sets the TTL of cached values in seconds.
Default value is `300` (5m).

The app won't start if not all mandatory configuration properties are set.
There will be a log message with the details before the application exits.

### Run it

The most straightforward way to use this is to run the Docker image that's provided.

```sh
 docker run -p 8282:8282 \
  -e GCB_GITLAB_API_TOKEN=xyz \
  -e GCB_GITLAB_BASE_URL=https://example.gitlab.com \
  -e GCB_GITLAB_GROUP_ID=1 \
  l7r7/gitlab-ci-build-statuses:latest
```

### API

The app exposes the following endpoints:

* `/statuses`: Responds with an HTML page that shows the current statuses of the pipelines.
This page will automatically refresh using the configured UI update interval.
You can use the query flag `norefresh` to disable that (this is probably only helpful for debugging)
* `/jobs`: Responds with an HTML page that shows the current running jobs on the online runners.
This page will automatically refresh using the configured UI update interval.
You can use the query flag `norefresh` to disable that (this is probably only helpful for debugging)
* `/health`: Responds with a status indicating if the app is ready to serve requests.
The status code will be either 200 or 503, the body will always be JSON and will include a `status` field that's either "HEALTHY" or "UNHEALTHY" alongside a `build` field that shows which version of the code is running
* `/metrics`: Returns [Prometheus](https://prometheus.io/) application metrics

All endpoints are available under the prefix `/builds` as well.
This is especially helpful when you deploy the app behind something like an ingress proxy where you want to have a clear prefix to do the routing.

## Operating showcase

This repository includes a showcase for a docker-compose based deployment in [docker-compose](docker-compose/).
This includes the app itself (you have to configure it in [the docker-compose file](docker-compose/docker-compose.yml)), a Prometheus and a Grafana including a ready to go setup with some dashboards to demonstrate what the app offers.

## FAQ

### My Gitlab group has subgroups. Will the pipelines of projects in there be included?

Yes. Projects in subgroups will be included.
At the moment the status page will show a flat list of pipeline statuses.

### How is it possible for a project to have no default branch?

The corresponding [API docs](https://docs.gitlab.com/ee/api/projects.html#list-all-projects) don't say that, but if a project is empty (e.g. if it was just created) it doesn't have a default branch.

### Why do you need an extra call to get the single pipeline to determine the build status?

When I built this, I found out that the API is not always ideal for my needs.
In my project, we're using jobs that are allowed to fail (e.g. because they require manual steps, or just run some checks that shouldn't break the pipeline).
I'd like to make it clear on the status page if a pipeline was successful or if it ended with warnings.
However, if a pipeline fails with warnings the status that is returned by the [List Project Pipelines](https://docs.gitlab.com/ee/api/pipelines.html#list-project-pipelines) Endpoint will be `success`.
To get the exact status, a request to the [Single Pipeline](https://docs.gitlab.com/ee/api/pipelines.html#get-a-single-pipeline) Endpoint is necessary for a pipeline that seems to be successful.  
There are two open issues that address this inconsistency in the API (see [here](https://gitlab.com/gitlab-org/gitlab/-/issues/323025) and [here](https://gitlab.com/gitlab-org/gitlab/-/issues/229137)).

### Isn't this thing slightly over-engineered?

Well, yes. It probably is.
To be honest, the intention of this project never was to build something that's the ideal, minimal solution for the problem.
I was looking for something to build with Haskell to see if I had learned enough to get this working.
I learned a lot while building this, and that's what is important to me in this case.
