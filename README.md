# gitlab-ci-build-statuses

[![build](https://github.com/l7r7/gitlab-ci-build-statuses/actions/workflows/build.yml/badge.svg)](https://github.com/l7r7/gitlab-ci-build-statuses/actions) ![code stats](https://tokei.rs/b1/github/l7r7/gitlab-ci-build-statuses)

Fetch the current statuses of the latest Gitlab CI pipelines for the default branches of a list of projects (all projects from one or more groups in GitLab, and/or an explicit list of projects) and show them on an HTML page.
Optionally, collect information about the pipeline jobs that are running at the moment and show them grouped by the runner that executes the job.

## Features

* Lightweight UI
    * Plain HTML and CSS, barely any JS involved
    * Refreshes automatically, so you always see the latest statuses
    * The favicon will show an indicator summarizing all statuses, so you can directly see if everything is fine (this is especially helpful if you pin the tab in your browser)
    * Direct links to the pipeline/job, so you can get to the details fast
* Production-ready Docker container including Prometheus metrics, health endpoint, and configurable structured JSON logging
* Caching for the list of projects of the group to speed up the regular updates as well as reduce the load on the Gitlab API
* The pipelines will be determined in the following way:
    * Get all relevant projects
        * All projects in all provided groups
        * All extra projects that have been configured
    * For the projects that have a default branch, try to get the latest pipeline run for the default branch
    * Determine the status of the pipeline and include it in the result

## UI samples

### Current build statuses (GET `/statuses`, GET `/statuses?view=plain`)

![statuses](https://user-images.githubusercontent.com/16477399/141801876-cdcd3517-3931-486c-8c21-45c85c5ff979.png)

### Current build statuses grouped by subgroups (GET `/statuses?view=grouped`)

![statuses-grouped](https://user-images.githubusercontent.com/16477399/176452677-7d330c43-0c90-4fef-ae77-8841a9c40601.png)

### Current running jobs (GET `/jobs`, if the jobs view is enabled)

![jobs](https://user-images.githubusercontent.com/16477399/141801911-53e67c17-3909-42a6-b1cd-0a9f58ae9ec9.png)

## Usage

### Configuration

The application has to be configured via environment variables.
Some of them are mandatory, others are optional:

* `GCB_GITLAB_BASE_URL`: The base URL for the Gitlab instance you want to fetch the data from, e.g. `https://www.gitlab.com/`
* `GCB_GITLAB_GROUP_ID` (optional): Comma-separated list of group IDs for which the build statuses to get.
* `GCB_EXTRA_PROJECTS` (optional): Set a lists of additional projects that should be included in the list.
  Must be a comma-separated list of integers of project IDs.
  Defaults to an empty list if the value is not set or can't be parsed.
* `GCB_GITLAB_API_TOKEN`: The Access Token for the Gitlab API
* `GCB_USER_AGENT` (optional): Sets the User-Agent that will be used for requests to the Gitlab API. Defaults to `gitlab-ci-build-statuses`.
* `GCB_DATA_UPDATE_INTERVAL_SECS` (optional): Sets the interval in seconds that defines how often the cached data should be updated.
  The higher the value, the lower the number of requests to the Gitlab API.
  The lower the value, the less stale will the in-memory data be.
  Default value is 60 seconds.
* `GCB_UI_UPDATE_INTERVAL_SECS` (optional): Sets the interval in seconds that defines how often the HTML page will refresh itself.
Default value is 5 seconds.
* `GCB_INCLUDE_SHARED_PROJECTS`(optional): Sets whether projects that are shared with the group should be included.
Possible values are `include` and `exclude`.
Default value is `include`.
* `GCB_PROJECT_CACHE_TTL_SECS` (optional): You can configure caching for the list of projects to reduce the load on the Gitlab API.
The provided value must be positive and sets the TTL of cached values in seconds.
Default value is `3600` (1h).
* `GCB_LOG_LEVEL` (optional): Set the minimum log level for log output.
  Possible values are `DEBUG`, `INFO`, `WARN`, `ERROR`.
  Defaults to Info if the value is not set or can't be parsed.
* `GCB_EXCLUDE_PROJECTS` (optional): Set a list of projects that should be excluded from the list.
  Must be a comma-separated list of integers of project IDs.
  If you specify projects that don't appear in the result from the API, a warning will be logged.
  Defaults to an empty list if the value is not set or can't be parsed.
* `GCB_JOBS_VIEW` (optional): Enables/disables whether to fetch the data for the currently running pipeline jobs.
  Possible values are `enabled` and `disabled`.
  Default value is `enabled`.
* `GCB_RUNNER_CACHE_TTL_SECS` (optional): You can configure caching for the list of online runners to reduce the load on the Gitlab API.
The provided value must be positive and sets the TTL of cached values in seconds.
Default value is `300` (5m).

The app won't start if not all mandatory configuration properties are set.
There will be a log message with the details before the application exits.

### Run it

The most straightforward way to use this is to run the Docker image that's provided via [Docker Hub](https://hub.docker.com/r/l7r7/gitlab-ci-build-statuses).

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
You can choose between a plain view of projects and a grouped view, in which the tiles will be arranged according to the subgropus in your group.
There's a link on the page to toggle the view, you can also use the `view` query parameter to choose. Possible values are `plain` and `grouped`, the former being the default.
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

This repository includes a showcase for a docker-compose based deployment in [docker-compose](docker-compose).
This includes the app itself (you have to configure it in [the docker-compose file](docker-compose/docker-compose.yml)), a Prometheus and a Grafana including a ready to go setup with some dashboards to demonstrate what the app offers.

### Grafana

You'll find the Grafana instance at [http://localhost:3000](http://localhost:3000).
It includes a dashboard that looks like this:

![screencapture-localhost-3000-d-oD7GwCVMk-build-statuses-2021-11-11-15_30_55](https://user-images.githubusercontent.com/16477399/141315436-deebaf4e-2eda-4366-bc2b-a835c74249f0.png)

## FAQ

### My Gitlab group has subgroups. Will the pipelines of projects in there be included?

Yes. Projects in subgroups will be included.
At the moment the status page will show a flat list of pipeline statuses.

### Can I do horizontal scaling by using multiple instances?

Yes. Be aware, though, there's no shared persistence between the instances.
Each instance will fetch all the data it needs and will store it in memory.
So it's not efficient, but the different instances won't step on each other's toes (up until you reach the point where you're essentially DDoS'ing your Gitlab instance).  
I don't think it will be necessary to run more than one instance.
In my experience the performance of a single instance and the available possibilities of vertical scaling is more than enough for medium-sized teams with a large number of projects.
If you do encounter scalability issues, feel free to open an issue and let me know!

### How is it possible for a project to have no default branch?

The corresponding [API docs](https://docs.gitlab.com/ee/api/projects.html#list-all-projects) don't say that, but if a project is empty (e.g. if it was just created) it doesn't have a default branch.
A project might also contain no repository if it just contains issues or a Wiki, see [#223](https://github.com/L7R7/gitlab-ci-build-statuses/issues/223).

### Why do you need an extra call to get the single pipeline to determine the build status?

When I built this, I found out that the API is not always ideal for my needs.
In my team, we're using jobs that are allowed to fail (e.g. because they require manual steps, or just run some checks that shouldn't break the pipeline).
I'd like to make it clear on the status page if a pipeline was successful or if it ended with warnings.
However, if a pipeline fails with warnings the status that is returned by the [List Project Pipelines](https://docs.gitlab.com/ee/api/pipelines.html#list-project-pipelines) endpoint will be `success`.
To get the exact status, a request to the [Single Pipeline](https://docs.gitlab.com/ee/api/pipelines.html#get-a-single-pipeline) Endpoint is necessary for a pipeline that seems to be successful.  
There are two open issues that address this inconsistency in the API (see [here](https://gitlab.com/gitlab-org/gitlab/-/issues/323025) and [here](https://gitlab.com/gitlab-org/gitlab/-/issues/229137)).

### Isn't this thing slightly over-engineered?

Yes. It is.  
To be honest, the intention of this project never was to build something that's the ideal, minimal solution for the problem.
I was looking for something to build with Haskell to see if I had learned enough to get this working.
I learned a lot while building this, and that's what is important to me in this case.  
More on the reason behind the technical details below.

## Technical considerations

Here are the answers to the questions you might not have known you wanted to ask about the technical details of this project.
You won't need to know any of that to use this app.

### Persistence

I went with the simplest way of persisting the build statuses I could think of:
It's just an `IORef` that stores the information in memory.  
I wanted to have a persistence to avoid having to call the GitLab API whenever somebody wants to see the current pipeline statuses.
In addition to that, it was important to me to keep it self-contained, so something like a proper database was no option here (besides maybe SQLite).

### Frontend

My initial goal here was to have a lightweight UI that works without any JS.
The only real challenge was to solve the automatic refreshing, which I was able to solve without any JavaScript by using a `meta` HTML tag.
I like this solution because it couldn't be any simpler.

The only place that uses JS is the conversion of the timestamp of the last update into the user's browser timezone.
I was tired of converting from UTC in my head, so I added the conversion.
There was no other way to do that conversion with reasonable effort, so I went for the eleven lines of JS.
Fortunately, the UI is still fully functional with JS disabled, as the timestamp will be shown in UTC.

### polysemy

[Polysemy](https://hackage.haskell.org/package/polysemy) is one of the effect system implementations in Haskell.
It describes itself as being "a library for writing high-power, low-boilerplate domain specific languages" based on "Higher-order, low-boilerplate free monads".
This project is the one where I first tried polysemy in a way that's beyond a simple example.
I think it's a cool way to abstract over (side) effects in your program, and it helps to switch implementations for testing (e.g. to provide a fixed clock in tests and a "real" one in production).
In addition to that, I find it makes it easier to see what a function does:
If it doesn't have a dependency on the `Time` effect, it can't access the clock.

If you're interested in why polysemy might be a good idea, I can recommend [this talk](https://www.youtube.com/watch?v=kIwd1D9m1gE) as a starter.

**Do we need that here?** Probably not.
The complexity of this application is low enough to manage it without the help of an effect system.
Also, one needs to be aware that polysemy comes at a cost:
For most people, it's a rather steep learning curve and there is a number of people that say it's not worth the effort.

### servant

[Servant](https://docs.servant.dev/) is one of the most fascinating libraries in Haskell.
The idea is to have a type-safe web API using some advanced features of the Haskell type system.
While I wouldn't consider myself an expert in Haskell's type system, I found this library to be well-documented and easy-to-use, at least for the simple API in this project.  
Since the API is described as a Haskell type, it's well-structured and allows for very interesting things like automated generation of client functions, documentation, OpenAPI spec definitions and mock servers.
This makes the library very interesting and made me want to try it.

**Do we need that here?** The API is probably simple enough to stay on top of it without the help of a type-level API definition.
Nevertheless, it's not too much overhead to argue that it's overkill.
I'd choose it again, even for simple projects.

### Hexagonal Architecture

I'm a big fan of this architecture pattern, and I think it works especially well combined with pure functional programming, as described [here](https://www.youtube.com/watch?v=US8QG9I1XW0) by Mark Seemann.
I wanted to give it a try and implement this in Haskell.
My implementation is inspired by [this repository](https://github.com/thma/PolysemyCleanArchitecture), which also has a nice explanation of how it connects to polysemy.

**Do we need that here?** No, absolutely not.
In a more real world scenario (read: a scenario where a team of people is working on the codebase and is required to make money with it) I would argue that it's overkill and a more simple architecture is the way to go because there's not a lot of
domain logic going on that is worth protecting.  
In hindsight, it wasn't a good idea and not a good example to demonstrate the power of hexagonal architecture.
I might do a refactoring to a simpler approach at some point in the future.

### logging

I was looking for a way to implement [structured logging](https://www.innoq.com/en/blog/structured-logging/) with JSON output and ended up using [katip](https://hackage.haskell.org/package/katip) with a self written effect and interpretation.
Ideally, there would be a library that provides all that functionality out of the box, but I haven't found anything that was a good fit for me.

**Do we need that here?** Yes. Logging is important, and structured logging allows me to add more information to the logs that help understand what's going on.

### Configuration parsing using higher kinded data types

I first heard of this concept in [this talk](https://www.youtube.com/watch?v=sIqZEmnFer8) by Chris Penner.
He also wrote [a blog post](https://chrispenner.ca/posts/hkd-options) about the idea, which I used as a starting point for my own implementation.
I dedicated a whole [thread on Twitter](https://twitter.com/l7r7_/status/1399389526791950337) describing my solution.

**Do we need that here?** I'm not really sure.
This approach really is fascinating, but it also comes at a cost.
Thinking in terms of higher-kinded datatypes and working with higgledy/barbies means a comparatively steep learning curve.  
On the other hand, this approach has some nice advantages:

* It's really easy to read config from multiple sources and combining them in a "first come, first serve" way.
* Error reporting is great because it not only accumulates all errors, but it also includes a nice description for each error.
* Since I put the information for each concern (name of the environment variables, parsing function, error message, default value) in a dedicated HKD, it's easy to keep track of everything because stuff is not scattered over different places.
* If I add a new field to the config, the compiler will remind me of the places where I need to add something. It's impossible to forget something.
* All the complex stuff can be moved into a separate module separate from the application specific parts.
So the use side can use it without knowing how barbies/higgledy work in detail.
I think it's a good idea to put the generic parts into a dedicated library, and it's entirely possible that I'll give it a try at some point.

Now, in this particular case here I think it's a nice solution for config parsing, but not the most efficient one.
A more efficient approach could be to just use [envy](https://hackage.haskell.org/package/envy).
