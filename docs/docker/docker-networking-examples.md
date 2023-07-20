---
title: Networking Examples :: Docker
description: Some practical examples of working with, inspecting and debugging Docker networking
---

# Networking Examples

## Dockerfile

Create a `Dockerfile` to build an image derived from `nginx` official image and install the `ping` command line utility:

```yaml
FROM nginx:latest

##
# Directory inside the container.
#
# WORKDIR /home/node/app

# COPY app /home/node/app

RUN apt update
RUN apt install inetutils-ping --yes

##
# CMD executes when we actually run an instance of the
# container from that image.
#
# Executed when we do `docker run ...`.
#
CMD ["nginx", "-g", "daemon off;"]
```
