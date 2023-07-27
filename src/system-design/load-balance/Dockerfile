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


