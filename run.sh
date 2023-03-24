#!/bin/bash
set -e
podman build --tag ladybug_test -f Dockerfile .
podman run --rm -it localhost/ladybug_test
