#!/bin/bash

set -e

IMAGE_NAME=$1
DOCKERFILE_PATH=$2

if [ "${IMAGE_NAME}" = "" ]; then
	echo "usage: $0 image_name [dockerfile]"
	exit 1
fi

if [ "${DOCKERFILE_PATH}" = "" ]; then
	DOCKERFILE_PATH="${IMAGE_NAME}"
fi

if [ "${DOCKER_REGISTRY}" = "" ]; then
	echo "DOCKER_REGISTRY environment variable is unset."
	echo "Not running inside GitHub Actions or misconfigured?"
	exit 1
fi

DOCKER_CONTAINER="${GITHUB_REPOSITORY}/${IMAGE_NAME}"
DOCKER_REGISTRY_CONTAINER="${DOCKER_REGISTRY}/${DOCKER_CONTAINER}"

echo "dockerfile=${DOCKERFILE_PATH}" >> $GITHUB_ENV
echo "docker-container=${DOCKER_CONTAINER}" >> $GITHUB_ENV
echo "docker-registry-container=${DOCKER_REGISTRY_CONTAINER}" >> $GITHUB_ENV

# Identify the last git commit that touched the Dockerfiles
# Use this as a hash to identify the resulting docker containers
DOCKER_SHA=$(git log -1 --pretty=format:"%h" -- "${DOCKERFILE_PATH}")
echo "docker-sha=${DOCKER_SHA}" >> $GITHUB_ENV

DOCKER_REGISTRY_CONTAINER_SHA="${DOCKER_REGISTRY_CONTAINER}:${DOCKER_SHA}"

echo "docker-registry-container-sha=${DOCKER_REGISTRY_CONTAINER_SHA}" >> $GITHUB_ENV
echo "docker-registry-container-latest=${DOCKER_REGISTRY_CONTAINER}:latest" >> $GITHUB_ENV

echo "::: logging in to ${DOCKER_REGISTRY} as ${GITHUB_ACTOR}"

exists="true"
docker login https://${DOCKER_REGISTRY} -u ${GITHUB_ACTOR} -p ${GITHUB_TOKEN} || exists="false"

echo "::: pulling ${DOCKER_REGISTRY_CONTAINER_SHA}"

if [ "${exists}" != "false" ]; then
	docker pull ${DOCKER_REGISTRY_CONTAINER_SHA} || exists="false"
fi

if [ "${exists}" = "true" ]; then
	echo "docker-container-exists=true" >> $GITHUB_ENV
else
	echo "docker-container-exists=false" >> $GITHUB_ENV
fi
