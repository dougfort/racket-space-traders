#!/usr/bin/env bash

curl 'https://api.spacetraders.io/v2/my/agent' \
 --header "Authorization: Bearer $1"
