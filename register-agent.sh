#!/usr/bin/env bash

curl --request POST \
 --url 'https://api.spacetraders.io/v2/register' \
 --header 'Content-Type: application/json' \
 --data '{
    "symbol": "DRFOGOUT",
    "faction": "COSMIC"
   }'
