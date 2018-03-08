#!/bin/bash

curl -H "Content-Type: application/json" -X POST -d "\"$1\"" http://localhost:8081/submit
echo 
