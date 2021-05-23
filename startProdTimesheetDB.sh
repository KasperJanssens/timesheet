#!/bin/bash
docker run -d -p 9875:5432 --name timesheet-db-prod -e POSTGRES_PASSWORD=mysecretpassword postgres

