#!/bin/bash
docker run -d -p 5432:5432 --name timesheet-db -e POSTGRES_PASSWORD=mysecretpassword postgres

