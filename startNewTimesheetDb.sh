#!/bin/bash
docker run -d -p 9874:5432 --name timesheet-db -e POSTGRES_PASSWORD=mysecretpassword postgres

