# arbital
Capture the state of the debate. 

# Installation
## Postgres configuration
Install requirements
```bash
$ sudo apt-get update
$ sudo apt-get install postgresql postgresql-contrib
```
Set per [this](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-16-04)

Configure db. Change the user/password credentials in `backend/src/Arbital/Constants.hs` to match current user.
```bash
$ psql
user=# CREATE DATABASE arbital;
user=# \c arbital
arbital=# CREATE EXTENSION pg_trgm;
```
## Backend
Make sure you have postgres installed, and a database called `arbital`.
```bash
$ cd backend
$ stack install
$ stack exec arbital-exe
```
## Frontend
```bash
$ cd frontend
$ npm install
$ npm start
```
