# FUNKCIONALNO-PROJEKAT

Prikaz web-aplikacije koju smo raditi za projekat iz Funkcionalnog programiranja.

![alt Web Application Image](https://github.com/suncanjenamesecini/FUNKCIONALNO-PROJEKAT/blob/main/slike/aplikacija_za_funkcionalno.PNG?raw=true)

Prvi dio za server koji je radjen u Haskelu je preuzet i prilagodjen sa:  [https://github.com/MondayMorningHaskell/RealWorldHaskell#readme](https://github.com/MondayMorningHaskell/RealWorldHaskell#readme). Ostavljeni su dijelovi koje sam koristila kao primjer i osnovu, a to su prvi, drugi i peti dio, a proširen je tekst u nekim dijelovima, jer je i moj kod opširniji od ovog koji je korišten kao polzna tačka i referenca. Zahvaljujem se prema tome autorima i toplo preporučujem njihov blog, gdje je sve još detaljnije objašnjeno [Real World Haskell](https://www.mmhaskell.com/real-world)

## Requirements

### Postgresql

Building this code requires that you have Postgres installed on your system. If you run `stack build` and see the following error message, this indicates that you do not have Postgres:

```bash
>> stack build
setup: The program 'pg_config' is required but it could not be found
```

On Linux, you'll want at least the following packages:

```bash
>> sudo apt install postgresql postgresql-contrib libpq-dev
```

On Windows and MacOS, you should be able to use the [downloads here](https://postgresql.org/download).

## Running the Code

### [Part 1: Persistent](https://www.mmhaskell.com/real-world/databases) 

The code for this part can be run pretty easily through GHCI. The main thing is you need your Postgres server to be up
and running. You can modify the [`localConnString` variable](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/Database.hs#L17) in the code matches up with the settings you used. 
The default we use is that the username, DB name, and password are all "postgres":

```haskell
localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"
```

Then once you load the code in GHCI, you should use the `migrateDB` expression. This will migrate your Postgres database
so that it contains the `users` table specified in our schema! Note how you can also set your connection string here as well if it's different from our built-in.

```bash
>> stack ghci
>> :l
-- (Removes all modules so there are no name conflicts)
>> import Database
>> let localConnString' = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres" :: PGInfo
>> migrateDB localConnString'
```
Then you'll be able to start running queries using the other functions in the `Database` module

```bash
>> let u = User "Kristina" "kristina@gmail.com" 45 "Software Engineer"
>> createUserPG localConnString u
1
>> fetchUserPG localConnString 1
Just (User {userName = "Kristina", userEmail = "kristina@gmail.com", userAge = 45, userOccupation = "Software Engineer"})
>> deleteUserPG localConnString 1
```

After each step, you can also check your Postgres database to verify that the queries went through! You can bring up
a Postgres query terminal with the `psql` command. You can use the `-U` argument to pass your username. Then enter
your password. And finally, you can connect to a different database with `\c`.

```
>> psql -U postgres
(enter password)
>> \c postgres
>> select * from users;
(See the users you've created!)
```

### [Part 2: Servant](https://www.mmhaskell.com/real-world/servant)

In this second part, we make a very basic server to expose the information in our database. Take a look at the source
[in this module](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/BasicServer.hs).

To run this server, first make your database is migrated, if you didn't do that in part 1:

```bash
>> stack exec migrate-db
```

Then you can run the server with this executable:

```bash
>> stack exec run-server
```

Now you can make HTTP requests to your server from any client program. My favorite is [Postman](https://postman.com).
Then you can follow the same pattern you did in the first part. Try creating a user:

```bash
POST /users
{
  "name": "Kristina",
  "email": "kristina@gmail.com",
  "age": 45,
  "occupation": "Software Engineer"
}

...

2
```

Then try fetching it:

```bash
GET /users/2

...

{
  "name": "Kristina",
  "email": "kristina@gmail.com",
  "age": 45,
  "occupation": "Software Engineer"
}
```

You can also try fetching invalid users!

```bash
GET /users/45

...

Could not find user with that ID
```

### [Part 5: Esqueleto](https://www.mmhaskell.com/real-world/esqueleto)

In part 5, we add a new type to our schema, this time incorporating a foreign key relation. This part has its own
distinct set of modules to avoid conflicts with the code from the first 4 parts:

1. [New Schema Module](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/SchemaEsq.hs)
2. [New Database Library](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/DatabaseEsq.hs)
3. [Updated Server](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/ServerEsq.hs) (this server does not have any Redis caching)
4. [Sample Objects](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/SampleObjects.hs) (for database insertion)

To try out this code, you should start by running a new migration on your database:

```bash
>> stack exec migrate-db -- esq
```

This will add the `articles` table, but it should leave the `users` table unaffected, so it shouldn't cause any
problems with your original code.

Next you can update the database in a couple different ways. First, as with the first part, you can open up GHCI and
try running the insertions for yourself. In the `SampleObjects` module, we've provided a set of objects you can use
as sample database items. The `User` objects are fine on their own, but the `Article` objects require you to pass in
the integer ID of the User after they've been created. For example:

```bash
>> stack ghci
>> :l
>> :load DatabaseEsq SampleObjects
>> import SampleObjects
>> createUserPG localConnString testUser1
5
>> createArticlePG localConnString (testArticle1 5)
1
>> fetchRecentArticles
[(Entity {entityKey = SqlBackendKey 5, entityVal = User {...}}, Entity {entityKey = SqlBackendKey 1, entityVal = Article {...}})]
```

The other way to do this is to use the API via the server. Start by running the updated server:

```bash
>> stack exec run-server -- esq
```

And then you can make your requests, via Postman or whatever service you use:

```bash
POST /users
{
  "name": "Kristina",
  "email": "kristina@gmail.com",
  "age": 45,
  "occupation": "Software Engineer"
}

5

POST /articles

{
  "title": "First Post",
  "body": "A great description of our first blog post body.",
  "publishedTime": 1498914000,
  "authorId": 5
}

1

GET /articles/recent
[
  [
    {
      "name": "Kristina",
      "email": "kristina@gmail.com",
      "age": 45,
      "occupation": "Software Engineer",
      "id": 5
    },
    {
      "title": "First Post",
      "body": "A great description of our first blog post body.",
      "publishedTime": 1498914000,
      "authorId": 5,
      "id": 1
    }
  ]
]
```

