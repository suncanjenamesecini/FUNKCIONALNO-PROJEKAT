# FUNKCIONALNO-PROJEKAT

Ovo je prikaz i opis web-aplikacije koju smo radili za projekat iz Funkcionalnog programiranja. Zadatak nam je bio realizovati web-aplikaciju u čisto funkcionalnim jezicima, a prvenstveno u Haskelu, koji smo radili i na nastavi. Ova aplikacija se sastoji iz dva dijela, klijent i server, koji komuniciraju preko http-zahtjeva i odgovora, a pri tom podaci su uglavnom prenošeni u obliku JSON-a, u body elementu http-zahtjeva ili odgovora. Klijent je realizovan u jeziku za frontend pod nazivom Elm, o kojem ce biti više riječi kasnije. Server je realizovan u Haskellu, koristeći PostgreSQL bazu podataka i Servant biblioteku za izradu servera i API-ja. Za konekciju sa bazom koristi se biblioteka Persistent, koja omogućava ORM (object-relational mapping), tj. ekvivalentan koncept u funkcionalnim jezicima. Ta biblioteka takođe omogućava jednostavnije upite u obliku funkcija kao što su get, insert, delete, čijom se upotrebom smanjuje potreba za pisanjem čistog SQL koda od strane programera, pa prema tome se smanjuje mogućnost napada kao što su SQL injection. Druga biblioteka koričćena za pisanje upita je Esqueleto. Ona omogućava pisanje složenijih upita kao što su JOIN-i dvije ili više tabela, WHERE upiti i slično.

Ove tehnologije i struktura aplikacije su izabrani zbog iskustva u izradi web-aplikacija kod kojih je React korišćen za frontend, a Node.js za backend; frontend je prema tome SPA (single-page application), a backend je RESTful API. Izradom ovog projekta se uočavaju prednosti i mane korišćenja funkcionalnih jezika, dok je sama arhitektura aplikacije ista kao u standardnim PERN aplikacijama (PostgreSQL+Express+React+Node.js).

Aplikacija se zove `Katalog knjiga o funkcionalnim jezicima` i pruža mogućnost prikaza velikog broja knjiga koje se bave funkcionalnim programskim jzicima.  Klijent nakon pokretanja izgleda kao na narednoj slici, gdje se na stranici ispod forme prikaže red dugmadi za filtriranje knjiga po kategorijama. Prvo dugme u redu postiže isti efekat kao pokretanje aplikacije, a to je prikaz svih knjiga u nizu kartica, dok se na svaku karticu može kliknuti, što nas vodi na Google Books stranicu namijenjenu toj knjizi. Ostala dugmad filtriraju kartice, tako da se prikažu knjie po jezicima, dok posljednje dugme vraća prikaz 10 najobimnijih knjiga u bazi i postoji prvenstveno da bi se demonstrirala upotreba složenijih upita, kao što su JOIN dvije tabele i sotiranje. Forma omogućuje unos podataka o novoj knjizi koja se klikom na dugme Add Book dodaje u bazu, kao i brisanje knjige sa unijetim indeksom, ako ona postoji u bazi, ili pak selektovanje i prikaz jedne knjige koja odgovara unijetom indeksu.

![alt Web Application Image](https://github.com/suncanjenamesecini/FUNKCIONALNO-PROJEKAT/blob/main/slike/aplikacija_za_funkcionalno.PNG?raw=true)

Naredni dio ovog teksta, za server koji je rađen u Haskelu, je preuzet i prilagodjen sa:  [https://github.com/MondayMorningHaskell/RealWorldHaskell#readme](https://github.com/MondayMorningHaskell/RealWorldHaskell#readme). Ostavljeni su dijelovi koji su korišćeni kao primjer i osnova, a to su prvi, drugi i peti dio, a proširen je tekst u nekim dijelovima, jer je i kod aplikacije opširniji od ovog koji je služio  kao polazna tačka i referenca. Prema tome, preporuke za blog autora, gdje je sve još detaljnije objašnjeno [Real World Haskell](https://www.mmhaskell.com/real-world)

## Instalacija neophodnog

### Postgresql

Pokretanje aplikacije zahtijeva instalaciju Postgres-a. Ako se nakon lociranja u folder sa serverom pomoću komandi `cd serverHaskell`, `cd Books-Server` i izvršavanja naredbe `stack build` vidi sljedeća poruka o grešci, to je indikacija da Postgres nije instaliran ili nije dostupan:

```bash
>> stack build
setup: The program 'pg_config' is required but it could not be found
```

Na Linux operativnom sistemu su neophodni bar sljedeći paketi:

```bash
>> sudo apt install postgresql postgresql-contrib libpq-dev
```

Na Windows i MacOS operativnim sistemima -  [instalacija](https://postgresql.org/download).

U fajlu `Database.hs` se vide podaci za uspostavljanje konekcije s bazom; u našem slučaju je difoltni port za Postgres `5432`, naziv korisnika je `postgres`, naziv baze `probnaBaza`, a šifa je `postgres`, pa je neophodno podesiti ove podatke:

```haskell
localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=probnaBaza password=postgres"
```

## Pokretanje servera

Nakon što je Postgres instaliran i podešeni potrebni podaci, locirati se u folder sa serverom i kompajlirati:

```bash
>> stack build
```

Nakon toga komanda za migraciju baze:

```bash
>> stack exec migrate-db
```

Pa onda komanda za pokretanje servera na portu 5000 (Promijeniti port u fajlu BasicServer.hs ako ne odgovara):

```bash
>> stack exec run-server
```

U fajlu `BasicSchema.hs` se vidi definicija šeme baze:

```haskell
PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Language sql=languages
    name Text
    UniqueTitle name
    deriving Show Read Eq

  Book sql=books
    title Text
    thumbnail Text
    pages Int
    link Text
    publisher Text
    languageId LanguageId
    UniqueText title
    deriving Show Read Eq
|]
```

Postoje dvije tabele, languages i books; u prvoj je naziv jezika jedinstven, a id iz prve je strani ključ u drugoj, pod nazivom languageId. U drugoj tabeli je kolona title jedinstvena, a ostale kolone su thumbnail, pages, link i publisher. Tu se vidi definicija tipa Book i Language. Prikaz nekoliko relacija u tabelama preko `json` notacije i tabelarno:

```json
{
  "id":1,
  "name":"Elm"
}
```

```json
{
  "id":2,
  "name":"Haskell"
}
```

| id      | name      |
| ------- | --------- |
| 1       | Elm       |
| 2       | Haskell   |


Ovo je za tabelu languages, a za tabelu books sljedecih nekoliko primjera relacija predstavljenih `json` objektima i odgovarajući prikaz u tabeli:

```json
{"title":"Web Applications with Elm",
"thumbnail":"http://books.google.com/books/content?id=KnhqDwAAQBAJ&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api",
"pages":140,
"link":"https://books.google.com/books/about/Web_Applications_with_Elm.html?hl=&id=KnhqDwAAQBAJ",
"publisher":"Apress",
"languageId":1
}
```

```json
{"title":"Programming in Haskell",
"thumbnail":"http://books.google.com/books/content?id=75C5DAAAQBAJ&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api",
"pages":320,
"link":"https://books.google.com/books/about/Programming_in_Haskell.html?hl=&id=75C5DAAAQBAJ",
"publisher":"Cambridge University Press",
"languageId":2
}
```

```json
{"title":"Haskell in Depth",
"thumbnail":"http://books.google.com/books/content?id=r4UxEAAAQBAJ&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api",
"pages":664,
"link":"https://books.google.com/books/about/Haskell_in_Depth.html?hl=&id=r4UxEAAAQBAJ",
"publisher":"Simon and Schuster",
"languageId":2
}
```

|  id      | title      | thumbnail | pages   | link   | publisher  | languageId |
| --------- | ----------- | -------- | -------- | ------ | --------- | ---------- |
| 2   |  Web Applications with Elm  | http://books.google.com/books/content?id=KnhqDwAAQBAJ&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api  | 140  | https://books.google.com/books/about/Web_Applications_with_Elm.html?hl=&id=KnhqDwAAQBAJ    |  Apress     |  1    |
| 8   | Programming in Haskell   |  http://books.google.com/books/content?id=75C5DAAAQBAJ&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api   | 320  | https://books.google.com/books/about/Programming_in_Haskell.html?hl=&id=75C5DAAAQBAJ    | Cambridge University Press  | 2  |
| 9   | Haskell in Depth    | http://books.google.com/books/content?id=r4UxEAAAQBAJ&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api   | 664 | https://books.google.com/books/about/Haskell_in_Depth.html?hl=&id=r4UxEAAAQBAJ  |  Simon and Schuster  |  2  |


### [Konekcija sa bazom: Persistent](https://hackage.haskell.org/package/persistent)

Biblioteka za konekciju backend-a sa bazom je Persistent. Izabrana je zbog mnogih pozitivnih karakteristika, kao i broja preuzimanja paketa sa Hackage repozitorijuma. Nije neophodno da se koristi PostgreSQL baza uz biblioteku Persistent, već ona omogućava podršku za nekoliko najpoznatijih baza. Glavne osobine biblioteke prema autorima iste, preuzeto sa -  [Persistent :: Yesod Web Framework Book](https://www.yesodweb.com/book/persistent) :

> ...
> Persistent is Yesod’s answer to data storage- a type-safe, universal data store interface for Haskell.
>
> Haskell has many different database bindings available. However, most of these have little knowledge of a schema and therefore do not provide useful static guarantees. They also force database-dependent APIs and data types on the programmer.
> ...
> In contrast, Persistent allows us to choose among existing databases that are highly tuned for different data storage use cases, interoperate with other programming languages, and to use a safe and productive query interface, while still keeping the type safety of Haskell datatypes.
>
> Persistent follows the guiding principles of type safety and concise, declarative syntax. Some other nice features are:
>
>  - Database-agnostic. There is first class support for PostgreSQL, SQLite, MySQL and MongoDB, with experimental Redis support.
>
>  - Convenient data modeling. Persistent lets you model relationships and use them in type-safe ways. The default type-safe persistent API does not support joins, allowing support for a wider number of storage layers. Joins and other SQL specific functionality can be achieved through using a raw SQL layer (with very little type safety). An additional library, `Esqueleto`, builds on top of the Persistent data model, adding type-safe joins and SQL functionality.
> 
>  - Automatic database migrations in non-production environments to speed up development.
>
> Persistent works well with Yesod, but it is quite usable on its own as a standalone library.


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

