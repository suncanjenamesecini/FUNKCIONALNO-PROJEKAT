{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist hiding ((==.))
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)

import BasicSchema

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=probnaBaza password=postgres"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
   runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False


fetchBookPG :: PGInfo -> Int64 -> IO (Maybe Book)
fetchBookPG connString bid = runAction connString (get (toSqlKey bid))

--SELECT WHERE ZA ODREDJENI JEZIK:
fetchAllBooksForLanguagePG :: PGInfo -> Int64  -> IO [Entity Book]
fetchAllBooksForLanguagePG connString lid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Book]
    fetchAction = select . from $ \books2 -> do
      where_ (books2 ^. BookLanguageId ==. val (toSqlKey lid))
      return books2


fetchAllBooksPG :: PGInfo -> IO [Entity Book]
fetchAllBooksPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Book]
    fetchAction = select . from $ \books2 -> do
      --where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
      return books2

--PRIMJER JOINA DVE TABELE: --vraca se lista parova (jezik, knjiga) tako da to ne koristim u projektu, ali sam testirala preko Postmana i radi
fetchRecentBooksPG :: PGInfo -> IO [(Entity Language , Entity Book)]
fetchRecentBooksPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Language, Entity Book)]
    fetchAction = select . from $ \(languages `InnerJoin` books) -> do
      on (languages ^. LanguageId  ==. books ^. BookLanguageId)
      orderBy [desc (books ^. BookPages)]
      limit 10
      return (languages, books)

--PRIMJER JOINA DVE TABELE: --ne vracam tuple, tako da ovo koristim u projektu jer vracam listu knjiga kao i inace
fetchPrekoJoinaPG :: PGInfo -> IO [Entity Book]
fetchPrekoJoinaPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Book]
    fetchAction = select . from $ \(languages `InnerJoin` books) -> do
      on (languages ^. LanguageId  ==. books ^. BookLanguageId)
      orderBy [desc (books ^. BookPages)]
      limit 10
      return books

createBookPG :: PGInfo -> Book -> IO Int64 
createBookPG connString book = fromSqlKey <$> runAction connString (insert book)

createLanguagePG :: PGInfo -> Language  -> IO Int64
createLanguagePG connString language = fromSqlKey <$> runAction connString (insert language)


deleteBookPG :: PGInfo -> Int64 -> IO () --ZASTO JE TIPA IO (), SAD NEMAM NACIN DA SIGNALIZUJEM DA LI JE USPJESNO BRISANJE ILI JE NEUSPJESNO -- delete funkcija iz 
deleteBookPG connString bid = runAction connString (delete bookKey)
  where
    bookKey :: Key Book
    bookKey = toSqlKey bid
