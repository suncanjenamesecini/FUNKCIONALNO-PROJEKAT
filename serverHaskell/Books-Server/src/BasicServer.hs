{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
--import           Network.Wai.Middleware.Cors  --NISAM USPJELA DA PODESIM DA RADI OVO, TAKO DA SE MORA KLIJENT OTVARATI U BROWSERU SA ISKLJUCENIM SIGURNOSNIM FLAG-OVIMA
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchBookPG, fetchAllBooksPG, fetchAllBooksForLanguagePG, fetchRecentBooksPG, fetchPrekoJoinaPG, createLanguagePG, createBookPG, deleteBookPG, localConnString)
import           BasicSchema


type BooksAPI = 
       "books" :> Capture "bookid" Int64 :> Get '[JSON] Book
  :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Int64
  :<|> "books" :> "sve" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "elm" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "haskell" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "elixir" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "fsharp" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "clojure" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "ocaml" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "scala" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "racket" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "scheme" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "lisp" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "ml" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "apl" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "miranda" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "agda" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "erlang" :> Get '[JSON] [Entity Book]
  :<|> "books" :> "joinJezik" :> Get '[JSON] [(Entity Language , Entity Book)]
  :<|> "books" :> "join" :> Get '[JSON] [Entity Book]
  :<|> "books" :> Capture "bookid" Int64 :> Post '[JSON] () --Delete ZASAD
--  :<|> "languages" :> ReqBody '[JSON] Book :> Post '[JSON] Int64  --insert jezik, nesto ne radi


booksAPI :: Proxy BooksAPI
booksAPI = Proxy :: Proxy BooksAPI

fetchBooksHandler :: ConnectionString -> Int64 -> Handler Book
fetchBooksHandler connString bid = do
  maybeBook <- liftIO $ fetchBookPG connString bid
  case maybeBook of
    Just book -> return book
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find book with that ID" })


fetchAllBooksElmHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksElmHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 1

fetchAllBooksHaskellHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksHaskellHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 2

fetchAllBooksElixirHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksElixirHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 3

fetchAllBooksFsharpHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksFsharpHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 4

fetchAllBooksClojureHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksClojureHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 5

fetchAllBooksOcamlHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksOcamlHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 6

fetchAllBooksScalaHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksScalaHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 7

fetchAllBooksRacketHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksRacketHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 8

fetchAllBooksSchemeHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksSchemeHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 9

fetchAllBooksLispHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksLispHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 10

fetchAllBooksMlHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksMlHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 11

fetchAllBooksAplHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksAplHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 12

fetchAllBooksMirandaHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksMirandaHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 13

fetchAllBooksAgdaHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksAgdaHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 14

fetchAllBooksErlangHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksErlangHandler connString = liftIO $ fetchAllBooksForLanguagePG connString 15

fetchAllBooksHandler :: ConnectionString -> Handler [Entity Book]
fetchAllBooksHandler connString = liftIO $ fetchAllBooksPG connString

createBookHandler :: ConnectionString -> Book -> Handler Int64
createBookHandler connString book = liftIO $ createBookPG connString book

createLanguageHandler :: ConnectionString -> Language -> Handler Int64
createLanguageHandler connString language = liftIO $ createLanguagePG connString language

fetchRecentBooksHandler :: ConnectionString -> Handler [(Entity Language , Entity Book)]
fetchRecentBooksHandler connString = liftIO $ fetchRecentBooksPG localConnString 

fetchPrekoJoinaHandler :: ConnectionString -> Handler [Entity Book]
fetchPrekoJoinaHandler connString = liftIO $ fetchPrekoJoinaPG localConnString

deleteBookHandler :: ConnectionString -> Int64 -> Handler ()
deleteBookHandler connString bid = liftIO $ deleteBookPG connString bid

booksServer :: ConnectionString -> Server BooksAPI
booksServer connString = 
  (fetchBooksHandler connString) :<|> 
  (createBookHandler connString) :<|>
  (fetchAllBooksHandler connString) :<|>
  (fetchAllBooksElmHandler connString) :<|>
  (fetchAllBooksHaskellHandler connString) :<|>
  (fetchAllBooksElixirHandler connString) :<|>
  (fetchAllBooksFsharpHandler connString) :<|>
  (fetchAllBooksClojureHandler connString) :<|>
  (fetchAllBooksOcamlHandler connString) :<|>
  (fetchAllBooksScalaHandler connString) :<|>
  (fetchAllBooksRacketHandler connString) :<|>
  (fetchAllBooksSchemeHandler connString) :<|>
  (fetchAllBooksLispHandler connString) :<|>
  (fetchAllBooksMlHandler connString) :<|>
  (fetchAllBooksAplHandler connString) :<|>
  (fetchAllBooksMirandaHandler connString) :<|>
  (fetchAllBooksAgdaHandler connString) :<|>
  (fetchAllBooksErlangHandler connString) :<|>
  (fetchRecentBooksHandler connString) :<|>
  (fetchPrekoJoinaHandler connString) :<|>
  (deleteBookHandler connString)
--  (createLanguageHandler connString)

runServer :: IO ()
runServer = run 5000 (serve booksAPI (booksServer localConnString))

