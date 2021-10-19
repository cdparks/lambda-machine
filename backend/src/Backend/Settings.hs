module Backend.Settings
  ( Settings(..)
  , HasSettings(..)
  , load
  , parseSettings
  , parsePostgresConf
  ) where

import Backend.Prelude

import Backend.Database (ConnectionString, PostgresConf(..))
import Env

data Settings = Settings
  { port :: Int
  , root :: Text
  , origin :: Text
  , logLevel :: LogLevel
  , timeout :: Int
  , postgresConf :: PostgresConf
  }
  deriving stock Show

class HasSettings env where
  settingsLens :: Lens' env Settings

instance HasSettings Settings where
  settingsLens = id
  {-# INLINE settingsLens #-}

load :: IO Settings
load = parse (header "store") parseSettings

parseSettings :: Parser Error Settings
parseSettings =
  Settings
    <$> var auto "PORT" (def 3000)
    <*> var str "ROOT" (def "http://api.localhost.com:3000")
    <*> var str "ORIGIN" (def "http://localhost.com:1234")
    <*> var auto "LOG_LEVEL" (def LevelInfo)
    <*> var auto "TIMEOUT" (def 20)
    <*> parsePostgresConf

parsePostgresConf :: Parser Error PostgresConf
parsePostgresConf =
  PostgresConf
    <$> var str "DATABASE_URL" (def defaultUrl)
    <*> var auto "PGPOOLSTRIPES" (def 1)
    <*> var auto "PGPOOLIDLETIMEOUT" (def 20)
    <*> var auto "PGPOOLSIZE" (def 10)

defaultUrl :: ConnectionString
defaultUrl = "postgres://postgres:password@localhost:5432/lambda"
