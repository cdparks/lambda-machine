module Backend.Settings
  ( Settings(..)
  , HasSettings(..)
  , load
  , parseSettings
  , parsePostgresConf
  ) where

import Backend.Prelude

import Backend.Database (ConnectionString, PostgresConf(..))
import Env (Error, Parser, header, parse)
import qualified Env

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
load = parse (header "backend") parseSettings

parseSettings :: Parser Error Settings
parseSettings =
  Settings
    <$> Env.var Env.auto "PORT" (Env.def 3000)
    <*> Env.var Env.str "ROOT" (Env.def "http://api.localhost.com:3000")
    <*> Env.var Env.str "ORIGIN" (Env.def "http://localhost.com:1234")
    <*> Env.var Env.auto "LOG_LEVEL" (Env.def LevelInfo)
    <*> Env.var Env.auto "TIMEOUT" (Env.def 20)
    <*> parsePostgresConf

parsePostgresConf :: Parser Error PostgresConf
parsePostgresConf =
  PostgresConf
    <$> Env.var Env.str "DATABASE_URL" (Env.def defaultUrl)
    <*> Env.var Env.auto "PGPOOLSTRIPES" (Env.def 1)
    <*> Env.var Env.auto "PGPOOLIDLETIMEOUT" (Env.def 20)
    <*> Env.var Env.auto "PGPOOLSIZE" (Env.def 10)

defaultUrl :: ConnectionString
defaultUrl = "postgres://postgres:password@localhost:5432/lambda"
