{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Arbital.Database.Driver 
  ( 
  -- * Config
    Connection
  , DbSession
  , openConnection
  , closeConnection
  , runDb
  , dbResultErr 
  -- * SQL API
  , Field(..)
  , createTable
  , dropTable
  , select
  , selectAll
  , selectWhere1
  , insert
  , update
  , delete
  ) where

import           Data.Monoid
import           Data.Functor.Contravariant
import           Data.Time.Clock (UTCTime)
import           Data.Proxy
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Foldable (foldl')
import qualified Data.Aeson as A
import           Control.Monad (replicateM)
import           Control.Monad.Except (throwError)
import           Hasql.Connection
import           Hasql.Query
import           Hasql.Session
import qualified Hasql.Encoders as Enc
import qualified Hasql.Decoders as Dec

import           Arbital.Types hiding (Session)
import qualified Arbital.Types as Ty

-- * Config

type DbSession = Session

openConnection :: IO (Either ConnectionError Connection)
openConnection = acquire dbSettings 

closeConnection :: Connection -> IO ()
closeConnection = release

dbSettings :: Settings
dbSettings = settings "localhost" 5432 "anand" "anand" "arbital"

runDb :: Connection -> Session a -> IO (Either Error a)
runDb c s = run s c

dbResultErr :: Text -> Session a
dbResultErr = throwError . ResultError . UnexpectedResult 

-- * Persistent typeclass

class Persistent a where
  enc :: Enc.Params a
  dec :: Dec.Row a

class ValuePersistent a where
  encVal :: Enc.Value a
  decVal :: Dec.Value a

-- ** General

instance ValuePersistent UTCTime where
  encVal = Enc.timestamptz
  decVal = Dec.timestamptz

instance Persistent UTCTime where
  enc = Enc.value encVal
  dec = Dec.value decVal

instance ValuePersistent Text where
  encVal = Enc.text
  decVal = Dec.text

instance Persistent Text where
  enc = Enc.value encVal
  dec = Dec.value decVal

instance (ValuePersistent a) => ValuePersistent [a] where
  encVal = Enc.array (Enc.arrayDimension foldl' (Enc.arrayValue encVal))
  decVal = Dec.array (Dec.arrayDimension replicateM (Dec.arrayValue decVal))

instance (ValuePersistent a) => Persistent (Maybe a) where
  enc = Enc.nullableValue encVal
  dec = Dec.nullableValue decVal

-- ** Users

instance Persistent UserID where
  enc = contramap (\(UserID e) -> e) enc
  dec = UserID <$> dec

instance Persistent Email where
  enc = contramap (\(Email e) -> e) enc
  dec = Email <$> dec

instance Persistent Name where
  enc = contramap (\(Name e) -> e) enc
  dec = Name <$> dec

instance Persistent User where
  enc = 
        contramap userId enc
    <>  contramap userEmail enc
    <>  contramap userName enc
    <>  contramap userClaims (Enc.value encVal)
    <>  contramap userArguments (Enc.value encVal)
    <>  contramap registrationDate enc
  dec = User 
    <$> dec 
    <*> dec 
    <*> dec
    <*> Dec.value decVal
    <*> Dec.value decVal
    <*> dec

-- ** Sessions

instance Persistent Ty.SessionID where
  enc = contramap (\(Ty.SessionID s) -> s) enc
  dec = Ty.SessionID <$> dec

instance Persistent Ty.Session where
  enc =
        contramap Ty.sessionId enc
    <>  contramap Ty.sessionUser enc
    <>  contramap Ty.sessionCreated enc
    <>  contramap Ty.sessionLastUsed enc
  dec = Ty.Session
    <$> dec
    <*> dec 
    <*> dec
    <*> dec

-- ** Claims

instance Persistent ClaimID where
  enc = contramap (\(ClaimID c) -> c) enc
  dec = ClaimID <$> dec

instance ValuePersistent ClaimID where
  encVal = contramap (\(ClaimID c) -> c) encVal
  decVal = ClaimID <$> decVal

instance Persistent Claim where
  enc = 
        contramap claimId enc
    <>  contramap claimText enc
    <>  contramap argsFor (Enc.value encVal)
    <>  contramap argsAgainst (Enc.value encVal)
    <>  contramap claimAuthorId enc
    <>  contramap claimCreationDate enc
  dec = Claim
    <$> dec
    <*> dec
    <*> Dec.value decVal
    <*> Dec.value decVal
    <*> dec
    <*> dec

-- ** Arguments

instance Persistent ArgumentID where
  enc = contramap (\(ArgumentID a) -> a) enc
  dec = ArgumentID <$> dec

instance ValuePersistent ArgumentID where
  encVal = contramap (\(ArgumentID a) -> a) encVal
  decVal = ArgumentID <$> decVal

instance Persistent Argument where
  enc = 
        contramap argumentId enc
    <>  contramap argumentSummary enc
    <>  contramap argumentClaims (Enc.value encVal)
    <>  contramap argumentAuthorId enc
    <>  contramap argumentCreationDate enc
  dec = Argument 
    <$> dec
    <*> dec
    <*> Dec.value decVal
    <*> dec
    <*> dec

-- ** Commits

instance Persistent CommitID where
  enc = contramap (\(CommitID c) -> c) enc
  dec = CommitID <$> dec

instance Persistent Commit where
  enc = 
        contramap commitId enc
    <>  contramap commitAuthor enc
    <>  contramap commitAction enc
    <>  contramap commitCreationDate enc
    <>  contramap commitMessage enc
  dec = Commit
    <$> dec
    <*> dec
    <*> dec
    <*> dec
    <*> dec

-- #partial
instance Persistent CommitAction where
  enc = contramap A.toJSON (Enc.value Enc.json)
  dec = from <$> Dec.value Dec.json
    where
      from a = case A.fromJSON a of 
        A.Error e -> error $ "Persistent/CommitAction: " ++ e 
        A.Success s -> s

-- * PSQL types

data PSQLType 
  -- = PChar { size :: Int }
  -- | PVarChar { size :: Int }
  = PText
  -- | PBit { size :: Int }
  -- | PVarBit { size :: Int }
  | PSmallInt
  | PInt 
  | PInteger
  | PBigInt
  | PSmallSerial
  | PSerial
  | PBigSerial
  -- | PNumeric { totalDigits :: Int, decimalDigits :: Int }
  | PDouble
  | PReal
  | PMoney
  | PBool
  | PDate
  | PTimeStamp
  | PTimeStampTZ
  | PTime
  | PTimeTZ
  | PArray PSQLType
  | PJSON

psTypeShow :: PSQLType -> ByteString
psTypeShow = \case
  -- PChar s -> sized "char" s
  -- PVarChar s -> sized "varchar" s
  PText -> "text"
  -- PBit s -> sized "bit" s
  -- PVarBit s -> sized "varbit" s
  PSmallInt -> "smallint"
  PInt -> "int"
  PInteger -> "integer"
  PBigInt -> "bigint"
  PSmallSerial -> "smallserial"
  PSerial -> "serial"
  PBigSerial -> "bigserial"
  -- PNumeric m d -> "numeric(" <> tshow m <> "," <> tshow d <> ")" 
  PDouble -> "double precision"
  PReal -> "real"
  PMoney -> "money"
  PBool -> "bool"
  PDate -> "date"
  PTimeStamp -> "timestamp"
  PTimeStampTZ -> "timestamptz"
  PTime -> "time"
  PTimeTZ -> "timetz"
  PArray p -> psTypeShow p <> "[]"
  PJSON -> "json"
  where
    -- tshow = BC.pack . show
    -- sized tag s = tag <> "(" <> tshow s <> ")" 

data Column = Column { columnName :: ByteString, columnType :: PSQLType }

data Schema = Schema 
  { idColumn :: Column
  , restColumns :: [Column]
  }

data Field a = Field { fieldName :: ByteString, fieldValue :: a }

showSchema :: Schema -> ByteString
showSchema s = B.intercalate "," (map showColumn $ idColumn s : restColumns s)

showColumn :: Column -> ByteString
showColumn f = columnName f <> " " <> psTypeShow (columnType f)

-- param :: Int -> ByteString
-- param n = "$" <> BC.pack (show n)

-- * SQL API

class HasTable a where 
  type Id a 
  tableName :: Proxy a -> ByteString
  tableSchema :: Proxy a -> Schema 

-- | The first Column is assumed to be the ID Column.
idField :: (HasTable a) => Proxy a -> ByteString
idField = columnName . idColumn . tableSchema

createTable :: (HasTable a) => Proxy a -> Session ()
createTable p = 
  sql $ 
       "CREATE TABLE " 
    <> tableName p 
    <> " (" 
    <> showSchema (tableSchema p) 
    <> ")"

dropTable :: (HasTable a) => Proxy a -> Session ()
dropTable p = sql $ "DROP TABLE " <> tableName p

select :: (HasTable a, Persistent (Id a), Persistent a) => Proxy a -> Id a -> Session (Maybe a)
select p i = query i q
  where
    q = statement cmd encoder decoder True
    cmd =
      "SELECT * FROM " <> tableName p <> " WHERE " <> idField p <> " = $1"
    encoder = enc
    decoder = Dec.maybeRow dec

selectAll :: (HasTable a, Persistent a) => Proxy a -> Session [a]
selectAll p = query () q
  where 
    q = statement cmd encoder decoder True
    cmd = "SELECT * FROM " <> tableName p
    encoder = Enc.unit
    decoder = Dec.rowsList dec 

selectWhere1 :: (HasTable a, Persistent a, Persistent b) => Proxy a -> Field b -> Session (Maybe a)
selectWhere1 p f = query (fieldValue f) q
  where
    q = statement cmd encoder decoder True
    cmd =
      "SELECT * FROM " <> tableName p <> " WHERE " <> fieldName f <> " = $1"
    encoder = enc
    decoder = Dec.maybeRow dec

insert :: (HasTable a, Persistent a) => Proxy a -> a -> Session ()
insert p a = query a q
  where
    q = statement cmd encoder decoder True
    cmd = 
      "INSERT INTO " <> tableName p <> " VALUES ($1)"
    encoder = enc
    decoder = Dec.unit

update :: (HasTable a, Persistent (Id a), Persistent a) => Proxy a -> Id a -> (a -> a) -> Session ()
update p i f = do
  ma_ <- select p i
  case ma_ of 
    Nothing -> dbResultErr "update: value not found"
    Just a_ -> do
      delete p i
      insert p (f a_)

delete :: (HasTable a, Persistent (Id a)) => Proxy a -> Id a -> Session ()
delete p i = query i q
  where
    q = statement cmd encoder decoder True
    cmd = 
      "DELETE FROM " <> tableName p <> " WHERE " <> idField p <> " = $1"
    encoder = enc
    decoder = Dec.unit

-- * SQL instances 

instance HasTable User where
  type Id User = UserID
  tableName _ = "users"
  tableSchema _ = Schema
    { idColumn = Column "id" PText
    , restColumns = 
      [ Column "email" PText
      , Column "name" PText
      , Column "claims" (PArray PText)
      , Column "arguments" (PArray PText)
      , Column "registrationDate" PTimeStampTZ
      ]
    }

instance HasTable Claim where
  type Id Claim = ClaimID
  tableName _ = "claims"
  tableSchema _ = Schema
    { idColumn = Column "id" PText
    , restColumns = 
      [ Column "text" PText
      , Column "argsFor" (PArray PText)
      , Column "argsAgainst" (PArray PText)
      , Column "authorId" PText
      , Column "creationDate" PTimeStampTZ
      ]
    }

instance HasTable Argument where
  type Id Argument = ArgumentID
  tableName _ = "arguments"
  tableSchema _ = Schema
    { idColumn = Column "id" PText
    , restColumns = 
      [ Column "text" PText
      , Column "claims" (PArray PText)
      , Column "authorId" PText
      , Column "creationDate" PTimeStampTZ
      ]
    }

instance HasTable Commit where
  type Id Commit = CommitID
  tableName _ = "commits"
  tableSchema _ = Schema
    { idColumn = Column "id" PText
    , restColumns = 
      [ Column "authorId" PText
      , Column "action" PJSON
      , Column "creationDate" PTimeStampTZ
      , Column "message" PText
      ]
    }
