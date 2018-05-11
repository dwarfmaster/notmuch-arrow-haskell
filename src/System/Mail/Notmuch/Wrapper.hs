{-# LANGUAGE TemplateHaskell #-}

module System.Mail.Notmuch.Wrapper
       ( ErrorCode(..), errorCodeMessage, statusToErrorCode
       , Database(..), getDatabasePath, makeDatabase, withCDatabase
       , DatabaseMode(..), cdatabaseToDatabaseMode, databaseToCDatabaseMode
       , Query(..)      , makeGBQuery    , withCQuery
       , Threads(..)    , makeThreads    , withCThreads
       , Thread(..)     , makeThread     , withCThread
       , Messages(..)   , makeMessages   , withCMessages
       , Message(..)    , makeMessage    , withCMessage
       , Tags(..)       , makeTags       , withCTags
       , Directory(..)  , makeDirectory  , withCDirectory
       , Filenames(..)  , makeFilenames  , withCFilenames
       , ConfigList(..) , makeConfigList , withCConfigList
       , Properties(..) , makeProperties , withCProperties
       )
       where

import Data.Monoid
import Foreign
import Foreign.C.String
import System.IO.Unsafe
import System.Mail.Notmuch.Binding
import System.Mail.Notmuch.Wrapper.Template

lookupWithDefault :: Eq a => a -> b -> [(a,b)] -> b
lookupWithDefault key def lst = case lookup key lst of
                                 Just val -> val
                                 Nothing  -> def

data ErrorCode = Success
               | OutOfMemory
               | ReadOnlyDatabase
               | XapianException
               | FileError
               | FileNotMail
               | DuplicateMessageId
               | NullPointer
               | TagTooLong
               | UnbalancedFreezeThaw
               | UnbalancedAtomic
               | UnsupportedOperation
               | UpgradeRequired
               | PathError
               | IllegalArgument
               | Unknown
               deriving (Eq,Show,Enum)

statusErrorCorrespondance :: [(StatusCode,ErrorCode)]
statusErrorCorrespondance =
    [ (success                , Success              )
    , (out_of_memory          , OutOfMemory          )
    , (read_only_database     , ReadOnlyDatabase     )
    , (xapian_exception       , XapianException      )
    , (file_error             , FileError            )
    , (file_not_mail          , FileNotMail          )
    , (duplicate_message_id   , DuplicateMessageId   )
    , (null_pointer           , NullPointer          )
    , (tag_too_long           , TagTooLong           )
    , (unbalanced_freeze_thaw , UnbalancedFreezeThaw )
    , (unbalanced_atomic      , UnbalancedAtomic     )
    , (unsupported_operation  , UnsupportedOperation )
    , (upgrade_required       , UpgradeRequired      )
    , (path_error             , PathError            )
    , (illegal_argument       , IllegalArgument      )
    ]

statusToErrorCode :: StatusCode -> ErrorCode
statusToErrorCode stcode = lookupWithDefault stcode Unknown statusErrorCorrespondance

errorCodeMessage :: ErrorCode -> String
errorCodeMessage errcode = lookupWithDefault errcode "Unknown error" errMessages
 where errMessages :: [(ErrorCode,String)]
       errMessages = map (\(a,b) -> (b, showStatusCode a)) statusErrorCorrespondance

type NoDep = Ptr ()
$(makeGBDepData "Database" "NoDep" "c_database_destructor")

getDatabasePath :: Database s -> FilePath
getDatabasePath dt = unsafePerformIO
                   $ withCDatabase dt $ \cdb ->
                     c_database_get_path cdb >>= peekCString

instance Show (Database s) where
    show dt = "OpenDatabase <" <> getDatabasePath dt <> ">"

data DatabaseMode = DbReadOnly
                  | DbReadWrite
                  deriving (Eq,Show,Enum)
cdatabaseToDatabaseMode :: CDatabaseMode -> DatabaseMode
cdatabaseToDatabaseMode dbmode = lookupWithDefault dbmode DbReadOnly
    [ (dbmode_read_only  , DbReadOnly  )
    , (dbmode_read_write , DbReadWrite )
    ]

databaseToCDatabaseMode :: DatabaseMode -> CDatabaseMode
databaseToCDatabaseMode DbReadOnly  = dbmode_read_only
databaseToCDatabaseMode DbReadWrite = dbmode_read_write

type TagCreator = Either CMessage (Either CThread CDatabase)
type MsgCreator = Either CMessages CDatabase
type FNCreator  = Either CMessage CDirectory

$(makeManualOrGB "Query" "c_query_destructor")
$(makeGBDepData "Threads"    "CQuery"       "c_threads_destructor"            )
$(makeGBDepData "Thread"     "CThreads"     "c_thread_destructor"             )
$(makeGBDepData "Messages"   "CQuery"       "c_messages_destructor"           )
$(makeGBDepData "Message"    "MsgCreator"  "c_message_destructor"            )
$(makeGBDepData "Tags"       "TagCreator"  "c_tags_destructor"               )
$(makeGBDepData "Directory"  "CDatabase"    "c_directory_destructor"          )
$(makeGBDepData "Filenames"  "FNCreator"   "c_filenames_destructor"          )
$(makeGBDepData "ConfigList" "CDatabase"    "c_config_list_destructor"        )
$(makeGBDepData "Properties" "CMessage"     "c_message_properties_destructor" )

