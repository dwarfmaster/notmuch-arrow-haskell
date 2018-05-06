{-# LANGUAGE TemplateHaskell #-}

module System.Mail.Notmuch.Wrapper
       ( ErrorCode(..), errorCodeMessage, statusToErrorCode
       , Database(..), getDatabasePath
       , DatabaseMode(..), cdatabaseToDatabaseMode
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

data Database = OpenDatabase !CDatabase | ClosedDatabase

getDatabasePath :: Database -> FilePath
getDatabasePath ClosedDatabase       = ""
getDatabasePath (OpenDatabase cdata) = unsafePerformIO
                                     $ c_database_get_path cdata >>= peekCString

instance Show Database where
    show ClosedDatabase      = "ClosedDatabase"
    show dt@(OpenDatabase _) = "OpenDatabase <" <> getDatabasePath dt <> ">"

data DatabaseMode = DbReadOnly
                  | DbReadWrite
                  deriving (Eq,Show,Enum)
cdatabaseToDatabaseMode :: CDatabaseMode -> DatabaseMode
cdatabaseToDatabaseMode dbmode = lookupWithDefault dbmode DbReadOnly
    [ (dbmode_read_only  , DbReadOnly  )
    , (dbmode_read_write , DbReadWrite )
    ]

type TagCreator s = Either CMessage CThread
type NoDep      s = Ptr ()
type FNCreator  s = Either CMessage CDirectory

$(makeManualOrGB "Query" "c_query_destructor")
$(makeGBDepData "Threads"    "Query"       "c_threads_destructor"            )
$(makeGBDepData "Thread"     "Threads"     "c_thread_destructor"             )
$(makeGBDepData "Messages"   "Query"       "c_messages_destructor"           )
$(makeGBDepData "Message"    "Messages"    "c_message_destructor"            )
$(makeGBDepData "Tags"       "TagCreator"  "c_tags_destructor"               )
$(makeGBDepData "Directory"  "NoDep"       "c_directory_destructor"          )
$(makeGBDepData "Filenames"  "FNCreator"   "c_filenames_destructor"          )
$(makeGBDepData "ConfigList" "NoDep"       "c_config_list_destructor"        )
$(makeGBDepData "Properties" "Message"     "c_message_properties_destructor" )

