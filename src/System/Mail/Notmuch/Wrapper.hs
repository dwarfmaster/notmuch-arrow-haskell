{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs           #-}

module System.Mail.Notmuch.Wrapper
       ( ErrorCode(..), errorCodeMessage, statusToErrorCode
       , Database(..), getDatabasePath, makeDatabase, withCDatabase
       , DatabaseMode(..), cdatabaseToDatabaseMode, databaseToCDatabaseMode
       , QueryExclude(..), queryExcludeToExcludeFlag, excludeFlagToQuery
       , QuerySort(..), querySortToSortOrder, sortOrderToQuerySort
       , Query      , makeQuery      , withCQuery
       , Threads    , makeThreads    , withCThreads
       , Thread     , makeThread     , withCThread
       , Messages   , makeMessages   , withCMessages
       , Message    , makeMessage    , withCMessage
       , Tags       , makeTags       , withCTags
       , Directory  , makeDirectory  , withCDirectory
       , Filenames  , makeFilenames  , withCFilenames
       , ConfigList , makeConfigList , withCConfigList
       , Properties , makeProperties , withCProperties
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
data Database s = Database CDatabase
makeDatabase :: Ptr () -> CDatabase -> IO (Database s)
makeDatabase = const $ return . Database
withCDatabase :: Database s -> (CDatabase -> IO a) -> IO a
withCDatabase (Database cdb) f = f cdb
-- TODO make database a foreign ptr with a working destructor
-- As of now the following segfaults
-- $(makeGBDepData "Database" "NoDep" "c_database_destructor")

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

data QueryExclude = QueryExcludeFlag
                  | QueryExcludeTrue
                  | QueryExcludeFalse
                  | QueryExcludeAll
                  deriving (Eq,Show,Enum)

queryExcludeToExcludeFlag :: QueryExclude -> Exclude
queryExcludeToExcludeFlag QueryExcludeFlag  = exclude_flag
queryExcludeToExcludeFlag QueryExcludeTrue  = exclude_true
queryExcludeToExcludeFlag QueryExcludeFalse = exclude_false
queryExcludeToExcludeFlag QueryExcludeAll   = exclude_all

excludeFlagToQuery :: Exclude -> QueryExclude
excludeFlagToQuery excl = lookupWithDefault excl QueryExcludeFlag
    [ (exclude_flag  , QueryExcludeFlag  )
    , (exclude_true  , QueryExcludeTrue  )
    , (exclude_false , QueryExcludeFalse )
    , (exclude_all   , QueryExcludeAll   )
    ]

data QuerySort = SortOldestFirst
               | SortNewestFirst
               | SortMessageId
               | SortUnsorted
               deriving (Eq,Show,Enum)

querySortToSortOrder :: QuerySort -> SortOrder
querySortToSortOrder SortOldestFirst = sort_oldest_first
querySortToSortOrder SortNewestFirst = sort_newest_first
querySortToSortOrder SortMessageId   = sort_message_id
querySortToSortOrder SortUnsorted    = sort_unsorted

sortOrderToQuerySort :: SortOrder -> QuerySort
sortOrderToQuerySort sorto = lookupWithDefault sorto SortUnsorted
    [ (sort_oldest_first , SortOldestFirst )
    , (sort_newest_first , SortNewestFirst )
    , (sort_message_id   , SortMessageId   )
    , (sort_unsorted     , SortUnsorted    )
    ]

$(makeGBDepData "Query"      "Database"    "c_query_destructor"              )
$(makeGBDepData "Threads"    "Query"       "c_threads_destructor"            )
$(makeGBDepData "Thread"     "Threads"     "c_thread_destructor"             )
$(makeGBDepData "Messages"   "Query"       "c_messages_destructor"           )
type MsgCreator s = Either (Messages s) (Database s)
$(makeGBDepData "Message"    "MsgCreator"  "c_message_destructor"            )
type TagCreator s = Either (Message s) (Either (Thread s) (Database s))
$(makeGBDepData "Directory"  "Database"    "c_directory_destructor"          )
type FNCreator  s = Either (Message s) (Directory s)
$(makeGBDepData "Tags"       "TagCreator"  "c_tags_destructor"               )
$(makeGBDepData "Filenames"  "FNCreator"   "c_filenames_destructor"          )
$(makeGBDepData "ConfigList" "Database"    "c_config_list_destructor"        )
$(makeGBDepData "Properties" "Message"     "c_message_properties_destructor" )

