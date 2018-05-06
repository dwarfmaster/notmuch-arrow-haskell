{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Mail.Notmuch.Binding where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

#include <notmuch.h>

newtype StatusCode = StatusCode { unStatusCode :: CInt }
                   deriving (Eq,Show)

#{enum StatusCode, StatusCode
 , success                        = NOTMUCH_STATUS_SUCCESS
 , out_of_memory                  = NOTMUCH_STATUS_OUT_OF_MEMORY
 , read_only_database             = NOTMUCH_STATUS_READ_ONLY_DATABASE
 , xapian_exception               = NOTMUCH_STATUS_XAPIAN_EXCEPTION
 , file_error                     = NOTMUCH_STATUS_FILE_ERROR
 , file_not_mail                  = NOTMUCH_STATUS_FILE_NOT_EMAIL
 , duplicate_message_id           = NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID
 , null_pointer                   = NOTMUCH_STATUS_NULL_POINTER
 , tag_too_long                   = NOTMUCH_STATUS_TAG_TOO_LONG
 , unbalanced_freeze_thaw         = NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW
 , unbalanced_atomic              = NOTMUCH_STATUS_UNBALANCED_ATOMIC
 , unsupported_operation          = NOTMUCH_STATUS_UNSUPPORTED_OPERATION
 , upgrade_required               = NOTMUCH_STATUS_UPGRADE_REQUIRED
 , path_error                     = NOTMUCH_STATUS_PATH_ERROR
 , illegal_argument               = NOTMUCH_STATUS_ILLEGAL_ARGUMENT
 }

foreign import ccall "notmuch.h notmuch_status_to_string"
    c_status_to_string :: StatusCode
                       -> IO CString

showStatusCode :: StatusCode -> String
showStatusCode stc = unsafePerformIO $ c_status_to_string stc >>= peekCString

-- Create aliases for pointers to opaque C structures
newtype CDatabase   = CDatabase   { unCDb         :: Ptr CDatabase   }
newtype CQuery      = CQuery      { unCQuery      :: Ptr CQuery      }
newtype CThreads    = CThreads    { unCThreads    :: Ptr CThreads    }
newtype CThread     = CThread     { unCThread     :: Ptr CThread     }
newtype CMessages   = CMessages   { unCMessages   :: Ptr CMessages   }
newtype CMessage    = CMessage    { unCMessage    :: Ptr CMessage    }
newtype CTags       = CTags       { unCTags       :: Ptr CTags       }
newtype CDirectory  = CDirectory  { unCDirectory  :: Ptr CDirectory  }
newtype CFilenames  = CFilenames  { unCFilenames  :: Ptr CFilenames  }
newtype CConfigList = CConfigList { unCConfigList :: Ptr CConfigList }
newtype CProperties = CProperties { unCProperties :: Ptr CProperties }

-- notmuch_database_create* is not exposed~: this library aims to allow to manipulate
-- the objects in the database, not the database itself

newtype CDatabaseMode = CDatabaseMode { unCDatabaseMode :: CInt }
                     deriving (Eq, Show)

#{enum CDatabaseMode, CDatabaseMode
 , dbmode_read_only  = NOTMUCH_DATABASE_MODE_READ_ONLY
 , dbmode_read_write = NOTMUCH_DATABASE_MODE_READ_WRITE
 }

foreign import ccall "notmuch.h notmuch_database_open"
    c_database_open :: CString
                    -> CDatabaseMode
                    -> Ptr CDatabase
                    -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_status_string"
    c_database_string :: CDatabase
                      -> IO CString

foreign import ccall "notmuch.h notmuch_database_close"
    c_database_close :: CDatabase
                     -> IO StatusCode

-- For the same reason as for notmuch_database_create, notmuch_database_compact
-- is not exposed

foreign import ccall "notmuch.h notmuch_database_destroy"
    c_database_destroy :: CDatabase
                       -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_get_path"
    c_database_get_path :: CDatabase
                        -> IO CString

foreign import ccall "notmuch.h notmuch_database_get_version"
    c_database_get_version :: CDatabase
                           -> IO CUInt

-- For the same reason as for notmuch_database_create, all the database upgrade feature
-- are not exposed

foreign import ccall "notmuch.h notmuch_database_begin_atomic"
    c_database_begin_atomic :: CDatabase
                            -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_end_atomic"
    c_database_end_atomic :: CDatabase
                          -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_get_revision"
    c_database_get_revision :: CDatabase
                            -> Ptr CString
                            -> IO CULong

foreign import ccall "notmuch.h notmuch_database_get_directory"
    c_database_get_directory :: CDatabase
                             -> CString
                             -> Ptr CDirectory
                             -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_add_message"
    c_database_add_message :: CDatabase
                           -> CString
                           -> Ptr CMessage
                           -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_remove_message"
    c_database_remove_message :: CDatabase
                              -> CString
                              -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_find_message"
    c_database_find_message :: CDatabase
                            -> CString
                            -> Ptr CMessage
                            -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_find_message_by_filename"
    c_database_find_message_by_filename :: CDatabase
                                        -> CString
                                        -> Ptr CMessage
                                        -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_get_all_tags"
    c_database_get_all_tags :: CDatabase
                            -> IO CTags

foreign import ccall "notmuch.h notmuch_query_create"
    c_query_create :: CDatabase
                   -> CString
                   -> IO CQuery

newtype SortOrder = SortOrder { unSortOrder :: CInt }
                  deriving (Eq,Show)

#{enum SortOrder, SortOrder
 , sort_oldest_first = NOTMUCH_SORT_OLDEST_FIRST
 , sort_newest_first = NOTMUCH_SORT_NEWEST_FIRST
 , sort_message_id   = NOTMUCH_SORT_MESSAGE_ID
 , sort_unsorted     = NOTMUCH_SORT_UNSORTED
 }

foreign import ccall "notmuch.h notmuch_query_get_query_string"
    c_query_get_query_string :: CQuery
                             -> IO CString

newtype Exclude = Exclude { unExclude :: CInt }
                deriving (Eq,Show)

#{enum Exclude, Exclude
 , exclude_flag  = NOTMUCH_EXCLUDE_FLAG
 , exclude_true  = NOTMUCH_EXCLUDE_TRUE
 , exclude_false = NOTMUCH_EXCLUDE_FALSE
 , exclude_all   = NOTMUCH_EXCLUDE_ALL
 }

foreign import ccall "notmuch.h notmuch_query_set_omit_excluded"
    c_query_set_omit_excluded :: CQuery
                              -> Exclude
                              -> IO ()

foreign import ccall "notmuch.h notmuch_query_set_sort"
    c_query_set_sort :: CQuery
                     -> SortOrder
                     -> IO ()

foreign import ccall "notmuch.h notmuch_query_add_tag_exclude"
    c_query_add_tag_exclude :: CQuery
                            -> CString
                            -> IO ()

foreign import ccall "notmuch.h notmuch_query_search_threads_st"
    c_query_search_threads :: CQuery
                           -> Ptr CThreads
                           -> StatusCode

foreign import ccall "notmuch.h &notmuch_threads_destroy"
    c_threads_destructor :: FunPtr (Ptr CThreads -> IO ())

foreign import ccall "notmuch.h notmuch_query_search_messages_st"
    c_query_search_messages :: CQuery
                            -> Ptr CMessages
                            -> StatusCode

foreign import ccall "notmuch.h &notmuch_messages_destroy"
    c_messages_destructor :: FunPtr (Ptr CMessages -> IO ())

foreign import ccall "notmuch.h notmuch_query_destroy"
    c_query_destroy :: CQuery
                    -> IO ()

foreign import ccall "notmuch.h &notmuch_query_destroy"
    c_query_destructor :: FunPtr (Ptr CQuery -> IO ())

foreign import ccall "notmuch.h notmuch_threads_valid"
    c_threads_valid :: CThreads
                    -> IO CInt
                   
foreign import ccall "notmuch.h notmuch_threads_get"
    c_threads_get :: CThreads
                  -> IO CThread

foreign import ccall "notmuch.h notmuch_threads_move_to_next"
    c_threads_move_to_next :: CThreads
                           -> IO ()
                           
foreign import ccall "notmuch.h notmuch_query_count_messages_st"
    c_query_count_messages :: CQuery
                           -> Ptr CUInt
                           -> IO StatusCode

foreign import ccall "notmuch.h notmuch_query_count_threads_st"
    c_query_count_threads :: CQuery
                          -> Ptr CUInt
                          -> IO StatusCode

foreign import ccall "notmuch.h notmuch_thread_get_thread_id"
    c_thread_get_thread_id :: CThread
                           -> IO CString

foreign import ccall "notmuch.h notmuch_thread_get_total_messages"
    c_thread_get_total_messages :: CThread
                                -> IO CInt

foreign import ccall "notmuch.h notmuch_thread_get_toplevel_messages"
    c_thread_get_toplevel_messages :: CThread
                                   -> IO CMessages

foreign import ccall "notmuch.h notmuch_thread_get_messages"
    c_thread_get_messages :: CThread
                          -> IO CMessages
                          
foreign import ccall "notmuch.h notmuch_thread_get_matched_messages"
    c_thread_get_matched_messages :: CThread
                                  -> IO CInt

foreign import ccall "notmuch.h notmuch_thread_get_authors"
    c_thread_get_authors :: CThread
                         -> IO CString

foreign import ccall "notmuch.h notmuch_thread_get_subject"
    c_thread_get_subject :: CThread
                         -> IO CString

foreign import ccall "notmuch.h notmuch_thread_get_oldest_date"
    c_thread_get_oldest_date :: CThread
                             -> IO CTime

foreign import ccall "notmuch.h notmuch_thread_get_newest_date"
    c_thread_get_newest_date :: CThread
                             -> IO CTime

foreign import ccall "notmuch.h notmuch_thread_get_tags"
    c_thread_get_tags :: CThread
                      -> IO CTags

foreign import ccall "notmuch.h notmuch_thread_destroy"
    c_thread_destroy :: CThread
                     -> IO ()

foreign import ccall "notmuch.h &notmuch_thread_destroy"
    c_thread_destructor :: FunPtr (Ptr CThread -> IO ())

foreign import ccall "notmuch.h notmuch_messages_valid"
    c_messages_valid :: CMessages
                     -> IO CInt

foreign import ccall "notmuch.h notmuch_messages_get"
    c_messages_get :: CMessages
                   -> IO CMessage

foreign import ccall "notmuch.h notmuch_messages_move_to_next"
    c_messages_move_to_next :: CMessages
                            -> IO ()

foreign import ccall "notmuch.h notmuch_messages_collect_tags"
    c_messages_collect_tags :: CMessages
                            -> IO CTags

foreign import ccall "notmuch.h notmuch_message_get_message_id"
    c_message_get_message_id :: CMessage
                             -> IO CString

foreign import ccall "notmuch.h notmuch_message_get_thread_id"
    c_message_get_thread_id :: CMessage
                            -> IO CString

foreign import ccall "notmuch.h notmuch_messages_get_replies"
    c_message_get_replies :: CMessage
                          -> IO CMessages

foreign import ccall "notmuch.h notmuch_message_get_filenames"
    c_message_get_filenames :: CMessage
                            -> IO CFilenames

foreign import ccall "notmuch.h notmuch_message_destroy"
    c_message_destroy :: CMessage
                      -> IO ()

foreign import ccall "notmuch.h &notmuch_message_destroy"
    c_message_destructor :: FunPtr (Ptr CMessage -> IO ())

newtype MsgFlag = MsgFlag { unMsgFlag :: CInt }
                deriving (Eq,Show)

#{enum MsgFlag, MsgFlag
 , flag_match    = NOTMUCH_MESSAGE_FLAG_MATCH
 , flag_excluded = NOTMUCH_MESSAGE_FLAG_EXCLUDED
 , flag_ghost    = NOTMUCH_MESSAGE_FLAG_GHOST
 }

foreign import ccall "notmuch.h notmuch_message_get_flag"
    c_message_get_flag :: CMessage
                       -> MsgFlag
                       -> IO CInt

foreign import ccall "notmuch.h notmuch_message_set_flag"
    c_message_set_flag :: CMessage
                       -> MsgFlag
                       -> CInt
                       -> IO ()

foreign import ccall "notmuch.h notmuch_message_get_date"
    c_message_get_date :: CMessage
                       -> IO CTime

foreign import ccall "notmuch.h notmuch_message_get_header"
    c_message_get_header :: CMessage
                         -> CString
                         -> IO CString

foreign import ccall "notmuch.h notmuch_message_get_tags"
    c_message_get_tags :: CMessage
                       -> IO CTags
                     
foreign import ccall "notmuch.h notmuch_message_add_tag"
    c_message_add_tag :: CMessage
                      -> CString
                      -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_remove_tag"
    c_message_remove_tag :: CMessage
                         -> CString
                         -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_remove_all_tags"
    c_message_remove_all_tags :: CMessage
                              -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_freeze"
    c_message_freeze :: CMessage
                     -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_thaw"
    c_message_thaw :: CMessage
                   -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_get_property"
    c_message_get_property :: CMessage
                           -> CString
                           -> Ptr CString
                           -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_add_property"
    c_message_add_property :: CMessage
                           -> CString
                           -> CString
                           -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_remove_property"
    c_message_remove_property :: CMessage
                              -> CString
                              -> CString
                              -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_remove_all_properties"
    c_message_remove_all_properties :: CMessage
                                    -> CString
                                    -> IO StatusCode

foreign import ccall "notmuch.h notmuch_message_get_properties"
    c_message_get_properties :: CMessage
                             -> CString
                             -> IO CProperties
                             
foreign import ccall "notmuch.h notmuch_message_properties_valid"
    c_message_properties_valid :: CProperties
                               -> IO CInt

foreign import ccall "notmuch.h notmuch_message_properties_move_to_next"
    c_message_properties_move_to_next :: CProperties
                                      -> IO ()

foreign import ccall "notmuch.h notmuch_message_properties_key"
    c_message_properties_key :: CProperties
                             -> IO CString

foreign import ccall "notmuch.h notmuch_message_properties_value"
    c_message_properties_value :: CProperties
                               -> IO CString

foreign import ccall "notmuch.h &notmuch_message_properties_destroy"
    c_message_properties_destructor :: FunPtr (Ptr CProperties -> IO ())

foreign import ccall "notmuch.h notmuch_tags_valid"
    c_tags_valid :: CTags
                 -> IO CInt

foreign import ccall "notmuch.h notmuch_tags_get"
    c_tags_get :: CTags
               -> IO CString

foreign import ccall "notmuch.h notmuch_tags_move_to_next"
    c_tags_move_to_next :: CTags
                        -> IO ()

foreign import ccall "notmuch.h &notmuch_tags_destroy"
    c_tags_destructor :: FunPtr (Ptr CTags -> IO ())

foreign import ccall "notmuch.h notmuch_directory_set_mtime"
    c_directory_set_mtime :: CDirectory
                          -> CTime
                          -> IO StatusCode

foreign import ccall "notmuch.h notmuch_directory_get_mtime"
    c_directory_get_mtime :: CDirectory
                          -> IO CTime
                          
foreign import ccall "notmuch.h notmuch_directory_get_child_files"
    c_directory_get_child_files :: CDirectory
                                -> IO CFilenames

foreign import ccall "notmuch.h notmuch_directory_get_child_directories"
    c_directory_get_child_directories :: CDirectory
                                      -> IO CFilenames

-- We don't expose notmuch_directory_delete

foreign import ccall "notmuch.h notmuch_directory_destroy"
    c_directory_destroy :: CDirectory
                        -> IO ()

foreign import ccall "notmuch.h &notmuch_directory_destroy"
    c_directory_destructor :: FunPtr (Ptr CDirectory -> IO ())
                        
foreign import ccall "notmuch.h notmuch_filenames_valid"
    c_filenames_valid :: CFilenames
                      -> IO CInt

foreign import ccall "notmuch.h notmuch_filenames_get"
    c_filenames_get :: CFilenames
                    -> IO CString

foreign import ccall "notmuch.h notmuch_filenames_move_to_next"
    c_filenames_move_to_next :: CFilenames
                             -> IO ()

foreign import ccall "notmuch.h &notmuch_filenames_destroy"
    c_filenames_destructor :: FunPtr (Ptr CFilenames -> IO ())

foreign import ccall "notmuch.h notmuch_database_set_config"
    c_database_set_config :: CDatabase
                          -> CString
                          -> CString
                          -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_get_config"
    c_database_get_config :: CDatabase
                          -> CString
                          -> Ptr CString
                          -> IO StatusCode

foreign import ccall "notmuch.h notmuch_database_get_config_mist"
    c_database_get_config_list :: CDatabase
                               -> CString
                               -> Ptr CConfigList
                               -> IO StatusCode

foreign import ccall "notmuch.h notmuch_config_list_valid"
    c_config_list_valid :: CConfigList
                        -> IO CInt

foreign import ccall "notmuch.h notmuch_config_list_key"
    c_config_list_key :: CConfigList
                      -> IO CString

foreign import ccall "notmuch.h notmuch_config_list_value"
    c_config_list_value :: CConfigList
                        -> IO CString

foreign import ccall "notmuch.h notmuch_config_list_move_to_next"
    c_config_list_move_to_next :: CConfigList
                               -> IO ()

foreign import ccall "notmuch.h notmuch_config_list_destroy"
    c_config_list_destroy :: CConfigList
                          -> IO ()

foreign import ccall "notmuch.h &notmuch_config_list_destroy"
    c_config_list_destructor :: FunPtr (Ptr CConfigList -> IO ())

foreign import ccall "notmuch.h notmuch_built_with"
    c_built_with :: CString
                 -> IO CInt

