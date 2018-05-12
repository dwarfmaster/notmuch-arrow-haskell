{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module System.Mail.Notmuch.Utils where

import Foreign
import Foreign.C.String
import Data.Default
import Data.Functor.Const
import System.Mail.Notmuch.Binding
import System.Mail.Notmuch.Wrapper
import System.Mail.Notmuch.Arrow

newtype MsgId = MsgId { unMsgId :: String }

class Equiv a b where
    rEquiv :: a -> b
    lEquiv :: b -> a
instance Equiv a a where
    rEquiv = id
    lEquiv = id
instance Equiv (Const a b) a where
    rEquiv = getConst
    lEquiv = Const

oneShotA :: (Equiv (a s) c, Equiv (b s) d)
         => (Database s -> a s -> IO (Either StatusCode (Maybe (b s))))
         -> NotmuchArrow s c d
oneShotA f = NmA (\db x b -> if b
                             then do er <- f db $ lEquiv x
                                     return $ case er of
                                       Left stcode    -> Left $ statusToErrorCode stcode
                                       Right Nothing  -> Right Nothing
                                       Right (Just r) -> Right $ Just (rEquiv r, False)
                             else return $ Right Nothing
                 )
                 True

oneShotA' :: forall a b s. (Database s -> a -> IO b) -> NotmuchArrow s a b
oneShotA' f = oneShotA nf
 where nf :: Database s -> Const a s -> IO (Either StatusCode (Maybe (Const b s)))
       nf db x = f db (getConst x) >>= \r -> return $ Right $ Just $ Const r

actPtr :: Default b => (Ptr a -> IO b) -> Ptr a -> IO b
actPtr f ptr = do
    if ptr == nullPtr then return def
                      else f ptr

dbStatusNmA :: NotmuchArrow s a String
dbStatusNmA = oneShotA' dbStatusNm
dbStatusNm :: Database s -> a -> IO String
dbStatusNm dt _ = withCDatabase dt $ \cdb ->
                  c_database_string cdb >>= actPtr peekCString

dbPathNmA :: NotmuchArrow s a FilePath
dbPathNmA = oneShotA' dbPathNm
dbPathNm :: Database s -> a -> IO FilePath
dbPathNm dt _ = withCDatabase dt $ \cdb ->
                c_database_get_path cdb >>= actPtr peekCString

dbVersionNmA :: NotmuchArrow s a Integer
dbVersionNmA = oneShotA' dbVersionNm
dbVersionNm :: Database s -> a -> IO Integer
dbVersionNm dt _ = withCDatabase dt $ \cdb ->
                   toInteger <$> c_database_get_version cdb

makeAtomicNmA :: NotmuchArrow s a b -> NotmuchArrow s a b
makeAtomicNmA (NmA f init_acc) =
    NmA (\db x acc -> withCDatabase db $ \cdb -> do
             stcode <- c_database_begin_atomic cdb
             if stcode /= success
             then return $ Left $ statusToErrorCode stcode
             else do r <- f db x acc
                     stcode' <- c_database_end_atomic cdb
                     if stcode' /= success
                     then return $ Left $ statusToErrorCode stcode'
                     else return r
        )
        init_acc

dbRevisionNmA :: NotmuchArrow s a (String,Integer)
dbRevisionNmA = oneShotA' dbRevisionNm
dbRevisionNm :: Database s -> a -> IO (String,Integer)
dbRevisionNm dt _ =
    withCDatabase dt $ \cdb ->
    alloca $ \str_ptr -> do
    rev <- c_database_get_revision cdb str_ptr
    str <- peek str_ptr >>= actPtr peekCString
    return (str, toInteger rev)

getDirectoryNmA :: NotmuchArrow s FilePath (Directory s)
getDirectoryNmA = oneShotA getDirectoryNm
getDirectoryNm :: Database s -> (Const FilePath s) -> IO (Either StatusCode
                                                                 (Maybe (Directory s)))
getDirectoryNm db (Const path) =
    withCDatabase db $ \cdb ->
    withCString path $ \cpath ->
    alloca $ \dir_ptr -> do
    status <- c_database_get_directory cdb cpath dir_ptr
    if status /= success
    then return $ Left status
    else do dir <- peek dir_ptr >>= makeDirectory db
            return $ Right $ Just dir

addMessageNmA :: NotmuchArrow s FilePath (Message s)
addMessageNmA = oneShotA addMessageNm
addMessageNm :: Database s -> (Const FilePath s) -> IO (Either StatusCode
                                                               (Maybe (Message s)))
addMessageNm db (Const path) =
    withCDatabase db $ \cdb ->
    withCString path $ \cpath ->
    alloca $ \msg_ptr -> do
    status <- c_database_add_message cdb cpath msg_ptr
    if status /= success && status /= duplicate_message_id
    then return $ Left status
    else do msg <- peek msg_ptr >>= makeMessage (Right db)
            return $ Right $ Just msg

rmMessageNmA :: NotmuchArrow s FilePath ()
rmMessageNmA = oneShotA rmMessageNm
rmMessageNm :: Database s -> Const FilePath s -> IO (Either StatusCode
                                                            (Maybe (Const () s)))
rmMessageNm dt (Const path) =
    withCDatabase dt $ \cdb ->
    withCString path $ \cpath -> do
    status <- c_database_remove_message cdb cpath
    if status /= success
    then return $ Left status
    else return $ Right $ Just $ Const ()

findMessageNmA :: NotmuchArrow s MsgId (Message s)
findMessageNmA = oneShotA findMessageNm
findMessageNm :: Database s -> Const MsgId s -> IO (Either StatusCode (Maybe (Message s)))
findMessageNm db (Const (MsgId mid)) =
    withCDatabase db $ \cdb ->
    withCString mid $ \cmid ->
    alloca $ \msg_ptr -> do
    status <- c_database_find_message cdb cmid msg_ptr
    if status /= success
    then return $ Left status
    else do cmsg <- peek msg_ptr
            if unCMessage cmsg == nullPtr
            then return $ Right Nothing
            else do msg <- makeMessage (Right db) cmsg
                    return $ Right $ Just msg

findMessageByFilenameNmA :: NotmuchArrow s FilePath (Message s)
findMessageByFilenameNmA = oneShotA findMessageByFilenameNm
findMessageByFilenameNm :: Database s -> Const FilePath s
                        -> IO (Either StatusCode (Maybe (Message s)))
findMessageByFilenameNm db (Const path) =
    withCDatabase db $ \cdb ->
    withCString path $ \cpath ->
    alloca $ \msg_ptr -> do
    status <- c_database_find_message_by_filename cdb cpath msg_ptr
    if status /= success
    then return $ Left status
    else do cmsg <- peek msg_ptr
            if unCMessage cmsg == nullPtr
            then return $ Right Nothing
            else do msg <- makeMessage (Right db) cmsg
                    return $ Right $ Just msg

msgHeaderNmA :: String -> NotmuchArrow s (Message s) String
msgHeaderNmA hd = oneShotA $ msgHeaderNm hd
msgHeaderNm :: String -> Database s -> Message s -> IO (Either StatusCode
                                                               (Maybe (Const String s)))
msgHeaderNm hd _ msg =
    withCString hd $ \chd ->
    withCMessage msg $ \cmsg -> do
    chdval <- c_message_get_header cmsg chd
    if chdval == nullPtr
    then return $ Left null_pointer
    else do hdval  <- peekCString chdval
            return $ Right $ Just $ Const hdval

arrowOfIterator :: forall a a' b c c' s
                 . (Equiv (a s) a', Equiv (c s) c')
                => (Database s -> (a s) -> IO (Either StatusCode (b s))) -- Initialisation
                -> ((b s) -> IO (Maybe (c s)))       -- Reading
                -> ((b s) -> IO (b s))               -- next (can render invalid the
                                                     --       previous value)
                -> NotmuchArrow s a' c'
arrowOfIterator int rd nxt = NmA arrow_fun Nothing
 where arrow_fun :: Database s -> a' -> (Maybe (b s))
                 -> IO (Either ErrorCode (Maybe (c', Maybe (b s))))
       arrow_fun db x Nothing    = do
           eacc <- int db (lEquiv x)
           case eacc of
               Left scode -> return $ Left $ statusToErrorCode scode
               Right acc  -> arrow_fun db x (Just acc)
       arrow_fun _  _ (Just acc) = do
           val <- rd acc
           case val of
             Nothing   -> return $ Right Nothing
             Just nval -> nxt acc
                      >>= \nacc -> return $ Right $ Just (rEquiv nval, Just nacc)

dbAllTags :: NotmuchArrow s () String
dbAllTags = arrowOfIterator int rd nxt
 where int :: Database s -> Const () s -> IO (Either StatusCode (Tags s))
       int db _ =
           withCDatabase db $ \cdb -> do
           ctags <- c_database_get_all_tags cdb
           tags  <- makeTags (Right $ Right db) ctags
           return $ Right tags

       rd :: Tags s -> IO (Maybe (Const String s))
       rd tags =
           withCTags tags $ \ctags -> do
           b <- c_tags_valid ctags
           if b == 0
           then return Nothing
           else c_tags_get ctags >>= actPtr peekCString
                                 >>= return . Just . Const
           
       nxt :: Tags s -> IO (Tags s)
       nxt tags =
           withCTags tags $ \ctags -> do
           c_tags_move_to_next ctags
           return tags

dbQueryNmA :: QueryExclude -> NotmuchArrow s String (Query s)
dbQueryNmA fl = oneShotA $ dbQueryNm fl
dbQueryNm :: QueryExclude -> Database s -> Const String s
          -> IO (Either StatusCode (Maybe (Query s)))
dbQueryNm fl db (Const str) =
    withCDatabase db $ \cdb ->
    withCString str  $ \cstr -> do
    cquery <- c_query_create cdb cstr
    c_query_set_omit_excluded cquery (queryExcludeToExcludeFlag fl)
    query  <- makeQuery db cquery
    return $ Right $ Just query

queryThreadsNmA :: QuerySort -> NotmuchArrow s (Query s) (Thread s)
queryThreadsNmA srt = arrowOfIterator int rd nxt
 where int :: Database s -> (Query s) -> IO (Either StatusCode (Threads s))
       int _ query =
           withCQuery query $ \cquery ->
           alloca $ \threads_ptr -> do 
               c_query_set_sort cquery $ querySortToSortOrder srt
               scode <- c_query_search_threads cquery threads_ptr
               if scode /= success
               then return $ Left scode
               else do cthreads <- peek threads_ptr
                       threads  <- makeThreads query cthreads
                       return $ Right threads

       rd :: Threads s -> IO (Maybe (Thread s))
       rd threads =
           withCThreads threads $ \cthreads -> do
           b <- c_threads_valid cthreads
           if b == 0
           then return Nothing
           else c_threads_get cthreads >>= makeThread threads
                                       >>= return . Just

       nxt :: Threads s -> IO (Threads s)
       nxt threads =
           withCThreads threads $ \cthreads -> do
           c_threads_move_to_next cthreads
           return threads

