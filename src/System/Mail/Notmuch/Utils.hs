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

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIO

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
         => (Database -> a s -> IO (Either StatusCode (Maybe (b s))))
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

oneShotA' :: forall a b s. (Database -> a -> IO b) -> NotmuchArrow s a b
oneShotA' f = oneShotA nf
 where nf :: Database -> Const a s -> IO (Either StatusCode (Maybe (Const b s)))
       nf db x = f db (getConst x) >>= \r -> return $ Right $ Just $ Const r

actPtr :: Default b => (Ptr a -> IO b) -> Ptr a -> IO b
actPtr f ptr = do
    if ptr == nullPtr then return def
                      else f ptr

dbStatusNmA :: NotmuchArrow s a String
dbStatusNmA = oneShotA' dbStatusNm
dbStatusNm :: Database -> a -> IO String
dbStatusNm (Database cdb) _ = c_database_string cdb >>= actPtr peekCString

dbPathNmA :: NotmuchArrow s a FilePath
dbPathNmA = oneShotA' dbPathNm
dbPathNm :: Database -> a -> IO FilePath
dbPathNm (Database cdb) _ = c_database_get_path cdb >>= actPtr peekCString

dbVersionNmA :: NotmuchArrow s a Integer
dbVersionNmA = oneShotA' dbVersionNm
dbVersionNm :: Database -> a -> IO Integer
dbVersionNm (Database cdb) _ = toInteger <$> c_database_get_version cdb

makeAtomicNmA :: NotmuchArrow s a b -> NotmuchArrow s a b
makeAtomicNmA (NmA f init_acc) =
    NmA (\db x acc -> do
             let cdb = cData db
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
dbRevisionNm :: Database -> a -> IO (String,Integer)
dbRevisionNm (Database cdb) _ =
    alloca $ \str_ptr -> do
    rev <- c_database_get_revision cdb str_ptr
    str <- peek str_ptr >>= actPtr peekCString
    return (str, toInteger rev)

getDirectoryNmA :: NotmuchArrow s FilePath (Directory s)
getDirectoryNmA = oneShotA getDirectoryNm
getDirectoryNm :: Database -> (Const FilePath s) -> IO (Either StatusCode
                                                               (Maybe (Directory s)))
getDirectoryNm (Database cdb) (Const path) =
    withCString path $ \cpath ->
    alloca $ \dir_ptr -> do
    status <- c_database_get_directory cdb cpath dir_ptr
    if status /= success
    then return $ Left status
    else do dir <- peek dir_ptr >>= makeDirectory nullPtr
            return $ Right $ Just dir

addMessageNmA :: NotmuchArrow s FilePath (Message s)
addMessageNmA = oneShotA addMessageNm
addMessageNm :: Database -> (Const FilePath s) -> IO (Either StatusCode
                                                             (Maybe (Message s)))
addMessageNm (Database cdb) (Const path) =
    withCString path $ \cpath ->
    alloca $ \msg_ptr -> do
    status <- c_database_add_message cdb cpath msg_ptr
    if status /= success && status /= duplicate_message_id
    then return $ Left status
    else do msg <- peek msg_ptr >>= makeMessage Nothing
            return $ Right $ Just msg

rmMessageNmA :: NotmuchArrow s FilePath ()
rmMessageNmA = oneShotA rmMessageNm
rmMessageNm :: Database -> Const FilePath s -> IO (Either StatusCode (Maybe (Const () s)))
rmMessageNm (Database cdb) (Const path) =
    withCString path $ \cpath -> do
    status <- c_database_remove_message cdb cpath
    if status /= success
    then return $ Left status
    else return $ Right $ Just $ Const ()

findMessageNmA :: NotmuchArrow s MsgId (Message s)
findMessageNmA = oneShotA findMessageNm
findMessageNm :: Database -> Const MsgId s -> IO (Either StatusCode (Maybe (Message s)))
findMessageNm (Database cdb) (Const (MsgId mid)) =
    withCString mid $ \cmid ->
    alloca $ \msg_ptr -> do
    status <- c_database_find_message cdb cmid msg_ptr
    if status /= success
    then return $ Left status
    else do cmsg <- peek msg_ptr
            if unCMessage cmsg == nullPtr
            then return $ Right Nothing
            else do msg <- makeMessage Nothing cmsg
                    return $ Right $ Just msg

findMessageByFilenameNmA :: NotmuchArrow s FilePath (Message s)
findMessageByFilenameNmA = oneShotA findMessageByFilenameNm
findMessageByFilenameNm :: Database -> Const FilePath s
                        -> IO (Either StatusCode (Maybe (Message s)))
findMessageByFilenameNm (Database cdb) (Const path) =
    withCString path $ \cpath ->
    alloca $ \msg_ptr -> do
    status <- c_database_find_message_by_filename cdb cpath msg_ptr
    if status /= success
    then return $ Left status
    else do cmsg <- peek msg_ptr
            if unCMessage cmsg == nullPtr
            then return $ Right Nothing
            else do msg <- makeMessage Nothing cmsg
                    return $ Right $ Just msg

msgHeaderNmA :: String -> NotmuchArrow s (Message s) String
msgHeaderNmA hd = oneShotA $ msgHeaderNm hd
msgHeaderNm :: String -> Database -> Message s -> IO (Either StatusCode
                                                             (Maybe (Const String s)))
msgHeaderNm hd _ msg =
    withCString hd $ \chd ->
    withCMessage msg $ \cmsg -> do
    chdval <- c_message_get_header cmsg chd
    if chdval == nullPtr
    then return $ Left null_pointer
    else do hdval  <- peekCString chdval
            return $ Right $ Just $ Const hdval

