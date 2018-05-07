{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module System.Mail.Notmuch.Arrow where

import Prelude                  hiding ((.))
import Data.Typeable
import Data.Monoid
import Control.Exception
import Control.Category
import Control.Arrow
import Control.Arrow.ArrowList
import Foreign
import Foreign.C.String

import System.Mail.Notmuch.Binding
import System.Mail.Notmuch.Wrapper

data NotmuchArrow s a b where
    NmA :: (Database -> a -> c -> IO (Either ErrorCode (Maybe (b,c))))
        -> c
        -> NotmuchArrow s a b

newtype NotmuchException = NmE ErrorCode
                         deriving (Show,Typeable)
instance Exception NotmuchException where
    displayException (NmE errcode) = "Notmuch error " <> show errcode
                                  <> " : " <> errorCodeMessage errcode

runArrowWith :: c -> (c -> IO (Either ErrorCode (Maybe (b, c))))
             -> IO (Either ErrorCode [b])
runArrowWith acc arrow = arrow acc >>= \ret ->
    case ret of
      Left errcode        -> return $ Left errcode
      Right Nothing       -> return $ Right []
      Right (Just (b,nc)) -> do etl <- runArrowWith nc arrow
                                return $ etl >>= Right . (b:)

-- If an error was encontered, it will be thrown as a NotmuchException
runWithDatabase :: Database -> (forall s. NotmuchArrow s a b) -> a -> IO [b]
runWithDatabase db (NmA f acc) x = do
  result <- runArrowWith acc (f db x)
  case result of
    Left  errcode -> throw $ NmE errcode
    Right lst     -> return lst

-- TODO add a way to discover the path using the environment if dbpath is empty
runNotmuchArrow :: FilePath -- Path to the notmuch database
                -> DatabaseMode
                -> (forall s. NotmuchArrow s a b)
                -> a
                -> IO [b]
runNotmuchArrow dbpath dbmode arrow x =
    withCString dbpath $ \dbpath_str ->
    alloca $ \db_ptr -> do
    statusCode <- c_database_open dbpath_str
                                  (databaseToCDatabaseMode dbmode)
                                  (castPtr db_ptr)
    cdata <- CDatabase <$> peek db_ptr
    if statusCode /= success then throw $ NmE $ statusToErrorCode statusCode
                             else return ()
    result <- ( runWithDatabase (OpenDatabase cdata) arrow x
                   `catch`
                \e -> c_database_destroy cdata >> throw (e :: SomeException)
              )
    _ <- c_database_destroy cdata
    return result

data CompAcc y c1 c2 = FirstRun  c1 c2
                     | MiddleRun c1 y c2

instance Category (NotmuchArrow s) where
    id = NmA (\_ x b -> return $ Right $ if b then Just (x,False) else Nothing)
             True
    (NmA f2 init2) . (NmA f1 init1) = NmA (\db x acc ->
        case acc of
          FirstRun acc1 acc2    -> processf1 db x acc1 acc2
          MiddleRun acc1 y acc2 -> processf2 db x y acc1 acc2
       )
       (FirstRun init1 init2)
      where processf2 db x y nacc1 acc2 =
                f2 db y acc2 >>= \ez ->
                case ez of
                  Left errcode           -> return $ Left errcode
                  Right (Just (z,nacc2)) -> return $ Right
                                          $ Just (z, MiddleRun nacc1 y nacc2)
                  Right Nothing          -> processf1 db x nacc1 init2
            processf1 db x acc1 acc2 =
                f1 db x acc1 >>= \ey ->
                case ey of
                  Left errcode           -> return $ Left errcode
                  Right (Just (y,nacc1)) -> processf2 db x y nacc1 acc2
                  Right Nothing          -> return $ Right Nothing

instance Arrow (NotmuchArrow s) where
    arr f = NmA (\_ x b -> return $ Right $ if b then Just (f x, False) else Nothing)
                True
    (NmA f1 init1) *** (NmA f2 init2) = NmA (\db (x, x') acc ->
        case acc of
          FirstRun acc1 acc2    -> processf1 db x x' acc1 acc2
          MiddleRun acc1 y acc2 -> processf2 db x x' y acc1 acc2
       )
       (FirstRun init1 init2)
      where processf2 db x x' y nacc1 acc2 =
                f2 db x' acc2 >>= \ez ->
                case ez of
                  Left errcode           -> return $ Left errcode
                  Right (Just (z,nacc2)) -> return $ Right
                                          $ Just ((y,z), MiddleRun nacc1 y nacc2)
                  Right Nothing          -> processf1 db x x' nacc1 init2
            processf1 db x x' acc1 acc2 =
                f1 db x acc1 >>= \ey ->
                case ey of
                  Left errcode           -> return $ Left errcode
                  Right (Just (y,nacc1)) -> processf2 db x x' y nacc1 acc2
                  Right Nothing          -> return $ Right Nothing

instance ArrowZero (NotmuchArrow s) where
    zeroArrow = NmA (\_ _ _ -> return $ Right Nothing)
                    ()

instance ArrowPlus (NotmuchArrow s) where
    (NmA f1 init1) <+> (NmA f2 init2) = NmA (\db x c ->
        case c of
          Left  c1 -> processf1 db x c1
          Right c2 -> processf2 db x c2
      )
      (Left init1)
     where processf1 db x c1 =
               f1 db x c1 >>= \ey ->
               case ey of
                 Left errcode           -> return $ Left errcode
                 Right Nothing          -> processf2 db x init2
                 Right (Just (y,nacc1)) -> return $ Right $ Just (y,Left nacc1)
           processf2 db x c2 =
               f2 db x c2 >>= \ey -> return $
               case ey of
                 Left errcode           -> Left errcode
                 Right Nothing          -> Right Nothing
                 Right (Just (y,nacc2)) -> Right $ Just (y,Right nacc2)

instance ArrowChoice (NotmuchArrow s) where
    (NmA f1 init1) +++ (NmA f2 init2) = NmA (\db tag (acc1,acc2) ->
        case tag of
          Left  x -> processf1 db x acc1 acc2
          Right x -> processf2 db x acc1 acc2
      )
      (init1,init2)
     where processf1 db x c1 c2 =
               f1 db x c1 >>= \ey -> return $
               case ey of
                 Left errcode           -> Left errcode
                 Right Nothing          -> Right Nothing
                 Right (Just (y,nacc1)) -> Right $ Just (Left y,(nacc1,c2))
           processf2 db x c1 c2 =
               f2 db x c2 >>= \ey -> return $
               case ey of
                 Left errcode           -> Left errcode
                 Right Nothing          -> Right Nothing
                 Right (Just (y,nacc2)) -> Right $ Just (Right y,(c1,nacc2))

instance ArrowApply (NotmuchArrow s) where
    app = NmA (\db (NmA f initf, x) acc ->
        case acc of
          Nothing            -> processf db x f  initf
          Just (NmA nf facc) -> processf db x nf facc
       )
       Nothing
      where processf db x f acc =
                f db x acc >>= \ez -> return $
                case ez of
                  Left errcode          -> Left errcode
                  Right (Just (y,nacc)) -> Right
                                         $ Just (y, Just (NmA f nacc))
                  Right Nothing         -> Right Nothing


-- TODO make an instance of ArrowLoop

instance ArrowList (NotmuchArrow s) where
    arrL f = NmA (\_ x acc -> return . Right $
        case acc of
          Nothing      -> case f x of
                            []    -> Nothing
                            h : t -> Just (h, Just t)
          Just []      -> Nothing
          Just (h : t) -> Just (h, Just t)
      ) Nothing
    
    isA predic = NmA (\_ x b -> return . Right
                              $ if b && (predic x) then Just (x,False) else Nothing)
                   True

    (NmA f initf) >>. g = NmA (\db x acc -> 
        case acc of
          Nothing      -> do result <- runArrowWith initf (f db x)
                             return $ case fmap g result of
                               Left errcode  -> Left errcode
                               Right []      -> Right Nothing
                               Right (h : t) -> Right $ Just (h, Just t)
          Just []      -> return $ Right Nothing
          Just (h : t) -> return $ Right $ Just (h, Just t)
        ) Nothing
    

