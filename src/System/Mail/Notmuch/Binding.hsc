{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Mail.Notmuch.Binding where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString (ByteString, useAsCString)
import System.IO.Unsafe

#include <notmuch.h>

