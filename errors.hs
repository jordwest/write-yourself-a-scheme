module Errors where

import Types

import Control.Monad.Except

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

