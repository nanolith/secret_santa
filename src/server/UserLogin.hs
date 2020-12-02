{-# LANGUAGE OverloadedStrings #-}

module UserLogin (
    UserLogin (UserLogin), email
) where

import Control.Monad
import Data.Aeson
import Data.Text

data UserLogin = UserLogin {
    email :: !Text
    }

instance FromJSON UserLogin where
    parseJSON (Object v) =
        UserLogin <$> v .: "email"
    parseJSON _ = mzero

instance ToJSON UserLogin where
    toJSON u =
        object [ "email"    .= email u ]
