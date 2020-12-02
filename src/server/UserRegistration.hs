{-# LANGUAGE OverloadedStrings #-}

module UserRegistration (
    UserRegistration (UserRegistration), email, name, salt, pubkey, iv, encdata,
    mac
) where

import Control.Monad
import Data.Aeson
import Data.Text

data UserRegistration = UserRegistration {
    email :: !Text,
    name :: !Text,
    salt :: !Text,
    pubkey :: !Text,
    iv :: !Text,
    encdata :: !Text,
    mac :: !Text
    }

instance FromJSON UserRegistration where
    parseJSON (Object v) =
        UserRegistration <$> v .: "email"
                         <*> v .: "name"
                         <*> v .: "salt"
                         <*> v .: "pubkey"
                         <*> v .: "iv"
                         <*> v .: "encdata"
                         <*> v .: "mac"
    parseJSON _ = mzero

instance ToJSON UserRegistration where
    toJSON u =
        object [ "email"   .= email u,
                 "name"    .= name u,
                 "salt"    .= salt u,
                 "pubkey"  .= pubkey u,
                 "iv"      .= iv u,
                 "encdata" .= encdata u,
                 "mac"     .= mac u ]
