{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V0 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

-- TODO: not sure about some of the primary keys, but the ones i picked at least need to be indices.

migration :: Migration
migration = Migration 0 "Initial schema" $ do
    void $ schema' [r|
        create columnfamily if not exists user
            ( id       uuid
            , idp_name text
            , account  text
            , primary key (idp_name, account) -- this is the primarcy handle in the SAML world.
            );
        |]

    {- this will be needed later, but for now we will get all the information in here from the config.
    void $ schema' [r|
        create columnfamily if not exists idp
            ( idp      uuid
            , idp_name text
            , owners    list<uuid>
            -- ...
            , primary key (idp_name)
            );
        |]
    -}
