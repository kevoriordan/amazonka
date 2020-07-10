{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @Table@ definition in a Data Catalog for a specified table.
--
--
module Network.AWS.Glue.GetTable
    (
    -- * Creating a Request
      getTable
    , GetTable
    -- * Request Lenses
    , gttblCatalogId
    , gttblDatabaseName
    , gttblName

    -- * Destructuring the Response
    , getTableResponse
    , GetTableResponse
    -- * Response Lenses
    , gttblrsTable
    , gttblrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTable' smart constructor.
data GetTable = GetTable'{_gttblCatalogId ::
                          !(Maybe Text),
                          _gttblDatabaseName :: !Text, _gttblName :: !Text}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gttblCatalogId' - The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- * 'gttblDatabaseName' - The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- * 'gttblName' - The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
getTable
    :: Text -- ^ 'gttblDatabaseName'
    -> Text -- ^ 'gttblName'
    -> GetTable
getTable pDatabaseName_ pName_
  = GetTable'{_gttblCatalogId = Nothing,
              _gttblDatabaseName = pDatabaseName_,
              _gttblName = pName_}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
gttblCatalogId :: Lens' GetTable (Maybe Text)
gttblCatalogId = lens _gttblCatalogId (\ s a -> s{_gttblCatalogId = a})

-- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
gttblDatabaseName :: Lens' GetTable Text
gttblDatabaseName = lens _gttblDatabaseName (\ s a -> s{_gttblDatabaseName = a})

-- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
gttblName :: Lens' GetTable Text
gttblName = lens _gttblName (\ s a -> s{_gttblName = a})

instance AWSRequest GetTable where
        type Rs GetTable = GetTableResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetTableResponse' <$>
                   (x .?> "Table") <*> (pure (fromEnum s)))

instance Hashable GetTable where

instance NFData GetTable where

instance ToHeaders GetTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTable where
        toJSON GetTable'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gttblCatalogId,
                  Just ("DatabaseName" .= _gttblDatabaseName),
                  Just ("Name" .= _gttblName)])

instance ToPath GetTable where
        toPath = const "/"

instance ToQuery GetTable where
        toQuery = const mempty

-- | /See:/ 'getTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'{_gttblrsTable
                                          :: !(Maybe Table),
                                          _gttblrsResponseStatus :: !Int}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gttblrsTable' - The @Table@ object that defines the specified table.
--
-- * 'gttblrsResponseStatus' - -- | The response status code.
getTableResponse
    :: Int -- ^ 'gttblrsResponseStatus'
    -> GetTableResponse
getTableResponse pResponseStatus_
  = GetTableResponse'{_gttblrsTable = Nothing,
                      _gttblrsResponseStatus = pResponseStatus_}

-- | The @Table@ object that defines the specified table.
gttblrsTable :: Lens' GetTableResponse (Maybe Table)
gttblrsTable = lens _gttblrsTable (\ s a -> s{_gttblrsTable = a})

-- | -- | The response status code.
gttblrsResponseStatus :: Lens' GetTableResponse Int
gttblrsResponseStatus = lens _gttblrsResponseStatus (\ s a -> s{_gttblrsResponseStatus = a})

instance NFData GetTableResponse where
