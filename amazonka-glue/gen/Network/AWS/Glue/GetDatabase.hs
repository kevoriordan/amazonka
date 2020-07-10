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
-- Module      : Network.AWS.Glue.GetDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a specified database.
--
--
module Network.AWS.Glue.GetDatabase
    (
    -- * Creating a Request
      getDatabase
    , GetDatabase
    -- * Request Lenses
    , gtdtbsCatalogId
    , gtdtbsName

    -- * Destructuring the Response
    , getDatabaseResponse
    , GetDatabaseResponse
    -- * Response Lenses
    , gdrsDatabase
    , gdrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDatabase' smart constructor.
data GetDatabase = GetDatabase'{_gtdtbsCatalogId ::
                                !(Maybe Text),
                                _gtdtbsName :: !Text}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtdtbsCatalogId' - The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
--
-- * 'gtdtbsName' - The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
getDatabase
    :: Text -- ^ 'gtdtbsName'
    -> GetDatabase
getDatabase pName_
  = GetDatabase'{_gtdtbsCatalogId = Nothing,
                 _gtdtbsName = pName_}

-- | The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
gtdtbsCatalogId :: Lens' GetDatabase (Maybe Text)
gtdtbsCatalogId = lens _gtdtbsCatalogId (\ s a -> s{_gtdtbsCatalogId = a})

-- | The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
gtdtbsName :: Lens' GetDatabase Text
gtdtbsName = lens _gtdtbsName (\ s a -> s{_gtdtbsName = a})

instance AWSRequest GetDatabase where
        type Rs GetDatabase = GetDatabaseResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetDatabaseResponse' <$>
                   (x .?> "Database") <*> (pure (fromEnum s)))

instance Hashable GetDatabase where

instance NFData GetDatabase where

instance ToHeaders GetDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetDatabase" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDatabase where
        toJSON GetDatabase'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gtdtbsCatalogId,
                  Just ("Name" .= _gtdtbsName)])

instance ToPath GetDatabase where
        toPath = const "/"

instance ToQuery GetDatabase where
        toQuery = const mempty

-- | /See:/ 'getDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'{_gdrsDatabase
                                                :: !(Maybe Database),
                                                _gdrsResponseStatus :: !Int}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDatabase' - The definition of the specified database in the Data Catalog.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDatabaseResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDatabaseResponse
getDatabaseResponse pResponseStatus_
  = GetDatabaseResponse'{_gdrsDatabase = Nothing,
                         _gdrsResponseStatus = pResponseStatus_}

-- | The definition of the specified database in the Data Catalog.
gdrsDatabase :: Lens' GetDatabaseResponse (Maybe Database)
gdrsDatabase = lens _gdrsDatabase (\ s a -> s{_gdrsDatabase = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDatabaseResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDatabaseResponse where
