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
-- Module      : Network.AWS.Glue.GetUserDefinedFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified function definition from the Data Catalog.
--
--
module Network.AWS.Glue.GetUserDefinedFunction
    (
    -- * Creating a Request
      getUserDefinedFunction
    , GetUserDefinedFunction
    -- * Request Lenses
    , getCatalogId
    , getDatabaseName
    , getFunctionName

    -- * Destructuring the Response
    , getUserDefinedFunctionResponse
    , GetUserDefinedFunctionResponse
    -- * Response Lenses
    , gtusrdfndfnctnrsUserDefinedFunction
    , gtusrdfndfnctnrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUserDefinedFunction' smart constructor.
data GetUserDefinedFunction = GetUserDefinedFunction'{_getCatalogId
                                                      :: !(Maybe Text),
                                                      _getDatabaseName :: !Text,
                                                      _getFunctionName :: !Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'GetUserDefinedFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getCatalogId' - The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
--
-- * 'getDatabaseName' - The name of the catalog database where the function is located.
--
-- * 'getFunctionName' - The name of the function.
getUserDefinedFunction
    :: Text -- ^ 'getDatabaseName'
    -> Text -- ^ 'getFunctionName'
    -> GetUserDefinedFunction
getUserDefinedFunction pDatabaseName_ pFunctionName_
  = GetUserDefinedFunction'{_getCatalogId = Nothing,
                            _getDatabaseName = pDatabaseName_,
                            _getFunctionName = pFunctionName_}

-- | The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
getCatalogId :: Lens' GetUserDefinedFunction (Maybe Text)
getCatalogId = lens _getCatalogId (\ s a -> s{_getCatalogId = a})

-- | The name of the catalog database where the function is located.
getDatabaseName :: Lens' GetUserDefinedFunction Text
getDatabaseName = lens _getDatabaseName (\ s a -> s{_getDatabaseName = a})

-- | The name of the function.
getFunctionName :: Lens' GetUserDefinedFunction Text
getFunctionName = lens _getFunctionName (\ s a -> s{_getFunctionName = a})

instance AWSRequest GetUserDefinedFunction where
        type Rs GetUserDefinedFunction =
             GetUserDefinedFunctionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetUserDefinedFunctionResponse' <$>
                   (x .?> "UserDefinedFunction") <*>
                     (pure (fromEnum s)))

instance Hashable GetUserDefinedFunction where

instance NFData GetUserDefinedFunction where

instance ToHeaders GetUserDefinedFunction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetUserDefinedFunction" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetUserDefinedFunction where
        toJSON GetUserDefinedFunction'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _getCatalogId,
                  Just ("DatabaseName" .= _getDatabaseName),
                  Just ("FunctionName" .= _getFunctionName)])

instance ToPath GetUserDefinedFunction where
        toPath = const "/"

instance ToQuery GetUserDefinedFunction where
        toQuery = const mempty

-- | /See:/ 'getUserDefinedFunctionResponse' smart constructor.
data GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse'{_gtusrdfndfnctnrsUserDefinedFunction
                                                                      ::
                                                                      !(Maybe
                                                                          UserDefinedFunction),
                                                                      _gtusrdfndfnctnrsResponseStatus
                                                                      :: !Int}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'GetUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtusrdfndfnctnrsUserDefinedFunction' - The requested function definition.
--
-- * 'gtusrdfndfnctnrsResponseStatus' - -- | The response status code.
getUserDefinedFunctionResponse
    :: Int -- ^ 'gtusrdfndfnctnrsResponseStatus'
    -> GetUserDefinedFunctionResponse
getUserDefinedFunctionResponse pResponseStatus_
  = GetUserDefinedFunctionResponse'{_gtusrdfndfnctnrsUserDefinedFunction
                                      = Nothing,
                                    _gtusrdfndfnctnrsResponseStatus =
                                      pResponseStatus_}

-- | The requested function definition.
gtusrdfndfnctnrsUserDefinedFunction :: Lens' GetUserDefinedFunctionResponse (Maybe UserDefinedFunction)
gtusrdfndfnctnrsUserDefinedFunction = lens _gtusrdfndfnctnrsUserDefinedFunction (\ s a -> s{_gtusrdfndfnctnrsUserDefinedFunction = a})

-- | -- | The response status code.
gtusrdfndfnctnrsResponseStatus :: Lens' GetUserDefinedFunctionResponse Int
gtusrdfndfnctnrsResponseStatus = lens _gtusrdfndfnctnrsResponseStatus (\ s a -> s{_gtusrdfndfnctnrsResponseStatus = a})

instance NFData GetUserDefinedFunctionResponse where
