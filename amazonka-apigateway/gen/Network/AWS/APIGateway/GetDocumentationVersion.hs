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
-- Module      : Network.AWS.APIGateway.GetDocumentationVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationVersion
    (
    -- * Creating a Request
      getDocumentationVersion
    , GetDocumentationVersion
    -- * Request Lenses
    , gtdcmnttnvrsnRestAPIId
    , gtdcmnttnvrsnDocumentationVersion

    -- * Destructuring the Response
    , documentationVersion
    , DocumentationVersion
    -- * Response Lenses
    , dvCreatedDate
    , dvVersion
    , dvDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Gets a documentation snapshot of an API.
--
--
--
-- /See:/ 'getDocumentationVersion' smart constructor.
data GetDocumentationVersion = GetDocumentationVersion'{_gtdcmnttnvrsnRestAPIId
                                                        :: !Text,
                                                        _gtdcmnttnvrsnDocumentationVersion
                                                        :: !Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'GetDocumentationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtdcmnttnvrsnRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gtdcmnttnvrsnDocumentationVersion' - [Required] The version identifier of the to-be-retrieved documentation snapshot.
getDocumentationVersion
    :: Text -- ^ 'gtdcmnttnvrsnRestAPIId'
    -> Text -- ^ 'gtdcmnttnvrsnDocumentationVersion'
    -> GetDocumentationVersion
getDocumentationVersion pRestAPIId_
  pDocumentationVersion_
  = GetDocumentationVersion'{_gtdcmnttnvrsnRestAPIId =
                               pRestAPIId_,
                             _gtdcmnttnvrsnDocumentationVersion =
                               pDocumentationVersion_}

-- | [Required] The string identifier of the associated 'RestApi' .
gtdcmnttnvrsnRestAPIId :: Lens' GetDocumentationVersion Text
gtdcmnttnvrsnRestAPIId = lens _gtdcmnttnvrsnRestAPIId (\ s a -> s{_gtdcmnttnvrsnRestAPIId = a})

-- | [Required] The version identifier of the to-be-retrieved documentation snapshot.
gtdcmnttnvrsnDocumentationVersion :: Lens' GetDocumentationVersion Text
gtdcmnttnvrsnDocumentationVersion = lens _gtdcmnttnvrsnDocumentationVersion (\ s a -> s{_gtdcmnttnvrsnDocumentationVersion = a})

instance AWSRequest GetDocumentationVersion where
        type Rs GetDocumentationVersion =
             DocumentationVersion
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetDocumentationVersion where

instance NFData GetDocumentationVersion where

instance ToHeaders GetDocumentationVersion where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDocumentationVersion where
        toPath GetDocumentationVersion'{..}
          = mconcat
              ["/restapis/", toBS _gtdcmnttnvrsnRestAPIId,
               "/documentation/versions/",
               toBS _gtdcmnttnvrsnDocumentationVersion]

instance ToQuery GetDocumentationVersion where
        toQuery = const mempty
