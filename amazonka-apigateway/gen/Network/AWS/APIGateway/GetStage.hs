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
-- Module      : Network.AWS.APIGateway.GetStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Stage' resource.
--
--
module Network.AWS.APIGateway.GetStage
    (
    -- * Creating a Request
      getStage
    , GetStage
    -- * Request Lenses
    , gtstgRestAPIId
    , gtstgStageName

    -- * Destructuring the Response
    , stage
    , Stage
    -- * Response Lenses
    , sDeploymentId
    , sVariables
    , sAccessLogSettings
    , sDocumentationVersion
    , sClientCertificateId
    , sTracingEnabled
    , sCreatedDate
    , sCacheClusterStatus
    , sMethodSettings
    , sLastUpdatedDate
    , sCacheClusterSize
    , sWebACLARN
    , sCanarySettings
    , sCacheClusterEnabled
    , sStageName
    , sDescription
    , sTags
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to get information about a 'Stage' resource.
--
--
--
-- /See:/ 'getStage' smart constructor.
data GetStage = GetStage'{_gtstgRestAPIId :: !Text,
                          _gtstgStageName :: !Text}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtstgRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gtstgStageName' - [Required] The name of the 'Stage' resource to get information about.
getStage
    :: Text -- ^ 'gtstgRestAPIId'
    -> Text -- ^ 'gtstgStageName'
    -> GetStage
getStage pRestAPIId_ pStageName_
  = GetStage'{_gtstgRestAPIId = pRestAPIId_,
              _gtstgStageName = pStageName_}

-- | [Required] The string identifier of the associated 'RestApi' .
gtstgRestAPIId :: Lens' GetStage Text
gtstgRestAPIId = lens _gtstgRestAPIId (\ s a -> s{_gtstgRestAPIId = a})

-- | [Required] The name of the 'Stage' resource to get information about.
gtstgStageName :: Lens' GetStage Text
gtstgStageName = lens _gtstgStageName (\ s a -> s{_gtstgStageName = a})

instance AWSRequest GetStage where
        type Rs GetStage = Stage
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetStage where

instance NFData GetStage where

instance ToHeaders GetStage where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetStage where
        toPath GetStage'{..}
          = mconcat
              ["/restapis/", toBS _gtstgRestAPIId, "/stages/",
               toBS _gtstgStageName]

instance ToQuery GetStage where
        toQuery = const mempty
