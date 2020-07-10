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
-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the bandwidth rate limits of a gateway. You can delete either the upload and download bandwidth rate limit, or you can delete both. If you delete only one of the limits, the other limit remains unchanged. To specify which gateway to work with, use the Amazon Resource Name (ARN) of the gateway in your request. This operation is supported for the stored volume, cached volume and tape gateway types.
--
--
module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
    (
    -- * Creating a Request
      deleteBandwidthRateLimit
    , DeleteBandwidthRateLimit
    -- * Request Lenses
    , dltbndwdthrtlmtGatewayARN
    , dltbndwdthrtlmtBandwidthType

    -- * Destructuring the Response
    , deleteBandwidthRateLimitResponse
    , DeleteBandwidthRateLimitResponse
    -- * Response Lenses
    , delrsGatewayARN
    , delrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the following fields:
--
--
--     * 'DeleteBandwidthRateLimitInput$BandwidthType' 
--
--
--
--
-- /See:/ 'deleteBandwidthRateLimit' smart constructor.
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'{_dltbndwdthrtlmtGatewayARN
                                                          :: !Text,
                                                          _dltbndwdthrtlmtBandwidthType
                                                          :: !Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DeleteBandwidthRateLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltbndwdthrtlmtGatewayARN' - Undocumented member.
--
-- * 'dltbndwdthrtlmtBandwidthType' - One of the BandwidthType values that indicates the gateway bandwidth rate limit to delete. Valid Values: @Upload@ , @Download@ , @All@ .
deleteBandwidthRateLimit
    :: Text -- ^ 'dltbndwdthrtlmtGatewayARN'
    -> Text -- ^ 'dltbndwdthrtlmtBandwidthType'
    -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit pGatewayARN_ pBandwidthType_
  = DeleteBandwidthRateLimit'{_dltbndwdthrtlmtGatewayARN
                                = pGatewayARN_,
                              _dltbndwdthrtlmtBandwidthType = pBandwidthType_}

-- | Undocumented member.
dltbndwdthrtlmtGatewayARN :: Lens' DeleteBandwidthRateLimit Text
dltbndwdthrtlmtGatewayARN = lens _dltbndwdthrtlmtGatewayARN (\ s a -> s{_dltbndwdthrtlmtGatewayARN = a})

-- | One of the BandwidthType values that indicates the gateway bandwidth rate limit to delete. Valid Values: @Upload@ , @Download@ , @All@ .
dltbndwdthrtlmtBandwidthType :: Lens' DeleteBandwidthRateLimit Text
dltbndwdthrtlmtBandwidthType = lens _dltbndwdthrtlmtBandwidthType (\ s a -> s{_dltbndwdthrtlmtBandwidthType = a})

instance AWSRequest DeleteBandwidthRateLimit where
        type Rs DeleteBandwidthRateLimit =
             DeleteBandwidthRateLimitResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBandwidthRateLimitResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable DeleteBandwidthRateLimit where

instance NFData DeleteBandwidthRateLimit where

instance ToHeaders DeleteBandwidthRateLimit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteBandwidthRateLimit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteBandwidthRateLimit where
        toJSON DeleteBandwidthRateLimit'{..}
          = object
              (catMaybes
                 [Just ("GatewayARN" .= _dltbndwdthrtlmtGatewayARN),
                  Just
                    ("BandwidthType" .= _dltbndwdthrtlmtBandwidthType)])

instance ToPath DeleteBandwidthRateLimit where
        toPath = const "/"

instance ToQuery DeleteBandwidthRateLimit where
        toQuery = const mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway whose bandwidth rate information was deleted.
--
--
--
-- /See:/ 'deleteBandwidthRateLimitResponse' smart constructor.
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'{_delrsGatewayARN
                                                                          ::
                                                                          !(Maybe
                                                                              Text),
                                                                          _delrsResponseStatus
                                                                          ::
                                                                          !Int}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'DeleteBandwidthRateLimitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsGatewayARN' - Undocumented member.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteBandwidthRateLimitResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse pResponseStatus_
  = DeleteBandwidthRateLimitResponse'{_delrsGatewayARN
                                        = Nothing,
                                      _delrsResponseStatus = pResponseStatus_}

-- | Undocumented member.
delrsGatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
delrsGatewayARN = lens _delrsGatewayARN (\ s a -> s{_delrsGatewayARN = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteBandwidthRateLimitResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteBandwidthRateLimitResponse
         where
