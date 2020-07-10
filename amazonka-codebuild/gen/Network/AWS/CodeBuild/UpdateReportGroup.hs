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
-- Module      : Network.AWS.CodeBuild.UpdateReportGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a report group. 
--
--
module Network.AWS.CodeBuild.UpdateReportGroup
    (
    -- * Creating a Request
      updateReportGroup
    , UpdateReportGroup
    -- * Request Lenses
    , urgExportConfig
    , urgArn

    -- * Destructuring the Response
    , updateReportGroupResponse
    , UpdateReportGroupResponse
    -- * Response Lenses
    , urgrsReportGroup
    , urgrsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateReportGroup' smart constructor.
data UpdateReportGroup = UpdateReportGroup'{_urgExportConfig
                                            :: !(Maybe ReportExportConfig),
                                            _urgArn :: !Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateReportGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urgExportConfig' - Used to specify an updated export type. Valid values are:      * @S3@ : The report results are exported to an S3 bucket.      * @NO_EXPORT@ : The report results are not exported. 
--
-- * 'urgArn' - The ARN of the report group to update. 
updateReportGroup
    :: Text -- ^ 'urgArn'
    -> UpdateReportGroup
updateReportGroup pArn_
  = UpdateReportGroup'{_urgExportConfig = Nothing,
                       _urgArn = pArn_}

-- | Used to specify an updated export type. Valid values are:      * @S3@ : The report results are exported to an S3 bucket.      * @NO_EXPORT@ : The report results are not exported. 
urgExportConfig :: Lens' UpdateReportGroup (Maybe ReportExportConfig)
urgExportConfig = lens _urgExportConfig (\ s a -> s{_urgExportConfig = a})

-- | The ARN of the report group to update. 
urgArn :: Lens' UpdateReportGroup Text
urgArn = lens _urgArn (\ s a -> s{_urgArn = a})

instance AWSRequest UpdateReportGroup where
        type Rs UpdateReportGroup = UpdateReportGroupResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 UpdateReportGroupResponse' <$>
                   (x .?> "reportGroup") <*> (pure (fromEnum s)))

instance Hashable UpdateReportGroup where

instance NFData UpdateReportGroup where

instance ToHeaders UpdateReportGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.UpdateReportGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateReportGroup where
        toJSON UpdateReportGroup'{..}
          = object
              (catMaybes
                 [("exportConfig" .=) <$> _urgExportConfig,
                  Just ("arn" .= _urgArn)])

instance ToPath UpdateReportGroup where
        toPath = const "/"

instance ToQuery UpdateReportGroup where
        toQuery = const mempty

-- | /See:/ 'updateReportGroupResponse' smart constructor.
data UpdateReportGroupResponse = UpdateReportGroupResponse'{_urgrsReportGroup
                                                            ::
                                                            !(Maybe
                                                                ReportGroup),
                                                            _urgrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'UpdateReportGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urgrsReportGroup' - Information about the updated report group. 
--
-- * 'urgrsResponseStatus' - -- | The response status code.
updateReportGroupResponse
    :: Int -- ^ 'urgrsResponseStatus'
    -> UpdateReportGroupResponse
updateReportGroupResponse pResponseStatus_
  = UpdateReportGroupResponse'{_urgrsReportGroup =
                                 Nothing,
                               _urgrsResponseStatus = pResponseStatus_}

-- | Information about the updated report group. 
urgrsReportGroup :: Lens' UpdateReportGroupResponse (Maybe ReportGroup)
urgrsReportGroup = lens _urgrsReportGroup (\ s a -> s{_urgrsReportGroup = a})

-- | -- | The response status code.
urgrsResponseStatus :: Lens' UpdateReportGroupResponse Int
urgrsResponseStatus = lens _urgrsResponseStatus (\ s a -> s{_urgrsResponseStatus = a})

instance NFData UpdateReportGroupResponse where
