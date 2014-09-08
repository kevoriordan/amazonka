{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GenerateCredentialReport
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates a credential report for the AWS account. For more information
-- about the credential report, see Getting Credential Reports in the Using
-- IAM guide.
module Network.AWS.IAM.V2010_05_08.GenerateCredentialReport
    (
    -- * Request
      GenerateCredentialReport
    -- ** Request constructor
    , mkGenerateCredentialReport
    -- * Response
    , GenerateCredentialReportResponse
    -- ** Response constructor
    , mkGenerateCredentialReportResponse
    -- ** Response lenses
    , gcrrState
    , gcrrDescription
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data GenerateCredentialReport = GenerateCredentialReport
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GenerateCredentialReport' request.
mkGenerateCredentialReport :: GenerateCredentialReport
mkGenerateCredentialReport = GenerateCredentialReport

instance ToQuery GenerateCredentialReport where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- GenerateCredentialReport action.
data GenerateCredentialReportResponse = GenerateCredentialReportResponse
    { _gcrrState :: Maybe ReportStateType
    , _gcrrDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GenerateCredentialReportResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkGenerateCredentialReportResponse :: GenerateCredentialReportResponse
mkGenerateCredentialReportResponse = GenerateCredentialReportResponse
    { _gcrrState = Nothing
    , _gcrrDescription = Nothing
    }

-- | Information about the state of a credential report.
gcrrState :: Lens' GenerateCredentialReportResponse (Maybe ReportStateType)
gcrrState = lens _gcrrState (\s a -> s { _gcrrState = a })

-- | Information about the credential report.
gcrrDescription :: Lens' GenerateCredentialReportResponse (Maybe Text)
gcrrDescription = lens _gcrrDescription (\s a -> s { _gcrrDescription = a })

instance FromXML GenerateCredentialReportResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GenerateCredentialReport where
    type Sv GenerateCredentialReport = IAM
    type Rs GenerateCredentialReport = GenerateCredentialReportResponse

    request = post "GenerateCredentialReport"
    response _ = xmlResponse
