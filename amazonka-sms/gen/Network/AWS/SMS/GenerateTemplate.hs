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
-- Module      : Network.AWS.SMS.GenerateTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates an Amazon CloudFormation template based on the current launch configuration and writes it to an Amazon S3 object in the customer’s Amazon S3 bucket.
--
--
module Network.AWS.SMS.GenerateTemplate
    (
    -- * Creating a Request
      generateTemplate
    , GenerateTemplate
    -- * Request Lenses
    , gtAppId
    , gtTemplateFormat

    -- * Destructuring the Response
    , generateTemplateResponse
    , GenerateTemplateResponse
    -- * Response Lenses
    , gtrsS3Location
    , gtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'generateTemplate' smart constructor.
data GenerateTemplate = GenerateTemplate'{_gtAppId ::
                                          !(Maybe Text),
                                          _gtTemplateFormat ::
                                          !(Maybe OutputFormat)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtAppId' - ID of the application associated with the Amazon CloudFormation template.
--
-- * 'gtTemplateFormat' - Format for generating the Amazon CloudFormation template.
generateTemplate
    :: GenerateTemplate
generateTemplate
  = GenerateTemplate'{_gtAppId = Nothing,
                      _gtTemplateFormat = Nothing}

-- | ID of the application associated with the Amazon CloudFormation template.
gtAppId :: Lens' GenerateTemplate (Maybe Text)
gtAppId = lens _gtAppId (\ s a -> s{_gtAppId = a})

-- | Format for generating the Amazon CloudFormation template.
gtTemplateFormat :: Lens' GenerateTemplate (Maybe OutputFormat)
gtTemplateFormat = lens _gtTemplateFormat (\ s a -> s{_gtTemplateFormat = a})

instance AWSRequest GenerateTemplate where
        type Rs GenerateTemplate = GenerateTemplateResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 GenerateTemplateResponse' <$>
                   (x .?> "s3Location") <*> (pure (fromEnum s)))

instance Hashable GenerateTemplate where

instance NFData GenerateTemplate where

instance ToHeaders GenerateTemplate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.GenerateTemplate"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GenerateTemplate where
        toJSON GenerateTemplate'{..}
          = object
              (catMaybes
                 [("appId" .=) <$> _gtAppId,
                  ("templateFormat" .=) <$> _gtTemplateFormat])

instance ToPath GenerateTemplate where
        toPath = const "/"

instance ToQuery GenerateTemplate where
        toQuery = const mempty

-- | /See:/ 'generateTemplateResponse' smart constructor.
data GenerateTemplateResponse = GenerateTemplateResponse'{_gtrsS3Location
                                                          ::
                                                          !(Maybe S3Location),
                                                          _gtrsResponseStatus ::
                                                          !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'GenerateTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsS3Location' - Location of the Amazon S3 object.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
generateTemplateResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GenerateTemplateResponse
generateTemplateResponse pResponseStatus_
  = GenerateTemplateResponse'{_gtrsS3Location =
                                Nothing,
                              _gtrsResponseStatus = pResponseStatus_}

-- | Location of the Amazon S3 object.
gtrsS3Location :: Lens' GenerateTemplateResponse (Maybe S3Location)
gtrsS3Location = lens _gtrsS3Location (\ s a -> s{_gtrsS3Location = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GenerateTemplateResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GenerateTemplateResponse where
