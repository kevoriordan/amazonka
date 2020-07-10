{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.UiConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provided configuration information for the worker UI for a labeling job. 
--
--
--
-- /See:/ 'uiConfig' smart constructor.
newtype UiConfig = UiConfig'{_ucUiTemplateS3URI ::
                             Text}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UiConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucUiTemplateS3URI' - The Amazon S3 bucket location of the UI template. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
uiConfig
    :: Text -- ^ 'ucUiTemplateS3URI'
    -> UiConfig
uiConfig pUiTemplateS3URI_
  = UiConfig'{_ucUiTemplateS3URI = pUiTemplateS3URI_}

-- | The Amazon S3 bucket location of the UI template. For more information about the contents of a UI template, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates-step2.html Creating Your Custom Labeling Task Template> .
ucUiTemplateS3URI :: Lens' UiConfig Text
ucUiTemplateS3URI = lens _ucUiTemplateS3URI (\ s a -> s{_ucUiTemplateS3URI = a})

instance FromJSON UiConfig where
        parseJSON
          = withObject "UiConfig"
              (\ x -> UiConfig' <$> (x .: "UiTemplateS3Uri"))

instance Hashable UiConfig where

instance NFData UiConfig where

instance ToJSON UiConfig where
        toJSON UiConfig'{..}
          = object
              (catMaybes
                 [Just ("UiTemplateS3Uri" .= _ucUiTemplateS3URI)])
