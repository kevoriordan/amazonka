{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OutputConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.OutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TargetDevice

-- | Contains information about the output location for the compiled model and the device (target) that the model runs on.
--
--
--
-- /See:/ 'outputConfig' smart constructor.
data OutputConfig = OutputConfig'{_ocS3OutputLocation
                                  :: !Text,
                                  _ocTargetDevice :: !TargetDevice}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocS3OutputLocation' - Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, s3://bucket-name/key-name-prefix.
--
-- * 'ocTargetDevice' - Identifies the device that you want to run your model on after it has been compiled. For example: ml_c5.
outputConfig
    :: Text -- ^ 'ocS3OutputLocation'
    -> TargetDevice -- ^ 'ocTargetDevice'
    -> OutputConfig
outputConfig pS3OutputLocation_ pTargetDevice_
  = OutputConfig'{_ocS3OutputLocation =
                    pS3OutputLocation_,
                  _ocTargetDevice = pTargetDevice_}

-- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, s3://bucket-name/key-name-prefix.
ocS3OutputLocation :: Lens' OutputConfig Text
ocS3OutputLocation = lens _ocS3OutputLocation (\ s a -> s{_ocS3OutputLocation = a})

-- | Identifies the device that you want to run your model on after it has been compiled. For example: ml_c5.
ocTargetDevice :: Lens' OutputConfig TargetDevice
ocTargetDevice = lens _ocTargetDevice (\ s a -> s{_ocTargetDevice = a})

instance FromJSON OutputConfig where
        parseJSON
          = withObject "OutputConfig"
              (\ x ->
                 OutputConfig' <$>
                   (x .: "S3OutputLocation") <*> (x .: "TargetDevice"))

instance Hashable OutputConfig where

instance NFData OutputConfig where

instance ToJSON OutputConfig where
        toJSON OutputConfig'{..}
          = object
              (catMaybes
                 [Just ("S3OutputLocation" .= _ocS3OutputLocation),
                  Just ("TargetDevice" .= _ocTargetDevice)])
