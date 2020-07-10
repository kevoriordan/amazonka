{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SharingSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SharingSettings where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.NotebookOutputOption

-- | The sharing settings.
--
--
--
-- /See:/ 'sharingSettings' smart constructor.
data SharingSettings = SharingSettings'{_ssS3KMSKeyId
                                        :: !(Maybe Text),
                                        _ssS3OutputPath :: !(Maybe Text),
                                        _ssNotebookOutputOption ::
                                        !(Maybe NotebookOutputOption)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SharingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssS3KMSKeyId' - The AWS Key Management Service encryption key ID.
--
-- * 'ssS3OutputPath' - The Amazon S3 output path.
--
-- * 'ssNotebookOutputOption' - The notebook output option.
sharingSettings
    :: SharingSettings
sharingSettings
  = SharingSettings'{_ssS3KMSKeyId = Nothing,
                     _ssS3OutputPath = Nothing,
                     _ssNotebookOutputOption = Nothing}

-- | The AWS Key Management Service encryption key ID.
ssS3KMSKeyId :: Lens' SharingSettings (Maybe Text)
ssS3KMSKeyId = lens _ssS3KMSKeyId (\ s a -> s{_ssS3KMSKeyId = a})

-- | The Amazon S3 output path.
ssS3OutputPath :: Lens' SharingSettings (Maybe Text)
ssS3OutputPath = lens _ssS3OutputPath (\ s a -> s{_ssS3OutputPath = a})

-- | The notebook output option.
ssNotebookOutputOption :: Lens' SharingSettings (Maybe NotebookOutputOption)
ssNotebookOutputOption = lens _ssNotebookOutputOption (\ s a -> s{_ssNotebookOutputOption = a})

instance FromJSON SharingSettings where
        parseJSON
          = withObject "SharingSettings"
              (\ x ->
                 SharingSettings' <$>
                   (x .:? "S3KmsKeyId") <*> (x .:? "S3OutputPath") <*>
                     (x .:? "NotebookOutputOption"))

instance Hashable SharingSettings where

instance NFData SharingSettings where

instance ToJSON SharingSettings where
        toJSON SharingSettings'{..}
          = object
              (catMaybes
                 [("S3KmsKeyId" .=) <$> _ssS3KMSKeyId,
                  ("S3OutputPath" .=) <$> _ssS3OutputPath,
                  ("NotebookOutputOption" .=) <$>
                    _ssNotebookOutputOption])
