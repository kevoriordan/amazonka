{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ResourceNotFoundException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ResourceNotFoundException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The requested resource could not be found. The stream might not be specified correctly.
--
--
--
-- /See:/ 'resourceNotFoundException' smart constructor.
newtype ResourceNotFoundException = ResourceNotFoundException'{_rnfeMessage
                                                               :: Maybe Text}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'ResourceNotFoundException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnfeMessage' - A message that provides information about the error.
resourceNotFoundException
    :: ResourceNotFoundException
resourceNotFoundException
  = ResourceNotFoundException'{_rnfeMessage = Nothing}

-- | A message that provides information about the error.
rnfeMessage :: Lens' ResourceNotFoundException (Maybe Text)
rnfeMessage = lens _rnfeMessage (\ s a -> s{_rnfeMessage = a})

instance FromJSON ResourceNotFoundException where
        parseJSON
          = withObject "ResourceNotFoundException"
              (\ x ->
                 ResourceNotFoundException' <$> (x .:? "message"))

instance Hashable ResourceNotFoundException where

instance NFData ResourceNotFoundException where
