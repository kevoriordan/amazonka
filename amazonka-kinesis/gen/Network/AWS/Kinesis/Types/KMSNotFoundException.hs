{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.KMSNotFoundException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.KMSNotFoundException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The request was rejected because the specified entity or resource can't be found.
--
--
--
-- /See:/ 'kmsNotFoundException' smart constructor.
newtype KMSNotFoundException = KMSNotFoundException'{_knfeMessage
                                                     :: Maybe Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'KMSNotFoundException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'knfeMessage' - A message that provides information about the error.
kmsNotFoundException
    :: KMSNotFoundException
kmsNotFoundException
  = KMSNotFoundException'{_knfeMessage = Nothing}

-- | A message that provides information about the error.
knfeMessage :: Lens' KMSNotFoundException (Maybe Text)
knfeMessage = lens _knfeMessage (\ s a -> s{_knfeMessage = a})

instance FromJSON KMSNotFoundException where
        parseJSON
          = withObject "KMSNotFoundException"
              (\ x -> KMSNotFoundException' <$> (x .:? "message"))

instance Hashable KMSNotFoundException where

instance NFData KMSNotFoundException where
