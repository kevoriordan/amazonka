{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.KMSInvalidStateException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.KMSInvalidStateException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The request was rejected because the state of the specified resource isn't valid for this request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
--
--
-- /See:/ 'kmsInvalidStateException' smart constructor.
newtype KMSInvalidStateException = KMSInvalidStateException'{_kiseMessage
                                                             :: Maybe Text}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'KMSInvalidStateException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kiseMessage' - A message that provides information about the error.
kmsInvalidStateException
    :: KMSInvalidStateException
kmsInvalidStateException
  = KMSInvalidStateException'{_kiseMessage = Nothing}

-- | A message that provides information about the error.
kiseMessage :: Lens' KMSInvalidStateException (Maybe Text)
kiseMessage = lens _kiseMessage (\ s a -> s{_kiseMessage = a})

instance FromJSON KMSInvalidStateException where
        parseJSON
          = withObject "KMSInvalidStateException"
              (\ x ->
                 KMSInvalidStateException' <$> (x .:? "message"))

instance Hashable KMSInvalidStateException where

instance NFData KMSInvalidStateException where
