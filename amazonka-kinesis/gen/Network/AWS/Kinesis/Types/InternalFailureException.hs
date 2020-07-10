{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.InternalFailureException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.InternalFailureException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'internalFailureException' smart constructor.
newtype InternalFailureException = InternalFailureException'{_ifeMessage
                                                             :: Maybe Text}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'InternalFailureException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifeMessage' - Undocumented member.
internalFailureException
    :: InternalFailureException
internalFailureException
  = InternalFailureException'{_ifeMessage = Nothing}

-- | Undocumented member.
ifeMessage :: Lens' InternalFailureException (Maybe Text)
ifeMessage = lens _ifeMessage (\ s a -> s{_ifeMessage = a})

instance FromJSON InternalFailureException where
        parseJSON
          = withObject "InternalFailureException"
              (\ x ->
                 InternalFailureException' <$> (x .:? "message"))

instance Hashable InternalFailureException where

instance NFData InternalFailureException where
