{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StartingPosition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.StartingPosition where

import Network.AWS.Kinesis.Types.ShardIteratorType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'startingPosition' smart constructor.
data StartingPosition = StartingPosition'{_spSequenceNumber
                                          :: !(Maybe Text),
                                          _spTimestamp :: !(Maybe POSIX),
                                          _spType :: !ShardIteratorType}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartingPosition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spSequenceNumber' - Undocumented member.
--
-- * 'spTimestamp' - Undocumented member.
--
-- * 'spType' - Undocumented member.
startingPosition
    :: ShardIteratorType -- ^ 'spType'
    -> StartingPosition
startingPosition pType_
  = StartingPosition'{_spSequenceNumber = Nothing,
                      _spTimestamp = Nothing, _spType = pType_}

-- | Undocumented member.
spSequenceNumber :: Lens' StartingPosition (Maybe Text)
spSequenceNumber = lens _spSequenceNumber (\ s a -> s{_spSequenceNumber = a})

-- | Undocumented member.
spTimestamp :: Lens' StartingPosition (Maybe UTCTime)
spTimestamp = lens _spTimestamp (\ s a -> s{_spTimestamp = a}) . mapping _Time

-- | Undocumented member.
spType :: Lens' StartingPosition ShardIteratorType
spType = lens _spType (\ s a -> s{_spType = a})

instance Hashable StartingPosition where

instance NFData StartingPosition where

instance ToJSON StartingPosition where
        toJSON StartingPosition'{..}
          = object
              (catMaybes
                 [("SequenceNumber" .=) <$> _spSequenceNumber,
                  ("Timestamp" .=) <$> _spTimestamp,
                  Just ("Type" .= _spType)])
