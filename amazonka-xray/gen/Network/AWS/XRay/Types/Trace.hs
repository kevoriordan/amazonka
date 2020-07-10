{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Trace
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.Trace where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.Segment

-- | A collection of segment documents with matching trace IDs.
--
--
--
-- /See:/ 'trace' smart constructor.
data Trace = Trace'{_tId :: !(Maybe Text),
                    _tSegments :: !(Maybe [Segment]),
                    _tDuration :: !(Maybe Double)}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Trace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tId' - The unique identifier for the request that generated the trace's segments and subsegments.
--
-- * 'tSegments' - Segment documents for the segments and subsegments that comprise the trace.
--
-- * 'tDuration' - The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
trace
    :: Trace
trace
  = Trace'{_tId = Nothing, _tSegments = Nothing,
           _tDuration = Nothing}

-- | The unique identifier for the request that generated the trace's segments and subsegments.
tId :: Lens' Trace (Maybe Text)
tId = lens _tId (\ s a -> s{_tId = a})

-- | Segment documents for the segments and subsegments that comprise the trace.
tSegments :: Lens' Trace [Segment]
tSegments = lens _tSegments (\ s a -> s{_tSegments = a}) . _Default . _Coerce

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
tDuration :: Lens' Trace (Maybe Double)
tDuration = lens _tDuration (\ s a -> s{_tDuration = a})

instance FromJSON Trace where
        parseJSON
          = withObject "Trace"
              (\ x ->
                 Trace' <$>
                   (x .:? "Id") <*> (x .:? "Segments" .!= mempty) <*>
                     (x .:? "Duration"))

instance Hashable Trace where

instance NFData Trace where
