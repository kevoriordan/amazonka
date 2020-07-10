{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentReference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SegmentReference where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the segment identifier and version of a segment.
--
--
--
-- /See:/ 'segmentReference' smart constructor.
data SegmentReference = SegmentReference'{_srVersion
                                          :: !(Maybe Int),
                                          _srId :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srVersion' - The version number of the segment.
--
-- * 'srId' - The unique identifier for the segment.
segmentReference
    :: Text -- ^ 'srId'
    -> SegmentReference
segmentReference pId_
  = SegmentReference'{_srVersion = Nothing,
                      _srId = pId_}

-- | The version number of the segment.
srVersion :: Lens' SegmentReference (Maybe Int)
srVersion = lens _srVersion (\ s a -> s{_srVersion = a})

-- | The unique identifier for the segment.
srId :: Lens' SegmentReference Text
srId = lens _srId (\ s a -> s{_srId = a})

instance FromJSON SegmentReference where
        parseJSON
          = withObject "SegmentReference"
              (\ x ->
                 SegmentReference' <$>
                   (x .:? "Version") <*> (x .: "Id"))

instance Hashable SegmentReference where

instance NFData SegmentReference where

instance ToJSON SegmentReference where
        toJSON SegmentReference'{..}
          = object
              (catMaybes
                 [("Version" .=) <$> _srVersion,
                  Just ("Id" .= _srId)])
