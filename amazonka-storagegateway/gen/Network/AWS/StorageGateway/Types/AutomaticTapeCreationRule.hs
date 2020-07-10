{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An automatic tape creation policy consists of automatic tape creation rules where each rule defines when and how to create new tapes.
--
--
--
-- /See:/ 'automaticTapeCreationRule' smart constructor.
data AutomaticTapeCreationRule = AutomaticTapeCreationRule'{_atcrTapeBarcodePrefix
                                                            :: !Text,
                                                            _atcrPoolId ::
                                                            !Text,
                                                            _atcrTapeSizeInBytes
                                                            :: !Integer,
                                                            _atcrMinimumNumTapes
                                                            :: !Nat}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'AutomaticTapeCreationRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atcrTapeBarcodePrefix' - A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
--
-- * 'atcrPoolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid values: "GLACIER", "DEEP_ARCHIVE"
--
-- * 'atcrTapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
--
-- * 'atcrMinimumNumTapes' - The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway.
automaticTapeCreationRule
    :: Text -- ^ 'atcrTapeBarcodePrefix'
    -> Text -- ^ 'atcrPoolId'
    -> Integer -- ^ 'atcrTapeSizeInBytes'
    -> Natural -- ^ 'atcrMinimumNumTapes'
    -> AutomaticTapeCreationRule
automaticTapeCreationRule pTapeBarcodePrefix_
  pPoolId_ pTapeSizeInBytes_ pMinimumNumTapes_
  = AutomaticTapeCreationRule'{_atcrTapeBarcodePrefix =
                                 pTapeBarcodePrefix_,
                               _atcrPoolId = pPoolId_,
                               _atcrTapeSizeInBytes = pTapeSizeInBytes_,
                               _atcrMinimumNumTapes = _Nat # pMinimumNumTapes_}

-- | A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
atcrTapeBarcodePrefix :: Lens' AutomaticTapeCreationRule Text
atcrTapeBarcodePrefix = lens _atcrTapeBarcodePrefix (\ s a -> s{_atcrTapeBarcodePrefix = a})

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid values: "GLACIER", "DEEP_ARCHIVE"
atcrPoolId :: Lens' AutomaticTapeCreationRule Text
atcrPoolId = lens _atcrPoolId (\ s a -> s{_atcrPoolId = a})

-- | The size, in bytes, of the virtual tape capacity.
atcrTapeSizeInBytes :: Lens' AutomaticTapeCreationRule Integer
atcrTapeSizeInBytes = lens _atcrTapeSizeInBytes (\ s a -> s{_atcrTapeSizeInBytes = a})

-- | The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway.
atcrMinimumNumTapes :: Lens' AutomaticTapeCreationRule Natural
atcrMinimumNumTapes = lens _atcrMinimumNumTapes (\ s a -> s{_atcrMinimumNumTapes = a}) . _Nat

instance FromJSON AutomaticTapeCreationRule where
        parseJSON
          = withObject "AutomaticTapeCreationRule"
              (\ x ->
                 AutomaticTapeCreationRule' <$>
                   (x .: "TapeBarcodePrefix") <*> (x .: "PoolId") <*>
                     (x .: "TapeSizeInBytes")
                     <*> (x .: "MinimumNumTapes"))

instance Hashable AutomaticTapeCreationRule where

instance NFData AutomaticTapeCreationRule where

instance ToJSON AutomaticTapeCreationRule where
        toJSON AutomaticTapeCreationRule'{..}
          = object
              (catMaybes
                 [Just
                    ("TapeBarcodePrefix" .= _atcrTapeBarcodePrefix),
                  Just ("PoolId" .= _atcrPoolId),
                  Just ("TapeSizeInBytes" .= _atcrTapeSizeInBytes),
                  Just ("MinimumNumTapes" .= _atcrMinimumNumTapes)])
