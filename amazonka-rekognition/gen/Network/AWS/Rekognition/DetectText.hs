{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectText
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects text in the input image and converts it into machine-readable text.
--
--
-- Pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, you must pass it as a reference to an image in an Amazon S3 bucket. For the AWS CLI, passing image bytes is not supported. The image must be either a .png or .jpeg formatted file. 
--
-- The @DetectText@ operation returns text in an array of 'TextDetection' elements, @TextDetections@ . Each @TextDetection@ element provides information about a single word or line of text that was detected in the image. 
--
-- A word is one or more ISO basic latin script characters that are not separated by spaces. @DetectText@ can detect up to 50 words in an image.
--
-- A line is a string of equally spaced words. A line isn't necessarily a complete sentence. For example, a driver's license number is detected as a line. A line ends when there is no aligned text after it. Also, a line ends when there is a large gap between words, relative to the length of the words. This means, depending on the gap between words, Amazon Rekognition may detect multiple lines in text aligned in the same direction. Periods don't represent the end of a line. If a sentence spans multiple lines, the @DetectText@ operation returns multiple lines.
--
-- To determine whether a @TextDetection@ element is a line of text or a word, use the @TextDetection@ object @Type@ field. 
--
-- To be detected, text must be within +/- 90 degrees orientation of the horizontal axis.
--
-- For more information, see DetectText in the Amazon Rekognition Developer Guide.
--
module Network.AWS.Rekognition.DetectText
    (
    -- * Creating a Request
      detectText
    , DetectText
    -- * Request Lenses
    , dtFilters
    , dtImage

    -- * Destructuring the Response
    , detectTextResponse
    , DetectTextResponse
    -- * Response Lenses
    , dtrsTextDetections
    , dtrsTextModelVersion
    , dtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectText' smart constructor.
data DetectText = DetectText'{_dtFilters ::
                              !(Maybe DetectTextFilters),
                              _dtImage :: !Image}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtFilters' - Optional parameters that let you set the criteria that the text must meet to be included in your response.
--
-- * 'dtImage' - The input image as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes.  If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
detectText
    :: Image -- ^ 'dtImage'
    -> DetectText
detectText pImage_
  = DetectText'{_dtFilters = Nothing,
                _dtImage = pImage_}

-- | Optional parameters that let you set the criteria that the text must meet to be included in your response.
dtFilters :: Lens' DetectText (Maybe DetectTextFilters)
dtFilters = lens _dtFilters (\ s a -> s{_dtFilters = a})

-- | The input image as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes.  If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
dtImage :: Lens' DetectText Image
dtImage = lens _dtImage (\ s a -> s{_dtImage = a})

instance AWSRequest DetectText where
        type Rs DetectText = DetectTextResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 DetectTextResponse' <$>
                   (x .?> "TextDetections" .!@ mempty) <*>
                     (x .?> "TextModelVersion")
                     <*> (pure (fromEnum s)))

instance Hashable DetectText where

instance NFData DetectText where

instance ToHeaders DetectText where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DetectText" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectText where
        toJSON DetectText'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dtFilters,
                  Just ("Image" .= _dtImage)])

instance ToPath DetectText where
        toPath = const "/"

instance ToQuery DetectText where
        toQuery = const mempty

-- | /See:/ 'detectTextResponse' smart constructor.
data DetectTextResponse = DetectTextResponse'{_dtrsTextDetections
                                              :: !(Maybe [TextDetection]),
                                              _dtrsTextModelVersion ::
                                              !(Maybe Text),
                                              _dtrsResponseStatus :: !Int}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectTextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsTextDetections' - An array of text that was detected in the input image.
--
-- * 'dtrsTextModelVersion' - The model version used to detect text.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
detectTextResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DetectTextResponse
detectTextResponse pResponseStatus_
  = DetectTextResponse'{_dtrsTextDetections = Nothing,
                        _dtrsTextModelVersion = Nothing,
                        _dtrsResponseStatus = pResponseStatus_}

-- | An array of text that was detected in the input image.
dtrsTextDetections :: Lens' DetectTextResponse [TextDetection]
dtrsTextDetections = lens _dtrsTextDetections (\ s a -> s{_dtrsTextDetections = a}) . _Default . _Coerce

-- | The model version used to detect text.
dtrsTextModelVersion :: Lens' DetectTextResponse (Maybe Text)
dtrsTextModelVersion = lens _dtrsTextModelVersion (\ s a -> s{_dtrsTextModelVersion = a})

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DetectTextResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DetectTextResponse where
