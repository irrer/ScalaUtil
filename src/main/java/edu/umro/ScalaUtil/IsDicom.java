/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ScalaUtil;

import edu.umro.util.Utility;

import java.io.*;

/**
 * This code was lifted from the Pixelmed toolkit.  It adds the functionality of being able to
 * determine if something is a DICOM file using a stream or byte array as input.
 */
public class IsDicom {

    // derived from BinaryInputStream.extractUnsigned16()
    private final static int extractUnsigned16(byte[] buffer, int offset, boolean bigEndian) {
        short v1 = (short) (buffer[offset + 0] & 0xff);
        short v2 = (short) (buffer[offset + 1] & 0xff);
        return (short) (bigEndian
                ? (v1 << 8) | v2
                : (v2 << 8) | v1);
    }


    // derived from BinaryInputStream.extractUnsigned32()
    private final static long extractUnsigned32(byte[] buffer, int offset, boolean bigEndian) {
        long v1 = ((long) buffer[offset + 0]) & 0xff;
        long v2 = ((long) buffer[offset + 1]) & 0xff;
        long v3 = ((long) buffer[offset + 2]) & 0xff;
        long v4 = ((long) buffer[offset + 3]) & 0xff;
        return bigEndian
                ? (((((v1 << 8) | v2) << 8) | v3) << 8) | v4
                : (((((v4 << 8) | v3) << 8) | v2) << 8) | v1;
    }


    /**
     * <p>Does the file contain a DICOM (or DICOM-like ACR-NEMA) dataset ?</p>
     *
     * <p>Any exceptions during attempts to read are (silently) caught and false returned.</p>
     *
     * <p>Note that this method may return true but {@link com.pixelmed.dicom.DicomInputStream DicomInputStream} and {@link com.pixelmed.dicom.AttributeList AttributeList} may fail
     * to read the file, since it may be "bad" ways that are not supported, e.g., missing TransferSyntaxUID in meta information header, etc.</p>
     *
     * <p>Will detect files with:</p>
     * <ul>
     * <li>PS 3.10 meta-header (even if in invalid big-endian transfer syntax and even if 1st meta information header attribute is not group length)</li>
     * <li>no meta-header but in little or big endian and explicit or implicit VR starting with a group 0x0008 attribute element &lt;= 0x0018 (SOPInstanceUID)</li>
     * </ul>
     * <p>Will reject everything else, including files with:</p>
     * <ul>
     * <li>no meta-header and not starting with a group 0x0008 attribute, e.g. that have command (group 0x0000) elements at the start of the dataset</li>
     * </ul>
     *
     * @param inputStream possibly DICOM data
     * @return true if file exists, can be read, and seems to contain a DICOM or ACR-NEMA dataset (with or without a PS 3.10 preamble and meta information header)
     */
    public static boolean isDicomOrAcrNema(InputStream inputStream) {
        boolean success = false;
        java.io.FileInputStream fi = null;
        try {
            InputStream in = new BufferedInputStream(inputStream);
            byte[] b = new byte[160];
            int length = 0;
            {                                            // derived from BinaryInputStream.readInsistently()
                int wanted = 160;
                while (wanted > 0) {
                    int got = in.read(b, length, wanted);
                    if (got == -1) break;
                    wanted -= got;
                    length += got;
                }
            }
            inputStream.close();
            inputStream = null;

            if (length >= 136 && new String(b, 128, 4).equals("DICM") && (extractUnsigned16(b, 132, false) == 0x0002 || extractUnsigned16(b, 132, false) == 0x0000)) {    // do NOT insist on group length (bad example dicomvl.imagAAAa0005r.dc3); allow group 0x0000 command elements before meta information elements (001132)
                success = true;
            } else if (length >= 136 && new String(b, 128, 4).equals("DICM") && extractUnsigned16(b, 132, true) == 0x0002 /*&& extractUnsigned16(b,134,true) == 0x0000*/) {    // big endian metaheader is illegal but allow it (bad example dicomvl.fich1.dcm)
                success = true;
            } else if (length >= 8
                    && extractUnsigned16(b, 0, false) == 0x0008
                    && extractUnsigned16(b, 2, false) <= 0x0018 /* SOPInstanceUID */
                    && (extractUnsigned32(b, 4, false) <= 0x0100 /* sane VL */ || (Character.isUpperCase((char) (b[4])) && Character.isUpperCase((char) (b[5]))) /* EVR */)
            ) {
                success = true;
            } else if (length >= 8
                    && extractUnsigned16(b, 0, true) == 0x0008
                    && extractUnsigned16(b, 2, true) <= 0x0018 /* SOPInstanceUID */
                    && (extractUnsigned32(b, 4, true) <= 0x0100 /* sane VL */ || (Character.isUpperCase((char) (b[4])) && Character.isUpperCase((char) (b[5]))) /* EVR */)
            ) {
                success = true;
            }
            // do not check for start with command group (e.g. acrnema/xpress/test.inf) (001134)
        } catch (Exception e) {
            success = false;
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
            } catch (IOException ioe) {
            }
        }
        return success;
    }

    public static boolean isDicomOrAcrNema(byte[] data) {
        return isDicomOrAcrNema(new ByteArrayInputStream(data));
    }

    public static void main(String[] args) {
        if (args.length > -1) {
            String fileName = "D:\\tmp\\aqa\\Phase2\\DICOM\\umich\\14\\$JM_AQA_phase2_v000_RTIMAGE_1__0005.DCM";
            File file = new File(fileName);
            byte[] data = Utility.readBinFile(file);
            System.out.println(file.getAbsolutePath() + " : " + isDicomOrAcrNema(data));
        }

        if (args.length > -1) {
            String fileName = "D:\\tmp\\index.xml";
            File file = new File(fileName);
            byte[] data = Utility.readBinFile(file);
            System.out.println(file.getAbsolutePath() + " : " + isDicomOrAcrNema(data));
        }
    }

}
