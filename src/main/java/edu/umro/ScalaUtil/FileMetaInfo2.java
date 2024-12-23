package edu.umro.ScalaUtil;
// package com.pixelmed.dicom; // Where the original file resides.


import com.pixelmed.dicom.*;
import com.pixelmed.utils.HexDump;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * Special note: This is a replacement for the standard com.pixelmed.dicom.FileMetaInformation version that did not handle metadata that
 * contained certain metadata tags, such as:
 * <p>
 * (0002,0100) PrivateInformationCreatorUID
 * (0002,0102) PrivateInformation
 * <p>
 * <p>
 * A version of this code has been submitted for inclusion in the Pixelmed library.  If it is included, then this file would be deprecated.
 *
 * <p>A class to abstract the contents of a file meta information header as used for a
 * DICOM PS 3.10 file, with additional static methods to add to and extract from an
 * existing list of attributes.</p>
 *
 * @author dclunie
 * @author irrer Updated to calculate FileMetaInformationGroupLength for all possible group 0x0002 items.
 */
public class FileMetaInfo2 {

    private final AttributeList list;

    private static final DicomDictionary dictionary = new DicomDictionary();

    /**
     * <p>Get the attribute list in this instance of the file meat information.</p>
     *
     * @return the attribute list
     */
    public AttributeList getAttributeList() {
        return list;
    }

    /**
     * <p>Construct an instance of the  file meta information from the specified parameters.</p>
     *
     * @param mediaStorageSOPClassUID      the SOP Class UID of the dataset to which the file meta information will be prepended
     * @param mediaStorageSOPInstanceUID   the SOP Instance UID of the dataset to which the file meta information will be prepended
     * @param transferSyntaxUID            the transfer syntax UID that will be used to write the dataset
     * @param sourceApplicationEntityTitle the source AE title of the dataset (= null)
     * @throws DicomException if error in DICOM encoding
     */
    public FileMetaInfo2(String mediaStorageSOPClassUID, String mediaStorageSOPInstanceUID, String transferSyntaxUID, String sourceApplicationEntityTitle) throws DicomException {
        list = new AttributeList();
        addFileMetaInfo2(list, mediaStorageSOPClassUID, mediaStorageSOPInstanceUID, transferSyntaxUID, sourceApplicationEntityTitle);
    }

    /**
     * Calculate the number of bytes in the metadata.
     *
     * @param attrList Metadata from this attribute list.
     * @return The number of bytes in the metadata.
     * @throws DicomException If there is a problem writing to the DICOM stream.
     */
    private static long measureMetaDataLength(AttributeList attrList) throws DicomException {
        // Make an attribute list containing only metadata.
        AttributeList onlyMetaList = new AttributeList();
        for (AttributeTag t : attrList.keySet()) {
            if (t.getGroup() == 0x0002)
                onlyMetaList.put(attrList.get(t));
            else
                break;
        }

        // write the metadata to a byte array so its size can be determined.
        ByteArrayOutputStream bytesOut = new ByteArrayOutputStream();
        try {
            /*
             * Metadata is always written using ExplicitVRLittleEndian as per the DICOM specification.
             * @see <a href="URL#https://dicom.nema.org/medical/dicom/current/output/chtml/part10/chapter_7.html#para_30c3d0e1-5179-42cb-b61a-3b59e6cfa5dd">DICOM specification</a>.
             */
            DicomOutputStream dicomOut = new DicomOutputStream(bytesOut, TransferSyntax.ExplicitVRLittleEndian, null);
            onlyMetaList.write(dicomOut);
        } catch (IOException | DicomException ex) {
            throw new DicomException("Unable to convert metadata to an output stream");
        }

        // calculation of standardHeaderLength
        // bytes
        //  128   DICOM preamble
        //    4   literal 'DICM' after preamble
        //    2   FileMetaInformationGroupLength group 0x0002
        //    2   FileMetaInformationGroupLength element 0x0000
        //    2   FileMetaInformationGroupLength 'UL' ASCII VR (value representation)
        //    2   FileMetaInformationGroupLength length in bytes (for this, always 4)
        //    4   FileMetaInformationGroupLength 32 bit value representing length of metadata
        //  ---
        //  144  byte total
        long standardHeaderLength = 128 + 4 + 2 + 2 + 2 + 2 + 4;
        return bytesOut.size() - standardHeaderLength;
    }

    /**
     * Remove all metadata (group 0x0002 attributes) from the given list.
     *
     * @param attrList Remove from here.
     */
    private static void removeMetaData(AttributeList attrList) {

        AttributeList metaOnly = new AttributeList();

        // make a list of metadata
        for (AttributeTag t : attrList.keySet()) {
            if (t.getGroup() == 0x0002)
                metaOnly.put(attrList.get(t));
            else
                break;
        }

        // remove them from the passed list
        for (AttributeTag t : metaOnly.keySet()) {
            attrList.remove(t);
        }
    }

    /**
     * <p>Remove the file meta information attributes and add new ones to an existing list, using
     * only the parameters supplied.</p>
     *
     * <p>Note that the appropriate (mandatory) file meta information group length tag is also computed and added.</p>
     *
     * <p>If either PrivateInformationCreatorUID or PrivateInformation are provided, then both must be provided.</p>
     *
     * @param list                         the list to be extended with file meta information attributes
     * @param mediaStorageSOPClassUID      the SOP Class UID of the dataset to which the file meta information will be prepended
     * @param mediaStorageSOPInstanceUID   the SOP Instance UID of the dataset to which the file meta information will be prepended
     * @param transferSyntaxUID            the transfer syntax UID that will be used to write the dataset
     * @param sourceApplicationEntityTitle the source AE title of the dataset (= null)
     * @param PrivateInformationCreatorUID Application defined private UID.  Ignored if null.
     * @param PrivateInformation           Application defined private information.  Ignored if null.
     * @throws DicomException if error in DICOM encoding
     */
    public static void addFileMetaInfo2(AttributeList list,
                                        String mediaStorageSOPClassUID,
                                        String mediaStorageSOPInstanceUID,
                                        String transferSyntaxUID,
                                        String sourceApplicationEntityTitle,
                                        String PrivateInformationCreatorUID,
                                        byte[] PrivateInformation) throws DicomException {


        removeMetaData(list);

        {
            Attribute a = new OtherByteAttribute(TagFromName.FileMetaInformationVersion);
            byte[] b = {0, 1};
            a.setValues(b);
            list.put(a);
        }

        if (mediaStorageSOPClassUID == null || mediaStorageSOPClassUID.trim().length() == 0) {
            throw new DicomException("Cannot add FileMetaInformation without MediaStorageSOPClassUID value");
        } else {
            Attribute a = new UniqueIdentifierAttribute(TagFromName.MediaStorageSOPClassUID);
            a.addValue(mediaStorageSOPClassUID);
            list.put(a);
        }

        if (mediaStorageSOPInstanceUID == null || mediaStorageSOPInstanceUID.trim().length() == 0) {
            throw new DicomException("Cannot add FileMetaInformation without MediaStorageSOPInstanceUID value");
        } else {
            Attribute a = new UniqueIdentifierAttribute(TagFromName.MediaStorageSOPInstanceUID);
            a.addValue(mediaStorageSOPInstanceUID);
            list.put(a);
        }

        if (transferSyntaxUID == null || transferSyntaxUID.trim().length() == 0) {
            throw new DicomException("Cannot add FileMetaInformation without TransferSyntaxUID value");
        } else {
            Attribute a = new UniqueIdentifierAttribute(TagFromName.TransferSyntaxUID);
            a.addValue(transferSyntaxUID);
            list.put(a);
        }

        {
            Attribute a = new UniqueIdentifierAttribute(TagFromName.ImplementationClassUID);
            a.addValue(VersionAndConstants.implementationClassUID);
            list.put(a);
        }

        {
            Attribute a = new ShortStringAttribute(TagFromName.ImplementationVersionName);
            a.addValue(VersionAndConstants.implementationVersionName);
            list.put(a);
        }

        if ((sourceApplicationEntityTitle != null) && (sourceApplicationEntityTitle.trim().length() != 0)) {
            Attribute a = new ApplicationEntityAttribute(TagFromName.SourceApplicationEntityTitle);
            a.addValue(sourceApplicationEntityTitle);
            list.put(a);
        }

        if (
                ((PrivateInformation == null) && (PrivateInformationCreatorUID != null)) ||
                        ((PrivateInformation != null) && (PrivateInformationCreatorUID == null))) {
            throw new DicomException("If either PrivateInformationCreatorUID or PrivateInformation are specified, then the other must also be specified.");
        }

        if ((PrivateInformationCreatorUID != null) && (PrivateInformationCreatorUID.trim().length() == 0)) {
            throw new DicomException("If either PrivateInformationCreatorUID can not be empty.");
        }

        {
            UniqueIdentifierAttribute piUID = new UniqueIdentifierAttribute(dictionary.getTagFromName("PrivateInformationCreatorUID"));
            piUID.addValue(PrivateInformationCreatorUID);
            list.put(piUID);

            OtherByteAttribute pi = new OtherByteAttribute(dictionary.getTagFromName("PrivateInformation"));
            pi.setValues(PrivateInformation);
            list.put(pi);
        }

        // Ensure that there is a group length element.
        Attribute groupLength = new UnsignedLongAttribute(TagFromName.FileMetaInformationGroupLength);
        groupLength.addValue(0); // The value does not matter because it will be overwritten.
        list.put(groupLength);

        long measuredGl = measureMetaDataLength(list);

        // Set the group length to the proper value
        groupLength.removeValues();
        groupLength.addValue(measuredGl);
    }


    /**
     * <p>Remove the file meta information attributes and add new ones to an existing list, using
     * only the parameters supplied.</p>
     *
     * <p>Note that the appropriate (mandatory) file meta information group length tag is also computed and added.</p>
     *
     * @param list                         the list to be extended with file meta information attributes
     * @param mediaStorageSOPClassUID      the SOP Class UID of the dataset to which the file meta information will be prepended
     * @param mediaStorageSOPInstanceUID   the SOP Instance UID of the dataset to which the file meta information will be prepended
     * @param transferSyntaxUID            the transfer syntax UID that will be used to write the dataset
     * @param sourceApplicationEntityTitle the source AE title of the dataset (= null)
     * @throws DicomException if error in DICOM encoding
     */
    public static void addFileMetaInfo2(AttributeList list,
                                        String mediaStorageSOPClassUID,
                                        String mediaStorageSOPInstanceUID,
                                        String transferSyntaxUID,
                                        String sourceApplicationEntityTitle
    ) throws DicomException {
        addFileMetaInfo2(list,
                mediaStorageSOPClassUID,
                mediaStorageSOPInstanceUID,
                transferSyntaxUID,
                sourceApplicationEntityTitle,
                null,
                null);
    }

    /**
     * <p>Add the file meta information attributes to an existing list, extracting
     * the known UIDs from that list, and adding the additional parameters supplied.</p>
     *
     * @param list                         the list to be extended with file meta information attributes
     * @param transferSyntaxUID            the transfer syntax UID that will be used to write this list
     * @param sourceApplicationEntityTitle the source AE title of the dataset in the list (= null)
     * @throws DicomException if error in DICOM encoding
     */
    @SuppressWarnings("unused")
    public static void addFileMetaInfo2(AttributeList list, String transferSyntaxUID, String sourceApplicationEntityTitle) throws DicomException {
        String mediaStorageSOPClassUID = null;
        Attribute aSOPClassUID = list.get(TagFromName.SOPClassUID);
        if (aSOPClassUID != null) {
            mediaStorageSOPClassUID = aSOPClassUID.getSingleStringValueOrNull();
        }

        String mediaStorageSOPInstanceUID = null;
        Attribute aSOPInstanceUID = list.get(TagFromName.SOPInstanceUID);
        if (aSOPInstanceUID != null) {
            mediaStorageSOPInstanceUID = aSOPInstanceUID.getSingleStringValueOrNull();
        }
        if (mediaStorageSOPClassUID == null && mediaStorageSOPInstanceUID == null && list.get(TagFromName.DirectoryRecordSequence) != null) {
            // is a DICOMDIR, so use standard SOP Class and make up a UID
            mediaStorageSOPClassUID = SOPClass.MediaStorageDirectoryStorage;
            mediaStorageSOPInstanceUID = new UIDGenerator().getNewUID();
        }

        if (mediaStorageSOPClassUID == null) {
            throw new DicomException("Could not add File Meta Information - missing or empty SOPClassUID and not a DICOMDIR");
        }
        if (mediaStorageSOPInstanceUID == null) {
            throw new DicomException("Could not add File Meta Information - missing or empty SOPInstanceUID and not a DICOMDIR");
        }

        addFileMetaInfo2(list, mediaStorageSOPClassUID, mediaStorageSOPInstanceUID, transferSyntaxUID, sourceApplicationEntityTitle);
    }

    /**
     * <p>For testing.</p>
     *
     * <p>Generate a dummy file meta information header and test reading and writing it.</p>
     *
     * @param arg ignored
     */
    public static void main(String[] arg) {
        try {
            AttributeList list = new AttributeList();

            // adding the PrivateInformationCreatorUID and PrivateInformation attributes tests support for other group 0x0002 attributes.
            {
                UniqueIdentifierAttribute ui = new UniqueIdentifierAttribute(dictionary.getTagFromName("PrivateInformationCreatorUID"));
                ui.addValue("1.2.3.5.7.11"); // randomly chosen UID for testing only.
                list.put(ui);
            }

            {
                OtherByteAttribute ob = new OtherByteAttribute(dictionary.getTagFromName("PrivateInformation"));
                ob.setValues("ABCDE".getBytes());
                list.put(ob);
            }

            FileMetaInfo2.addFileMetaInfo2(list, "1.2.3.44", "1.2", TransferSyntax.Default, "MYAE", "1.3.5.7", "PrivInfo".getBytes());

            System.err.println("As constructed:");    // no need to use SLF4J since command line utility/test
            System.err.print(list);
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            list.write(new DicomOutputStream(bout, TransferSyntax.ImplicitVRLittleEndian, null));
            byte[] b = bout.toByteArray();
            System.err.print(HexDump.dump(b));
            AttributeList rlist = new AttributeList();
            rlist.read(new DicomInputStream(new ByteArrayInputStream(b), TransferSyntax.ExplicitVRLittleEndian, true));
            System.err.println("As read:");
            System.err.print(rlist);
        } catch (Exception e) {
            e.printStackTrace(System.err);    // no need to use SLF4J since command line utility/test
            System.exit(0);
        }
    }
}
