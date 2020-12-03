package test.java;

import java.io.File;
import com.pixelmed.dicom.Attribute;
import com.pixelmed.dicom.AttributeFactory;
import com.pixelmed.dicom.AttributeList;
import com.pixelmed.dicom.TagFromName;
import com.pixelmed.dicom.TransferSyntax;

/**
 * Example of a program that reads a DICOM file, adds an attribute, and then writes it.
 * 
 * @author irrer
 *
 */

public class ReadModifyWrite {

    public static void main(String[] args) throws Exception {
        AttributeList al = new AttributeList();
        al.read("OldDICOM.dcm");
        
        // Substitute the attributes and values of your choice
        Attribute attr = AttributeFactory.newAttribute(TagFromName.PhysiciansOfRecord);
        attr.addValue("John Smith");
        //short[] shorts = al.get(TagFromName.PixelData).getShortValues();
        al.put(attr);
        String transferSyntax = al.get(TagFromName.TransferSyntaxUID).getSingleStringValueOrDefault(TransferSyntax.ExplicitVRLittleEndian);
        al.write(new File("NewDICOM.dcm"), transferSyntax, true, true);
    }

}
