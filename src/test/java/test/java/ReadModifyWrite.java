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
