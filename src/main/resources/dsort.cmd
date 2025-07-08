@echo off

set dirname=%~dp0

java -cp %dirname%\ScalaUtil_2.12-0.0.19-jar-with-dependencies.jar edu.umro.ScalaUtil.DicomSort.DicomSort %*

pause

