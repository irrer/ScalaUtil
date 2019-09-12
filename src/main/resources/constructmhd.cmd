
@echo off

set dirname=%~dp0

java -Xmx512m -cp %dirname%ScalaUtil_2.12-0.0.10-jar-with-dependencies.jar edu.umro.ScalaUtil.ConstructMhd %*

