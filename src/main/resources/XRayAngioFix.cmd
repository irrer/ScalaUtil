echo @echo off

java -version
set dirname=%~dp0
echo Starting XRayAngioFix ...

@rem If one of the table angles has an absolute value larger than this, then report an error and do not create files.
set MaxTableAngle=1.0

java -cp "%dirname%ScalaUtil_2.12-0.0.11-jar-with-dependencies.jar" -Dlog4j.configurationFile=%dirname%log4j2.xml edu.umro.ScalaUtil.XRayAngioFixer %*

echo Exit Code is %errorlevel%

start iexplore file:///%1/report/index.html

pause
