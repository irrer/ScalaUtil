echo @echo off

java -version
set dirname=%~dp0
echo Starting XRayAngioFix ...

java -cp "%dirname%ScalaUtil_2.12-0.0.11-jar-with-dependencies.jar" -Dlog4j.configurationFile=%dirname%log4j2.xml edu.umro.ScalaUtil.XRayAngioFixer %*

echo Exit Code is %errorlevel%

start iexplore file:///%1/report/index.html

pause
