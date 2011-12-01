set PORT_FILE=%1
set CLASSPATH=<RUNTIME_CLASSPATH>
if "%ENSIME_JVM_ARGS%"=="" (set ENSIME_JVM_ARGS=-XX:+DoEscapeAnalysis -Xms256M -Xmx1512M -XX:PermSize=128m -Xss1M -Dfile.encoding=UTF-8)
REM Change SBT_JAR to point to the launcher jar used by your starting sbt.bat
REM set SBT_JAR="C:\Dev\bin\sbt-launch-0.11.jar"
REM change JLINE_TERM to force changes in the SBT loading
set JLINE_TERM=-Djline.terminal=jline.UnixTerminal
REM change the line below to modify extra options to SBTs launcher
REM set SBT_OPTS=-Dfile.encoding=utf8
java -classpath %CLASSPATH% %ENSIME_JVM_ARGS% org.ensime.server.Server %PORT_FILE%
