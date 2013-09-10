@echo off
echo Writing PATH, INCLUDE, and LIB variables to file "tmp.txt"
echo Open tmp.txt and paste the 3 lines in the *shellopts.bat file
echo SET PATH=%PATH%;%%MATLAB_BIN%%; > tmp.txt
echo SET INCLUDE=%INCLUDE% >> tmp.txt
echo SET LIB=%LIB%;%%MATLAB%%\extern\lib\win32;  >> tmp.txt
