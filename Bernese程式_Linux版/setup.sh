#!/bin/sh
# ==============================================================================
#
# Name:       setup.sh
#
# Purpose:    Setup utility for the Bernese GNSS Software (UNIX version).
#
# Authors:    L.Mervart
#
# Created:    13-Aug-2003
#
# Changes:    16-Oct-2003 RD: Use "make_dir" instead of "mkdir" in all cases
#                             Prompt after typing the installation directory
#                             Use "--path" for configure.pm
#             11-Jan-2012 SL: Adapted for V5.2, do not create empty dirs
#             12-Jan-2012 SL: Error handling
#             01-Feb-2012 SL: EXAMPLE replaces old example campaign files
#             11-Jun-2012 SL: Use gzip -d instead of uncompress
#             23-Aug-2012 SL: File extension taz changed to tgz
#             13-Sep-2012 SL: DATAPOOL and SAVEDISK added, EXAMPLE
#             26-Sep-2012 SL: Handle OS specific files
#             27-Nov-2012 EO: Adapt for Mac OS X
#
# Copyright:  Astronomical Institute
#             University of Bern
#             Switzerland
#
# ==============================================================================

# Default values
# --------------
VERSION="52";
instDirDefault="${HOME}";
thisPath=`dirname $0`;
thisPath=`(cd $thisPath; /bin/pwd)`;

# Useful routines
# ---------------
check_program () {
  PROGRAM=`which $1`;
  if [ ! -x $PROGRAM ]; then
    echo;
    echo " *** Cannot execute program $1";
    echo;
    exit;
  fi
}

make_dir () {
  if [ ! -d $1 ]; then
    mkdir -p $1;
  fi
  if [ ! -d $1 ]; then
    echo;
    echo " *** Cannot create directory $1";
    echo;
    exit;
  fi
}

# Check the required programs
# ---------------------------
check_program tar;
check_program gzip;
check_program uname;

# Start script
# ------------
echo;
echo "***************************************";
echo "*        Bernese GNSS Software        *";
echo "*             Installation            *";
echo "*  (UNIX/Linux or Mac OS X platform)  *";
echo "***************************************";
echo;

# Ask about the Software directory
# --------------------------------
echo;
echo "Full path where the BERN52 software tree will be installed";
echo "[ ${instDirDefault} ]:";

read instDir;

if [ "$instDir" = "" ]; then
  instDir=$instDirDefault;
fi

echo;
echo "The software will be installed in $instDir/BERN$VERSION";

if [ -d "$instDir/BERN$VERSION" ]; then
  echo;
  echo " ### Directory $instDir/BERN$VERSION exists. Move it away!";
fi

echo;
echo "Press the return key to continue or Ctrl-C to abort";
read anyString;

# Create the software directory and copy archives
# -----------------------------------------------
instDir="$instDir/BERN$VERSION";
make_dir $instDir;

file="BERN$VERSION";
if [ ! -f "$thisPath/$file.tgz" ]; then
  echo;
  echo " *** $file.tgz is not available at";
  echo "     $thisPath";
  echo;
  exit;
fi
CMD="cp $thisPath/$file.tgz $instDir/$file.tar.gz";
echo $CMD; $CMD;

for file in CAMPAIGN$VERSION DATAPOOL SAVEDISK ICONS; do
  if [ ! -f "$thisPath/$file.tgz" ]; then
    echo " ### $file.tgz is not available at";
    echo "     $thisPath";
  else
    CMD="cp $thisPath/$file.tgz $instDir/$file.tgz";
    echo $CMD; $CMD;
  fi
done
echo;

# Extract BERN archive
# --------------------
cd $instDir;
CMD="gzip -d BERN$VERSION.tar.gz";
echo $CMD; $CMD;
CMD="tar -xvf BERN$VERSION.tar";
echo $CMD; $CMD;
rm BERN$VERSION.tar;
echo;

for file in *.tgz; do
  CMD="mv $file GPS/DOC";
  echo $CMD; $CMD;
done
echo;

# Handle OS specific files
# ------------------------
for file in INSTALL.TXT; do
  cp GPS/DOC/README_${file}_UNIX GPS/DOC/README_$file;
  rm GPS/DOC/README_${file}_*;
done

platform='UNIX';
unamestr=`uname -s`;
if [ "$unamestr" = "Darwin" ]; then
   platform='MAC';
fi

cp GPS/EXE/Makefile.template_$platform GPS/EXE/Makefile.template;
rm GPS/EXE/Makefile.template_*;
rm GPS/PAN/WIN32.CPU;

# Start the Bernese configuration utility
# ---------------------------------------
if [ -f "$instDir/GPS/EXE/configure.pm" ]; then
  echo "**********************************************************************";
  echo "* The Bernese GNSS Software has been successfully installed";
  echo "* in $instDir";
  echo "**********************************************************************";
else
  echo " *** There is a problem with the installation!";
  exit;
fi

defaultPERL=`which perl`;
echo;
echo "Perl program to be used [ $defaultPERL ]:";

read PERL;

if [ "$PERL" = "" ]; then
  PERL=$defaultPERL;
fi
check_program $PERL;
echo;

$PERL $instDir/GPS/EXE/configure.pm --init --perl=$PERL --path=$instDir;

# ==============================================================================
