package bpe_util;
# ==========================================================================
#
# Name       : bpe_util
#
# Purpose    : Provide some subroutines for BPE scripts
#
# Subroutines: initPar_Fl, initPar_Cl, initPar_Bl, setUserVar, replaceSel,
#              prtGoto, prtMess, prtErr, getWarn, setMinusPlus, appFile,
#              selSta, timstr, timprt, sysprint, rnx2crz, crz2rnx, copyRef,
#              isHourly, prtBPEfile, deleteFiles
#
# Author     : M.Meindl
#
# Created    : 10-Aug-2011
#
# Changes    : 09-Jul-2003 mm: setUserVar added
#              15-Jul-2003 mm: replaceSel, prtGoto, prtMess, prtErr added
#              22-Jul-2003 mm: initPar_Fl added
#              30-Jul-2003 mm: get_warn.pl adapted to getWarn subroutine
#              03-Aug-2003 mm: setPulsMinus and appFile added
#              16-Sep-2003 rd: Move timstr and sysprint from gpsutil.pm
#              15-Oct-2003 mm: appFile, timstr, sysprint modified
#              28-Oct-2004 ss: selSta implemented
#              02-Nov-2004 ss/mm: -priority option in selSta
#              11-Nov-2004 ss: timstr simplified; timprt added
#              25-Nov-2004 mm: Hatanaka routines added (rnx2crz, crz2rnx)
#              16-Dec-2004 ss: selSta adapted for selection of satellites
#              31-Jan-2005 mm: initPar routines revised (both, BPE object or
#                              filename possible)
#              01-Feb-2005 ss: crz2rnx/rnx2crz: cont. if file not available
#              20-Apr-2005 eb/ss: crz2rnx/rnx2crz: -l/-u option
#              27-Mar-2005 mm: setPlusMinus replaced by setMinusPlus
#                              rdUltFlg, replaceSelLst added
#              14-Jul-2008 ss: Synchronization with swisstopo
#              09-Mar-2010 ss: getWarn: 2 message lines added (cf. get_warn.pl)
#              17-Aug-2010 rd: getWarn: clean buffer in case of repeated run
#              10-Aug-2011 rd: copyRef, isHourly, prtBPEfile, and deleteFiles
#                              functions added
#              24-Aug-2011 rd: Error added if an initPar_?? modul is used
#                              in a non-parallel script
#              20-Apr-2012 rd: Check for executables $c2rPgm/$r2cPgm
#              31-May-2012 rd: Use myWhich instead of File::Which
#              05-Sep-2012 SL: Export &myWhich
#              18-Sep-2012 SL: Use option -a for all gzip -dc calls
#              24-Sep-2012 SL: Use File::Path::mkpath in check_dir
#
# ==========================================================================

use Fcntl ':flock';
use File::Basename;
use lib "$ENV{X}/EXE";
use Gps_Date;
use File::Copy;
use strict;
use vars qw(@ISA @EXPORT);
use Exporter;

@ISA    = qw(Exporter);
@EXPORT = qw(initPar_Fl initPar_Cl initPar_Bl
             replaceSel replaceSelLst redFileList
             setUserVar setMinusPlus
             prtGoto prtMess prtErr getWarn rdUltFlg
             appFile selSta timstr timprt sysprint
             rnx2crz crz2rnx copyRef isHourly prtBPEfile
             deleteFiles copy2archive check_dir myWhich);

# -------------------------------------------------------------------------
# Some parameters
# -------------------------------------------------------------------------

# Extension for BPE-files
my $bpeExt = ".BPE";

# Extension for INP-files
my $inpExt = ".INP";

# Hatanaka compression tools
my $c2rPgm = ($main::win32) ? "CRX2RNX.exe" : "CRX2RNX";
my $r2cPgm = ($main::win32) ? "RNX2CRX.exe" : "RNX2CRX";


# -------------------------------------------------------------------------
# Subroutines
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# initPar_Fl
#
# Creates file for parallel scripts (tmp file), using already existing
# cluster files.
#
# Usage 1  : initPar_Fl($bpe,@cluFil)
# Usage 2  : initPar_Fl($tmpFil,@cluFil)
#
# Example 1: initPar_Fl($bpe,@cluFil)
# Example 2: initPar_Fl("$T/AUTO_TEMP/$PARAM1",@cluFil)
#
# Parameter:    bpe    : BPE object ($bpe{CONTROL_FILE} is used)
#            or tmpFil : name of tmp file (usually $T/AUTO_TEMP/$PARAM1)
#               cluFil : list with already prepared cluster files
#
# Remark   : -use this sr instead of initPar_Cl if you want to have
#             a special cluster setup
#
# -------------------------------------------------------------------------
sub initPar_Fl {

# get arguments
  my ($tmpFil,@cluFil) = @_;

# create tmp file
  if (ref($tmpFil)) {
    if (defined $$tmpFil{CONTROL_FILE}) {
      $tmpFil = $$tmpFil{CONTROL_FILE} ;
    } else {
      prtErr($tmpFil,"NO PARALLEL PROCESSING AVAILABLE");
      warn("initPar_Bl: The script is not designed for parallel processing.\n");
    }
  }

  open(TMP,">$tmpFil");

  print TMP map { $_."\n" } @cluFil;
  close TMP;

# print information to log file
  print  "\n\n","-"x75,"\n";
  print  "List of cluster files\n";
  print  "-"x75,"\n";
  print  map { $_."\n" } @cluFil;
  print  "-"x75,"\n\n";

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# initPar_Cl
#
# Creates files for parallel scripts (tmp file, bpe files), used for
# cluster solutions.
#
# Usage 1  : initPar_Cl($bpe,$bpeFil,@filLst,
#                       $numClu,$numFil,$minClu,
#                       @args)
# Usage 2  : initPar_Cl($tmpFil,$bpeFil,@filLst,
#                       $numClu,$numFil,$minClu,
#                       @args)
#
# Example 1: initPar_Cl($bpe,"$OUT/RAP_",@obsFil,5,0,0)
# Example 2: initPar_Cl("$T/AUTO_TEMP/$PARAM1","$OUT/RAP_",@obsFil,5,0,0)
#
# Parameter:    bpe    : BPE object ($bpe{CONTROL_FILE} is used)
#            or tmpFil : name of tmp file (usually $T/AUTO_TEMP/$PARAM1)
#               bpeFil : name of BPE files (a cluster number and an extension
#                        will be appended)
#               filLst : list with filenames to be written in cluster files
#               numClu : desired number of clusters
#               numFil : desired number of files per cluster
#               minClu : minimum number of clusters
#               args   : list with additional arguments for BPE files
#                        (optional)
#
# Remark   : -$tmpFil and $bpeFil must be given with full path
#            -$numFil and $minClu are ignored if $numClu>0
#            -0 for $numClu, $numFil, and $minClu: ignore this option
#            -@args is optional ($args[] are passed as PARAM2, PARAM3, ...)
#
# -------------------------------------------------------------------------
sub initPar_Cl($$\@$$$@) {

# get arguments
  my ($tmpFil, $bpeFil, $refLst, $numClu, $numFil, $minClu, @args) = @_;
  my @filLst = @$refLst;

# variables
  my (@cluFil,$i,$cluster,$file,$rem,$argStr);
  my $numBL = @filLst;

# return on error
  if ($numClu<=0 && $numFil<=0) { return 0 }
  if ($numBL==0)                { return 1 }

# check additional arguments
  if (@args>0) { $argStr = " ".join(" ",@args) }
  else         { $argStr = "" }

# compute number of clusters, number of files per cluster
  if ($numClu>0) { $numFil = 0 }

  if ($numFil>0) {
    if ($numBL%$numFil!=0) { $numClu = int($numBL/$numFil)+1 }
    else                   { $numClu = int($numBL/$numFil) }
    if   ($numClu<$minClu) { $numClu = $minClu }}

  if ($numClu>$numBL) { $numClu = $numBL }
  $rem    = $numBL%$numClu;
  $numFil = int($numBL/$numClu)+1;

# create bpe filenames
  for ($i=0;$i<$numClu;$i++) {
    push(@cluFil,sprintf("%s%03d%s",$bpeFil,$i+1,$bpeExt)) }

# write filenames in cluster files, create tmp file
  chomp(@filLst);

  if (ref($tmpFil)) {
    if (defined $$tmpFil{CONTROL_FILE}) {
      $tmpFil = $$tmpFil{CONTROL_FILE} ;
    } else {
      prtErr($tmpFil,"NO PARALLEL PROCESSING AVAILABLE");
      warn("initPar_Cl: The script is not designed for parallel processing.\n");
    }
  }

  open(TMP,">$tmpFil");

  foreach $cluster (@cluFil) {
    if ($rem==0) { $numFil-- }
    open(CLU,">$cluster");
    for ($i=0;$i<$numFil;$i++) {
      if (@filLst==0) { last }
      print CLU shift(@filLst)."\n" }
    close CLU;
    $rem--;
    print TMP $cluster."$argStr\n" }
  close TMP;

# print information to log file
  print  "\n\n","-"x75,"\n";
  print  "List of cluster files\n";
  print  "-"x75,"\n";
  print  map { $_."\n" } @cluFil;
  print  "-"x75,"\n\n";

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# initPar_Bl
#
# Creates a file for parallel scripts (tmp file), used for baseline by
# baseline solutions
#
# Usage 1  : initPar_Bl($bpe,@filLst,@args)
# Usage 2  : initPar_Bl($tmpFil,@filLst,@args)
#
# Example 1: initPar_Bl($bpe,@obsFil)
# Example 2: initPar_Bl("$T/AUTO_TEMP/$PARAM1",@obsFil)
#
# Parameter:    bpe    : BPE object ($bpe{CONTROL_FILE} is used)
#            or tmpFil: name of tmp file (usually $T/AUTO_TEMP/$PARAM1)
#               filLst: list with filenames to be written in tmp file
#               args  : list with additional arguments for BPE files
#                       (optional)
#
# Remark   : -$tmpFil must be given with full path
#            -@args is optional ($args[] are passed as PARAM2, PARAM3, ...)
#
# -------------------------------------------------------------------------
sub initPar_Bl($\@@) {

# get arguments
  my ($tmpFil,$refLst,@args) = @_;
  my @filLst = @$refLst;

# variables
  my ($file,$argStr);

# check additional arguments
  if (@args>0) { $argStr = " ".join(" ",@args) }
  else         { $argStr = "" }

# create tmp file
  chomp(@filLst);

  if (ref($tmpFil)) {
    if (defined $$tmpFil{CONTROL_FILE}) {
      $tmpFil = $$tmpFil{CONTROL_FILE} ;
    } else {
      prtErr($tmpFil,"NO PARALLEL PROCESSING AVAILABLE");
      warn("initPar_Bl: The script is not designed for parallel processing.\n");
    }
  }

  open(TMP,">$tmpFil");

  foreach $file (@filLst) {
    print TMP $file."$argStr\n" }
  close TMP;

# print information to log file
  print  "\n\n","-"x75,"\n";
  print  "List of stations/baselines\n";
  print  "-"x75,"\n";
  print  map { $_."\n" } @filLst;
  print  "-"x75,"\n\n";


# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# setUserVar
#
# Prepend a list of user variables to MENU_VAR.INP
#
# Usage    : setUserVar($s,%vars)
#
# Example  : setUserVar($s,"CD4",$PARAM1)
#
# Parameter: s   : bpe object
#            var : hash containing the variables as keys with corresponding
#                  values
#
# Remark   :
#
# -------------------------------------------------------------------------
sub setUserVar {

# get arguments
  my ($s,%vars) = @_;

# variables
  my ($var,$val);
  my $U      = $$s{U};
  my $T      = $$s{T};
  my $tmpFil = "$T/setUserVar.$$";

# create tmp file
  open(TMP,">$tmpFil");
  foreach $var (keys(%vars)) {
    $val = $vars{$var};
    print TMP "\"$var\" \"$val\"\n" }
  close TMP;
  $s->putKey("${U}/PAN/MENU_VAR.INP", "USERVAR", "", "PREPEND", $tmpFil);
  unlink($tmpFil);

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# replaceSel
#
# Replace SELECTED with a file list
#
# Usage    : replaceSel($s,$prog,$keyWd,$filNam)
#
# Example  : replaceSel($s,"ADDNEQ2","INPFILE",$PARAM1)
#
# Parameter: s     : bpe object
#            prog  : name of program
#            keyWd : keyword where SELECTED should be replaced
#            filNam: file with file names in it (usually $PARAM1)
#
# Remark   :
#
# -------------------------------------------------------------------------
sub replaceSel {

# get arguments
  my ($s,$prog,$keyWd,$filNam) = @_;

# variables
  my $U      = $$s{U};
  my $inpFil = "${U}/PAN/${prog}${inpExt}";

# replace selected
  $s->putKey($inpFil,$keyWd,"SELECTED","REPLACE",$filNam);

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# replaceSelLst
#
# Replace SELECTED with a file list
#
# Usage    : replaceSelLst($s,$prog,$keyWd,@filLst)
#
# Example  : replaceSelLst($s,"ADDNEQ2","INPFILE",@files)
#
# Parameter: s     : bpe object
#            prog  : name of program
#            keyWd : keyword where SELECTED should be replaced
#            filLst: list with file names
#
# Remark   :
#
# -------------------------------------------------------------------------
sub replaceSelLst {

# get arguments
  my ($s,$prog,$keyWd,@filLst) = @_;

# variables
  my $U      = $$s{U};
  my $inpFil = "${U}/PAN/${prog}${inpExt}";
  my $selFil = "${U}/WORK/${prog}.SEL";

# create file with names
  open OUT,">$selFil";
  map { s/\n$// } @filLst;
  map { print OUT "$_\n" } @filLst;
  close OUT;

# replace selected
  $s->putKey($inpFil,$keyWd,"SELECTED","REPLACE",$selFil);
  unlink $selFil;

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# redFileList
#
# Reduce list of selected files
# Remove non existent files from a list of input files.
#
# Usage    : redFileList($s,$prog,$keyWd)
#
# Example  : redFileList($bpe,"COMPAR","COOFIL");
#              if the coordinate files are selected with, e.g., FIN$YD+-
#
# Parameter: s     : bpe object
#            prog  : program name
#            keyWd : keyword containing the file list
#
# Remark   : This subroutine is useful to reduce a file list composed by
#            the menu based on plus/minus variables. Non existent files
#            are removed from the list. The reduced list is written back
#            to the panel.
#
# -------------------------------------------------------------------------
sub redFileList {

# get arguments
  my ($s,$prog,$keyWd) = @_;

# variables
  my $U      = $$s{U};
  my $panFil = "${U}/PAN/${prog}${inpExt}";
  my $inpFil = "${U}/INP/${prog}${inpExt}";

# run menu to expand plus/minus variables
  $s->inpOutMenu("",$panFil,$inpFil,$prog);

# get list of files
  my @filLst = map { -s $_ ? $_ : () }
               split "\n",$s->getKey($inpFil,$keyWd);

# set reduced file list
  replaceSelLst($s,$prog,$keyWd,@filLst);

# end of subroutine
  return 1 }



# -------------------------------------------------------------------------
# prtGoto
#
# Print a GOTO_PID in protocol file
#
# Usage    : prtGoto($s,$pid)
#
# Example  : prtGoto($s,$PARAM1)
#
# Parameter: s  : bpe object
#            pid: PID to goto
#
# Remark   :
#
# -------------------------------------------------------------------------
sub prtGoto {

# get arguments
  my ($s,$pid) = @_;

# print the goto statement
  $s->PRT_GOTO($pid);

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# prtMess
#
# Print a message in protocol file
#
# Usage    : prtMess($s,$msg)
#
# Example  : prtMess($s,"GLONASS EXCLUDED")
#
# Parameter: s  : bpe object
#            msg: message to print
#
# Remark   :
#
# -------------------------------------------------------------------------
sub prtMess {

# get arguments
  my ($s,$msg) = @_;

# variables
  my $prtFil = $$s{PRT_FILE};
  my $script = $$s{SCRIPT};

# print the message
  RUNBPE::PRT_MESS($prtFil,"MSG",$script,$msg);

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# prtErr
#
# Print an error message in protocol file
#
# Usage    : prtErr($s,$err)
#
# Example  : prtErr($s,"NO CLOCK FILE FOUND")
#
# Parameter: s  : bpe object
#            err: error to print
#
# Remark   : Should be used in combination with die() to stop the BPE
#            prtErr($s,"SOME ERROR") and die "BPE stopped by user script"
#
# -------------------------------------------------------------------------
sub prtErr {

# get arguments
  my ($s,$err) = @_;

# variables
  my $prtFil = $$s{PRT_FILE};
  my $script = $$s{SCRIPT};

# print the error
  RUNBPE::PRT_MESS($prtFil,"ERR",$script,$err);

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# getWarn
#
# Extract warnings/errors from a BPE protocol file
#
# Usage    : getWarn($s,$sumFil)
#
# Example  : getWarn($bpe,"RNXERR.SUM")
#
# Parameter: s     : bpe object
#            sumFil: file in which the warnings should be collected
#
# Remark   :
#
# -------------------------------------------------------------------------
sub getWarn {

# get arguments
  my ($s,$sumFil) = @_;

# variables
  my $prtFil = $$s{PRT_FILE};
  my $iPrt   = 0;

# open files
  my @errMsg = ();
  open (PRT,$prtFil);

# loop over input file
  while (<PRT>) {
    chomp;

# Script was repeated
    unless (index($_,"SCRIPT  STARTED")==-1) {
      @errMsg = ();
      next;
    }

# Warning/Error indicators found
    $iPrt = 1 unless index($_,"###")==-1;
    $iPrt = 1 unless index($_,"***")==-1;
    $iPrt = 1 unless index($_,"R2RD")==-1;

# ignore some messages
    $iPrt = 0 unless index($_,"TOO MANY COMMENT LINES")==-1;
    $iPrt = 0 unless index($_,"SATELLITES SKIPPED")==-1;
    $iPrt = 0 unless index($_,"MAXSLP EXCEEDED")==-1;
    $iPrt = 0 unless index($_,"OBSERVATION DATA FROM OTHER SATELLITE SYSTEM REJECTED")==-1;
    $iPrt = 0 unless index($_,"INDICATED P1 MEASUREMENTS NOT AVAILABLE")==-1;
    $iPrt = 0 unless index($_,"INDICATED C1 MEASUREMENTS NOT AVAILABLE")==-1;
    $iPrt = 0 unless index($_,"JUMP INTRODUCED INTO PHASES")==-1;
    $iPrt = 0 unless index($_,"number overflow, number set to zero")==-1;
##    $iPrt = 0 unless index($_,"FREQUENCY NOT DEFINED IN SATELLITE FILE")==-1;
    $iPrt = 0 unless index($_,"SKIP INVALID NAVIGATION MESSAGE")==-1;
    $iPrt = 0 unless index($_,"EPOCH FLAG DETECTED")==-1;
    $iPrt = 0 unless index($_,"SPORADIC P1 MEASUREMENTS IGNORED")==-1;
    $iPrt = 0 unless index($_,"LOW PERCENTAGE OF P1 MEASUREMENTS")==-1;

    if (length($_)<5 && $iPrt==1) {
      push @errMsg,"\n";
      $iPrt = 0 }

# print line to summary file
    push @errMsg,"$_\n" if $iPrt==1 }

  close PRT;

  open (SUM,">>$sumFil");
  flock(SUM,LOCK_EX);
  print SUM @errMsg;
  flock(SUM,LOCK_UN);
  close SUM;

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# setMinusPlus
#
# Redefine minus/plus variables in MENU_VAR.INP
#
# Usage    : setMinusPlus($s,$minus,$plus); or
#            setMinusPlus($s);
#
# Example  : setMinusPlus($bpe,-6,0);
#            setMinusPlus($bpe);
#
# Parameter: s    : bpe object
#            minus: new value of V_MINUS
#            plus : new value of V_PLUS
#
# Remark   : Call 1: V_MINUS = $minus
#                    V_PLUS  = $plus
#
#            Call 2: V_MINUS = PARAM1
#                    V_PLUS  = PARAM2
#                    Original V_MINUS/V_PLUS is used if PARAM1 and/or
#                    PARAM2 is not set for the script (in PCF)
#
# -------------------------------------------------------------------------
sub setMinusPlus {

# get arguments
  my ($s,$minus,$plus) = @_;
  my ($V_MINUS, $V_PLUS, $PARAM1, $PARAM2) =
    $s->getKeys('V_MINUS','V_PLUS','PARAM1','PARAM2');

# set new PLUS/MINUS variables
  $minus = defined($minus) ? $minus  :
           $PARAM1 ne ""   ? $PARAM1 : $V_MINUS;

  $plus = defined($plus) ? $plus   :
          $PARAM2 ne ""  ? $PARAM2 : $V_PLUS ;

  $s->putKey("$ENV{U}/PAN/MENU_VAR.INP","VAR_MINUS",$minus);
  $s->putKey("$ENV{U}/PAN/MENU_VAR.INP","VAR_PLUS",$plus);

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# appFile
#
# Append a file to another
#
# Usage    : appFile ($title,$blank,$source,$target,$stop)
#
# Example  : appFile ("",0,$SYSOUT,"${outDir}SUMMARY.$DAYYEAR")
#            appFile ("SOMETHING",2,$SYSOUT,"${outDir}SUMMARY.$DAYYEAR",1)
#
# Parameter: title : title line (blank: no title printed)
#            blank : number of blank lines to append
#            source: file which should be appended
#            target: file to which source should be appended
#            stop  : script stops if summary file is missing
#
# Remark   :
#
# -------------------------------------------------------------------------
sub appFile {

# get arguments
  my ($title,$blank,$source,$target,$stop) = @_;

# print title
  open (TRG,">>$target");
  if (length($title)>0) {
    print TRG "="x80;
    print TRG "\n$title\n";
    print TRG "="x80,"\n\n" }

# append file
  if (-s $source) {
    open (SRC,$source);
    foreach (<SRC>) { chomp; print TRG "$_\n" }
##    print TRG <SRC>;
    close SRC }

# no file found -> die
  elsif ($stop==1) {
    print TRG "Mandatory summary file $source is missing.\n";
    die("Summary file $source is missing.\n") }

# no file found -> warning
  elsif ($source ne "") {
    print TRG "Summary file $source not found.\n" }

# print blank lines
  if ($blank>0) { print TRG "\n"x$blank }
  close TRG;

# end of subroutine
  return 1 }


# -------------------------------------------------------------------------
# rdUltFlg
#
# Read ultra flag
#
# Usage    : ($h_end,$h_tro,$h_rnx,$h_chr) = rdUltFlg($flgFil)
#
# Example  :
#
#
# Parameter: flgFil : ultra flag file (w/ full path)
#
# Remark   : If ultra flag does not exist:
#             -$h_end = $h_tro = $h_rnx = 24
#             -$h_chr = 0
#
# -------------------------------------------------------------------------
sub rdUltFlg {

# get arguments
    my $flgFil = shift;

# return values
    return -s $flgFil ? split " ",`cat $flgFil` : (24,24,24,0);

}


# -------------------------------------------------------------------------
# selSta
#
# Select stations for BPE processing (e.g. to copy specific RINEX files)
#
# Usage    : selSta ($selFil,@keyStr)
#
# Example  : selSta ("$ENV{X}/REF/PNAC.SEL")
#          : selSta ("$ENV{X}/REF/PNAC.SEL","%BPE_EUR")
#            selSta ("$ENV{X}/REF/PNAC.SEL","-or %FTP_INT %FTP_EXT")
#            selSta ("$ENV{X}/REF/PNAC.SEL","-priority %BPE_RAP")
#
# Parameter: selFil: File containing list of 4-character station names,
#                    followed by keywords for selection, if desired
#            keyStr: Optional list of keywords
#
# Remark   : List of keywords may include:
#              "-and" (default)
#              "-or"
#              "-not" (also in conjunction with "-and" or "-or")
#              "-priority" (to consider corresponding priority codes)
#              "-2" (to extract station names from CRD/VEL files)
#
# -------------------------------------------------------------------------
sub selSta {

# Get arguments
  my @opt=split(" ",join(" ",@_));
  my $selFil=shift(@opt);
  die "\n$selFil not found\n" unless -e $selFil;

  my $not=0;
  my $match="-and";
  my $priority=0;
  my $pos=0;
  my @selKey=();
  foreach (@opt) {
    if    (/-not/)          { $not=1 }
    elsif (/-and/ || /-or/) { $match=$_ }
    elsif (/-priority/)     { $priority=1 }
    elsif (/-2/)            { $pos=1 }
    else                    { push(@selKey,$_) } }

# Define minimum number of necessary matches
  if ($match eq "-and") { $match=@selKey }
  else                  { $match=1 }

# Build up list of station names
  my @selSta=();
  my (@line,$count,$key);
  open(FIL,$selFil);
  while (<FIL>) {
    chomp;
    @line=split(" ",$_);
    next if @line==0;
    next if length($line[$pos])!=4 && length($line[$pos])!=3;
    $count=0;
    foreach $key (@selKey) {
      if ($not) { $count++ unless /$key/ }
      else      { $count++ if     /$key/ } }
    next unless $count>=$match;
    if ($priority && /$selKey[0]\((\d)\)/) { $line[$pos]=$1.$line[$pos] }
    else                                   { $line[$pos]="_".$line[$pos] }
    push(@selSta,$line[$pos]) }

  close FIL;

# Sort and return list of selected stations
##  @selSta=sort(@selSta);
  @selSta=sort(@selSta) if $priority;
  @selSta=map { substr($_,1,4) } @selSta;

  return @selSta }



# -------------------------------------------------------------------------
# timstr
#
# Convert localtime-/gmtime-output to a formated datum string
# (e.g., "14-JUL-2004 13:50:15")
#
# Usage    : timstr(@time)
#
# Example  : timstr(localtime(time))
#
# Parameter: time: output from localtime or gmtime
#
# -------------------------------------------------------------------------
sub timstr {

# Get arguments
  my @now=@_;

# Adjust year and month
  $now[5]+=1900;
  $now[4]+=1;

# Call gps_date
  return my $str=gps_date("-ymd $now[5]-$now[4]-$now[3] -hms $now[2]:$now[1]:$now[0] -o %z") }



# -------------------------------------------------------------------------
# timprt
#
# Return time stamp for processing protocols
# (e.g., "14-JUL-2004 13:50:15")
#
# Usage    : timprt
#
# Example  : timprt
#
# Parameter: None or "-gmt" or "-l[ocal]" (default)
#
# Remark   : "%z" output format is defined in Gps_Date.pm.
#
# -------------------------------------------------------------------------
sub timprt {

# Get argument
  my $opt=defined($_[0]) ? $_[0] : "-local";

# Call gps_date
  return my $str=gps_date("-today $opt -o %z") }



# -------------------------------------------------------------------------
# sysprint
#
# Force the print command to STDOUT
#
# Usage    : sysprint($text)
#
# Example  : sysprint("So long, and thanks for all the fish")
#
# Parameter: text: text string to print
#
# Remark   : -Returns number of printed characters
#
# -------------------------------------------------------------------------
sub sysprint {

# get argument
  my $text = $_[0];

# print to STDOUT
  return syswrite(STDOUT,$text,length($text));
}



# -------------------------------------------------------------------------
# rnx2crz
#
# Compress RINEX files (M/m, N/n, G/g, O/o, D/d)
#
# Usage    : @done = rnx2crz(@source,[$dest],[-l|-u])
#
# Example  : @done = rnx2crz(glob("*.04O"), "$ENV{orxdp}")
#
# Parameter: @source : List with uncompressed RINEX files
#            $dest   : Path to destination directory
#            @done   : List of Compressed files
#
# Remark   : -Files may be already zipped
#            -If $dest not given, the original dir of the data is used
#
# -------------------------------------------------------------------------
sub rnx2crz {

# Get arguments
  my @rnx  = @_;
  my $opt  = $rnx[-1] =~ /([-][ul])/i ? pop @rnx : 0;
  my $dest = -d $rnx[-1] ? pop @rnx : 0;
  my ($fil, $nam, $dir, $ext, $yy, $typ, $zip, $new, $err, @done);

  unless (-e myWhich($r2cPgm)) {
    die ("*** crz2rnx: Executable $r2cPgm is missing\n".
         "    Install the program, or include it in the path vailable,\n".
         '    or redefine the variable $r2cPgm in ${BPE}/bpe_util.pm');
  };

# Loop over all files
  foreach $fil (@rnx) {
    $err = 0;

# Get dir, name, year, type, compression
    ($nam, $dir, $ext) = fileparse($fil,qr/\..*/);
    ($yy,  $typ, $zip) = $ext =~ /\.(\d\d)([DGMNO])(?:\.(Z))?$/i;

# No valid file
    next unless ($dir && $nam && $yy && $typ);

# New directory
    $dest = $dir unless $dest;

# Check whether file available
##    next unless -s $fil;

# Decide what to do
    if    ($typ =~ /[GMND]/i) {
      $new = "$dest/$nam.$yy$typ.Z";
      if (lc($opt) eq "-l") {$new = "$dest/".lc($nam).".$yy".lc($typ).".Z"}
      if (lc($opt) eq "-u") {$new = "$dest/".uc($nam).".$yy".uc($typ).".Z"}
      if (!$zip) { system("cat $fil | compress > $new") }
      else       { copy($fil,$dest) }
    }
    elsif ($typ eq "o") {
      $new = "$dest/$nam.${yy}d.Z";
      if (lc($opt) eq "-l") {$new = "$dest/".lc($nam).".${yy}d.Z"}
      if (lc($opt) eq "-u") {$new = "$dest/".uc($nam).".${yy}D.Z"}
      if (!$zip) { $err = system("$r2cPgm $fil - | compress > $new")        }
      else       { $err = system("gzip -dac $fil | $r2cPgm - | compress > $new") } 
    }
    elsif ($typ eq "O") {
      $new = "$dest/$nam.${yy}D.Z";
      if (lc($opt) eq "-l") {$new = "$dest/".lc($nam).".${yy}d.Z"}
      if (lc($opt) eq "-u") {$new = "$dest/".uc($nam).".${yy}D.Z"}
      if (!$zip) { $err = system("$r2cPgm $fil - | compress > $new")        }
      else       { $err = system("gzip -dac $fil | $r2cPgm - | compress > $new") } 
    }

# Error?   ## not active (how to get exit code from `...`??
    if ($err) {
      print "*** Error compressing file: $fil\n\n";
      unlink ($new);
    }
    else { push @done,$new }
  }

# Finished
  return @done;
}


# -------------------------------------------------------------------------
# crz2rnx
#
# Uncompress RINEX files (M/m, N/n, G/g, O/o, D/d)
#
# Usage    : @done = crz2rnx(@source,[$dest][-l|-u])
#
# Example  : @done = crz2rnx(glob("*.04D.Z"), $DIR_RAW)
#
# Parameter: @source : List with compressed RINEX files
#            $dest   : Path to destination directory
#            @done   : List of uncompressed files
#
# Remark   : -Files may be already unzipped
#            -If $dest not given, the original dir of the data is used
#
# -------------------------------------------------------------------------
sub crz2rnx {

# Get arguments
  my @crz   = @_;
  my $opt  = $crz[-1] =~ /([-][ul])/i ? pop @crz : 0;
  my $dest = -d $crz[-1] ? pop @crz : 0;
  my ($fil, $nam, $dir, $ext, $yy, $typ, $zip, $new, $err, @done);

  unless (-e myWhich($c2rPgm)) {
    die ("*** crz2rnx: Executable $c2rPgm is missing\n".
         "    Install the program, or include it in the path vailable,\n".
         '    or redefine the variable $crz2Pgm in ${BPE}/bpe_util.pm');
  };

# Loop over all files
  foreach $fil (@crz) {
    $err = 0;

# Get dir, name, year, type, compression
    ($nam, $dir, $ext) = fileparse($fil,qr/\..*/);
    ($yy,  $typ, $zip) = $ext =~ /\.(\d\d)([DGMNO])(?:\.(Z))?$/i;

# No valid file
    next unless ($dir && $nam && $yy && $typ);

# New directory
    $dest = $dir unless $dest;

# Check whether file available
    next unless -s $fil;

# Decide what to do
    if    ($typ =~ /[GMNO]/i) {
      $new = "$dest/$nam.$yy$typ";
      if (lc($opt) eq "-l") {$new = "$dest/".lc($nam).".$yy".lc($typ)}
      if (lc($opt) eq "-u") {$new = "$dest/".uc($nam).".$yy".uc($typ)}
      if ($zip) { system("gzip -dac $fil > $new") }
      else      { copy($fil,$new) }
    }
    elsif ($typ eq "d") {
      $new = "$dest/$nam.${yy}o";
      if (lc($opt) eq "-l") {$new = "$dest/".lc($nam).".${yy}o"}
      if (lc($opt) eq "-u") {$new = "$dest/".uc($nam).".${yy}O"}
      if ($zip) { $err = system("gzip -dac $fil | $c2rPgm - > $new") }
      else      { $err = system("cat $fil  | $c2rPgm - > $new") }
    }
    elsif ($typ eq "D") {
      $new = "$dest/$nam.${yy}O";
      if (lc($opt) eq "-l") {$new = "$dest/".lc($nam).".${yy}o"}
      if (lc($opt) eq "-u") {$new = "$dest/".uc($nam).".${yy}O"}
      if ($zip) { $err = system("gzip -dac $fil | $c2rPgm - > $new") }
      else      { $err = system("cat $fil  | $c2rPgm - > $new") }
    }

# Error?
    if ($err) {
      print "*** Error decompressing file: $fil\n\n";
      unlink ($new);
    }
    else { push @done,$new }
  }

# Finished
  return @done;
}

# ====================================================================
#
# Check if a directory exists and create it
#
# Usage: check_dir($dir) or check_dir(@dirs)
#
# Return: Names of created directories
#
# ====================================================================
sub check_dir {
  use File::Path qw(mkpath);
  my @dirNames = ();
  foreach (@_) {
    next if -d;
##    push @dirNames,mkpath($_,('verbose' => 1, 'mode' => 0777));
##    system("mkdir -p $_");
    &mkpath($_);
    push @dirNames,$_;
  }
  return @dirNames;
}

# -------------------------------------------------------------------------
# isHourly
# 
# Checks whether a session identifier indicates "hourly processing"
# (last character of the input string is one of A..X)
#
# Usage    : isHourly($session)
#
# Example  : if (isHourly($ssss)) { ... }
#
# Parameter: session : session identifier
#            isHourly: true if session ends with one of the chacters 
#                      between A..X
#
# -------------------------------------------------------------------------
sub isHourly {
    my $sess = shift;
    
    my $c1 = ord('A');
    my $c2 = ord(substr($sess,-1));
    my $c3 = ord('X');

    return ($c1 <= $c2 && $c2 <= $c3);
}

# -------------------------------------------------------------------------
# copyRef
# 
# Copy project specific files from the reference directory 
# into the campaign area
#
# Usage    : copyRef($source,@list_of_files)
#
# Example  : copyRef("$ENV{D}/DATAPOOL/REF52",@filLst);
#
# Parameter: source        : directory where the source files are located
#            @list_of_files: it contains a list of files to be copied and
#                            a "1"/"0" flag after a blank to specify whether
#                            it is an mandatory or optional file
#
# Remark   : - Files are only copied if they are newer in the source directory
#            - Files are first copied to an alternative name and afterwards
#              moved to their final names to minimize the risk of incomplete 
#              files
#
# -------------------------------------------------------------------------
sub copyRef {

    my $src    = shift(@_) . "/";$src =~ s/\/\/$/\//;
    my @filLst = @_;

    # Copy the files
    my $iErr = 0;
    foreach my $file ( @filLst ) {
      my ( $outFil,$mandatory ) = split(" ",$file);   # filename and mandatory flag
      my $inpFil = $src . basename($outFil);
      my $tmpFil = "${outFil}_$$";

      # Output name in big letters
      my $fname1 = basename($outFil);
      my $fname2 = uc basename($outFil);
      $outFil =~ s/\/$fname1$/\/$fname2/;

      # BSW-internal -->> external extensions
      $inpFil =~ s/\.IEP$/.ERP/;
      $inpFil =~ s/\.PRE$/.SP3/;
      my @hlp = glob "${inpFil}*";
      $inpFil =~ s/\.SP3$/.EPH/ if $#hlp == -1;
      @hlp = glob "${inpFil}*";
      $inpFil =~ s/\.EPH$/.PRE/ if $#hlp == -1;

      # Source files are compressed?
      my $compress = 0;
      unless (-s $inpFil) {
        ${inpFil} .= ".Z"  if ( -s "${inpFil}.Z" );
        ${inpFil} .= ".gz" if ( -s "${inpFil}.gz" );
        $compress = ${inpFil} =~ /\.(Z|gz)$/;
      }

      # Source file does exist
      if (-s $inpFil) {
        # Source newer than target
        if ( ! -s $outFil || (stat($inpFil))[9] > (stat($outFil))[9] )  {
          check_dir(dirname($outFil));
          if ( $compress ) {
            system("gzip -dac $inpFil > $tmpFil");
          } else {
            copy ($inpFil,$tmpFil);
          }
          move ($tmpFil,$outFil);
          sysprint "File $inpFil -->> $outFil copied\n"
        } else {
          sysprint "File $outFil is up-to-date\n"
        }

      # Complain if a mondatory file is missing
      } elsif ( $mandatory ) {
        sysprint "File $inpFil cannot be provided (mandatory)\n";
        $iErr++;

      # Note if an optional file is missing
      } else {
        sysprint "File $inpFil is not available (optional)\n";
      }
    }

    # Stop if a mandatory file was missing
    if ($iErr) {
      die("*** copyRef: $iErr mandatory file is missing\n") if ($iErr == 1);
      die("*** copyRef: $iErr mandatory files are missing\n");
    }
}


# -------------------------------------------------------------------------
# prtBPEfile
# 
# Prints the content of a BPE file to the standard output
#
# Usage    : prtBPEfile($BPEfile,$delete)
#
# Example  : prtBPEfile($$bpe{PARAM1},1);
#
# Parameter: $BPEfile : filename to be printed
#            $delete  : switch whether delete the files (!=0) or not (==0)
#
# -------------------------------------------------------------------------
sub prtBPEfile {
    my $file = shift;
    my $del  = shift || 0;
    
    open (BPE,$file);
    print "\n\n","-"x75,"\n";
    print "Contents of BPE file $file\n";
    print "-"x75,"\n";
    foreach (<BPE>) {
      chomp; 
      print "$_\n";
      unlink ("$_") if $del;
    }
    print "-"x75,"\n";
    close BPE;
    unlink $file;
}


# -------------------------------------------------------------------------
# deleteFiles
# 
# Delete files listed in the input file
#
# Usage    : deleteFiles(bpe,delFil,delete)
#
# Example  : deleteFiles($bpe,$delFil,1);
#
# Parameter: bpe     : BPE object
#            delFil  : name of the file containg the list of files 
#                      to be deleted
#            delete  : switch whether delete the deletion file or not
#
# -------------------------------------------------------------------------
sub deleteFiles {
    my $bpe    = shift;
    my $delFil = shift;
    my $del    = shift || 1;
    
    if (-s $delFil) {
      open (DEL,$delFil);
      while (<DEL>) {
        chomp;
        while ( /^ / ) { s/^ // }  # Remove leading blanks
        unlink ($_);
        prtMess($bpe,"$_ deleted");
      }
      close DEL 
    }
    unlink($delFil) if (-e $delFil && $del);
}


# ====================================================================
#
# copyarchive
# -----------
#
# Copy a list of files into the archive area
#
# Usage   : copyarchive(source,target,flags,title);
#
# Options : source: File to be copied into the archive (wildcards allowed)
#           target: Target where to put the file (may be a directory or
#                   a file)
#           flags:  c: UNIX compress the file at the target
#                   z: gnu-zip the file at the target
#                   f: appand a title line
#
# Examples: copyarchive("${dirStd}G3_${yyddd}.${extStd}",$orbSav);
#           copyarchive("${dirPre}G3_${yyddd}.${extPre}",$orbSav,"zf","repro 01");
#
#
sub copy2archive {

  my ($src,$trg,$c,$tit) = @_;
  $c   = " " unless defined($c);

  if ( $c =~ /f/ && !defined($tit) ) {
    print "\n *** sr copyarchive: A title string is needed to append a flag: $src\n";
    return 1;
  }

  my $iFil = 0;
  my $iErr = 0;
  foreach my $filnam ( glob "$src") {
    $iFil++;

    if ( ! -s $filnam ) {
      $iErr++;
      print "\n *** sr copy2archive: Source file is missing: $filnam\n";
      next;
    }

    # Copy the file into the archive
    copy($filnam,$trg);

    # Make sure to get the target filename
    my $fil = $trg;
    $fil .= "/" . &basename($filnam) if (-d $trg);

    if (-s $filnam != -s $fil) {
      $iErr++;
      print "\n *** sr copy2archive: Error copy file $src\n";
    }

    # append a flag line to the result file
    if ($c =~ /f/ && $tit ne "") {
      appFile("",5,"",$fil,0);
      appFile("$tit",2,"",$fil,0);
    }

    # compress if requested
    system("compress -f $fil") if ($c =~ /c/);       # UNIX compress
    system("gzip     -f $fil") if ($c =~ /z/);       # gnu-zip
  }

  if ( $iFil == 0 ) {
    print "\n *** sr copy2archive: File not found $src\n";
    return 1;
  }

  return $iErr;
}


# ====================================================================
#
# myWhich
# -------
#
# Check whether a certain program name can be called via the path variable
#
# Usage   : myWhich(programName);
#
# Options : programName: Name of the program
#
# Examples: myWhich("CRX2RNX");
#
#
sub myWhich {
  my $pgmNam = shift;

  my $found = "";

  if(-e $pgmNam) {
    $found = $pgmNam;
  }
  elsif($pgmNam !~ /\// && $pgmNam !~ /\\/) {
    my @pathList = $ENV{PATH} =~ /;/ ?
                   split(";",$ENV{PATH}) :
                   split(":",$ENV{PATH});
    foreach my $path(@pathList) {
      my $file = $path . "/" . $pgmNam;
      if(-e $file) {
        $found = $file;
        last;
      }
    }
  }

  return $found;
}

# -------------------------------------------------------------------------
# End of module bpe_util.pm
# -------------------------------------------------------------------------
1;
