#!/usr/bin/perl -w
# ==============================================================================
#
# Name:       cbern.pl
#
# Author:     M.Meindl
#
# Created:    05-Sep-2005
#
# Changes :   08-Jul-2009 SL: option -C added (F_DEBUG)
#             21-Apr-2010 LM/SL: F_VERS_LIST added for multi-compiler usage
#             19-May-2010 SL: use strict, send message
#             18-Aug-2010 SL/RD: check for tab characters
#             23-Aug-2010 PS/SL: Send message only for COD AC
#             09-Dec-2010 SL: compile with DEBUG option
#             13-May-2011 SL: support of single compiler/option run, lock-file
#             19-Aug-2011 SL: support also F_DEBUG="NO"
#             11-Jan-2012 SL: use cmp_util instead of gpsutil
#             01-Feb-2012 SL: use -B option for COMPLINK
#             04-Apr-2012 SL: check for trailing blanks in check4tab call
#             18-May-2012 SL: Remove all executables if COMPLINK
#             16-Jun-2012 RD: Remove lock-file if the scripts ends with an error
#             27-Aug-2012 SL: Use &os from cmp_util
#             14-Sep-2012 SL: Use &compilers from cmp_util
#             27-Nov-2012 EO: Remove .map, .pdb, and .dwf files
#             14-Jan-2013 SL: Variable $mod to load module added
#
# Programs:   make
#
# ==============================================================================

use strict;
use File::Basename qw(basename);
use lib "$ENV{X}/EXE";
use cmp_util qw(os compilers check4tab filter_make);

# Get options
# -----------
# Standard options
my $target  = "";
my $optMake = "-i";

# Explicit name
my $name = $ARGV[-1] && $ARGV[-1] !~ /^-/ ? uc($ARGV[-1]) : "";

# Special make options
my $optStr = join " ",@ARGV;
my $optN  .= $optStr =~ s/-n//      ? "-n"    : "" ;
my $optT  .= $optStr =~ s/-t//      ? "-t -s" : "" ;

# Show help
# ---------
sub help {
  my(@timstr) = localtime(time);
  $timstr[5] += 1900;
  $timstr[7] += 1;
  my($timstr) = sprintf("%04i%03i",$timstr[5],$timstr[7]);
  die "\nNAME:     ".&basename($0)."\n".
      "\nPURPOSE:  Compile the programs of the Bernese GNSS Software.\n".
      "\nSYNOPSIS: ".&basename($0)." [OPTIONS] NAME\n".
      "\nOPTIONS:  -h    Print this help message\n".
        "          -n    No execution mode, print only commands\n".
        "          -t    Special 'touch' option\n".
        "          NAME  Name of the subroutine or program to compile\n".
        "                Special names are: LIB, ALL, COMPLINK, VERSION\n".
      "\nREMINDER: Is the 'Makefile' up to date? Otherwise do\n".
        "          makemake.pl -r \$C\n".
      "\nBACKUP:   tar -czf \$XG/../EXE_$timstr.tgz \$XG\n".
      "\nEXAMPLES: ".&basename($0)." LIB >\${T}/CBERN_LIB.log 2>&1 &\n".
        "          ".&basename($0)." ALL >\${T}/CBERN_ALL.log 2>&1 &\n".
        "          ".&basename($0)." COMPLINK >\${X}/EXE/COMPLINK.log 2>&1 &\n".
      "\nREMARKS:  - To compile using only the g95 Fortran compiler do:\n".
        "            export F_VERS=\"G95\"\n".
        "            export F_VERS_LIST=\"\"\n".
        "          - To compile either in DEBUG or speed-optimized mode do:\n".
        "            export F_DEBUG=\"YES\"  or \n".
        "            export F_DEBUG=\"NO\"\n".
      "\n";
}

&help if(@ARGV == 0 || $ARGV[0] eq "-h" || $name eq "");

# Check lock file
# ---------------
if(-s "$ENV{C}/CBERN.lock") {
  print "\$C/CBERN.lock : ";
  open(LCK,"<$ENV{C}/CBERN.lock");
  print $_ while(<LCK>);
  close(LCK);
  exit;
}
else {
  open(LCK,">$ENV{C}/CBERN.lock");
  print LCK "$ENV{USER} is running ".&basename($0)." since ".localtime(time).
            ", PID: $$\n";
  close(LCK);
}

# Check for tab characters
# ------------------------
if(&check4tab("-b",glob("$ENV{C}/{INC,LIB,PGM}/FOR/*.{f,f90,c,inc}"))) {
  unlink("$ENV{C}/CBERN.lock");
  exit;
}

# Make command
# ------------
sub cbern_private {

  use Sys::Hostname qw(hostname);

  my(%os) = &os;
  my($compiler,$ext) = ("","");
  $compiler = $_[0];
  $ext      = $_[1] if($_[1]);

  my $libName    = "$ENV{C}$os{sl}LIB$os{sl}libBERN_$compiler$ext$os{lib_suffix}";
  my $objDirName = "$ENV{C}$os{sl}LIB$os{sl}OBJ_$compiler$ext";
  my $exeDirName = "$ENV{C}$os{sl}PGM$os{sl}EXE_$compiler$ext";

# What to compile?
# ----------------
  if($name eq "COMPLINK" && $optN eq "") {
    unlink(grep { -f $_ } glob("$libName"));
    unlink(grep { -f $_ } glob("$objDirName/*$os{obj_suffix}"));
    unlink(grep { -f $_ } glob("$objDirName/*.mod"));
    unlink(grep { -f $_ } glob("$exeDirName/*.map"));
    unlink(grep { -f $_ } glob("$exeDirName/*.pdb"));
    unlink(grep { -f $_ } glob("$exeDirName/*.dwf"));
    unlink(grep { -f $_ } glob("$exeDirName/*$os{exe_suffix}"));
    $optMake .= " -B" unless($optMake =~ / -B/);
  }
  elsif(lc $name eq "version") {
    $target = "version";
  }
  elsif($name eq "LIB") {
    $target = $libName;
  }
  elsif($name eq "ALL") {
    $target = "all";
  }
  elsif(-f "$ENV{FG}/$name.f" || -f "$ENV{FG}/$name.f90") {
    $target = "$exeDirName$os{sl}$name$os{exe_suffix}";
  }
  elsif($name ne "" && $name ne "COMPLINK") {
    $target = "$name$os{obj_suffix}";
  }

# Load module if necessary
# ------------------------
  my($mod) = "";

# Print header
# ------------
  print "\n\n","="x80;
  print "\nCompiling the programs for $ENV{USER}\@".&hostname." (".$^O.")";
  print "\n","-"x80;
  print "\nTaget (\$C) = $ENV{C}";
  print "\nCompiler   = $compiler\n";
  # Fortran compiler version
  open(VERS,"cd $objDirName; ${mod}make version 2>&1 |");
  my @vers=<VERS>;
  close VERS;
  my $text = "Version    = ";
  for ( my $ii = 1; $ii<= $#vers; $ii++ ) {
    next if $vers[$ii] =~ /^[ ]*$/;
    print "$text$vers[$ii]";
    $text = "             ";
  }
  print "Started at = " . localtime(time);
  print "\n","="x80,"\n\n";

# Make
# ----
  print "cd $objDirName\n";
  chdir($objDirName);
  print  "${mod}make $optMake $optN $target\n";
  system("${mod}make $optMake $optN $target 2>&1");

# Special 'touch' option
# ----------------------
  if($optT ne "") {
    while(system("make -q") != 0) {
      print  "${mod}make $optMake $optT\n";
      system("${mod}make $optMake $optT");
    }
    unlink($libName) if $optN eq "";
    print  "${mod}make $optMake $optN $libName\n";
    system("${mod}make $optMake $optN $libName");
  }

# Print footer
# ------------
  print "\n\n","="x80,"\n";
  print "Compilation finished\n";
}

# Loop over all compilers
# -----------------------
&filter_make("mail",&basename($0),"$name",&compilers)
unless($ENV{OS} =~ /win/i);

foreach(&compilers) {
  if($ENV{F_DEBUG}) {
    if($ENV{F_DEBUG} eq "NO") {     &cbern_private($_); }
    elsif($ENV{F_DEBUG} eq "YES") { &cbern_private($_,"c"); }
    else { unlink("$ENV{C}/CBERN.lock");
           die "F_DEBUG has to be set to \"YES\" or \"NO\"\n"; }
  } else {
    &cbern_private($_);
    &cbern_private($_,"c");
  }
}

close(STDOUT);

# Delete lock file
# ----------------
unlink("$ENV{C}/CBERN.lock");

# ==============================================================================

__END__
