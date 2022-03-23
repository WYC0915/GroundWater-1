#!/usr/bin/perl -w
# ==============================================================================
#
# Name:      cmp_util.pm
#
# Purpose:   This file contains auxililary compilation procedures.
#
# Authors:   S.Lutz
#
# Created:   10-Jan-2012
#
# Changes:   10-Jan-2012 SL: check4tab, filter_make from $X/AUTO/gpsutil.pm
#            26-Jun-2012 RD: msg2key only for "BERN"-version
#            27-Aug-2012 SL: &os for operating system dependent variables
#            14-Sep-2012 SL: &compilers added
#
# ==============================================================================

package cmp_util;

use strict;
use vars qw(@ISA @EXPORT);

our(@ISA)    = qw(Exporter);
our(@EXPORT) = qw(os compilers checkXG filter_make check4tab);

$|=1;

# ==============================================================================
#
# Name:       os
#
# Purpose:    Setting of operating system dependent variables
#
# Authors:    S.Lutz
#
# Created:    27-Aug-2012
#
# Changes:    __-___-____ __:
#
# ==============================================================================

sub os {
  my(%os);
  if($ENV{OS} eq "UNIX") {
    $os{sl}         = "/";
    $os{spc}        = " ";
    $os{optOut}     = "-o ";
    $os{fppDir}     = ".fpp";
    $os{exe_suffix} = "";
    $os{obj_suffix} = ".o";
    $os{lib_suffix} = ".a";
    $os{L_opt}      = " -L";
    $os{l_opt}      = " -l";
    $os{lib_prefix} = "";
  }
  elsif($ENV{OS} =~ /win/i) {
    $os{sl}         = "\\";
    $os{spc}        = "";
    $os{optOut}     = " /OUT:";
    $os{fppDir}     = "fpp";
    $os{exe_suffix} = ".exe";
    $os{obj_suffix} = ".obj";
    $os{lib_suffix} = ".lib";
    $os{L_opt}      = " /LIBPATH:";
    $os{l_opt}      = " /DEFAULTLIB:";
    $os{lib_prefix} = "lib";

    if($ENV{F_VERS} eq "G95") {
      $os{spc}        = " ";
      $os{optOut}     = "-o ";
      $os{obj_suffix} = ".o";
      $os{L_opt}      = " -L";
      $os{l_opt}      = " -l";
    }

    if($ENV{F_VERS} eq "AIUB" || $ENV{F_VERS} eq "PG_F90") {
      $os{optOut}     = "-o ";
      $os{L_opt}      = " -L";
      $os{l_opt}      = " -l";
      $os{lib_prefix} = "";
    }
  }
  else {
    die "\nUnknown operating system $ENV{OS}\n\n";
  }
  return(%os);
}

# ==============================================================================
#
# Name:       compilers
#
# Purpose:    Set up list of compilers
#
# Authors:    S.Lutz
#
# Created:    14-Sep-2012
#
# Changes:    __-___-____ __:
#
# ==============================================================================

sub compilers {
  my(@compilers) = ($ENV{F_VERS});

  my($list) = 1;
  $list = $_[0] if(defined($_[0]));

  if($list && defined($ENV{F_VERS_LIST})) {
    foreach(split(/\W+/,$ENV{F_VERS_LIST})) {
      push(@compilers,$_) if($_ ne $ENV{F_VERS});
    }
  }

  return(@compilers);
}

# ==============================================================================
#
# Name:       checkXG
#
# Purpose:    Check the consistency between $F_VERS and $XG.
#
# Authors:    S.Lutz
#
# Created:    12-Oct-2012
#
# Changes:    __-___-____ __:
#
# ==============================================================================

sub checkXG {
  use File::Basename qw(dirname);

  my %os = &os;

  if(!defined($ENV{F_VERS_LIST}) || $ENV{F_VERS_LIST} eq "") {
    my $XGnew = &dirname($ENV{XG}).$os{sl}."EXE_".$ENV{F_VERS};
    if($ENV{XG} ne $XGnew ) {
      print "\n\n ### Inconsistency in the environment variables";
      print   "\n     F_VERS and XG detected.";
      print "\n\n     Source compiled to:";
      print   "\n       $XGnew";
      print "\n\n     Current path to executables:";
      print   "\n       $ENV{XG}\n";

      print "\nPress return to continue\n";
      my $inp = <STDIN>;
      if(!defined $inp || $inp eq "") {
        exit;
      }
    }
  }
}

# ==============================================================================
#
# Name:       filter_make
#
# Purpose:    Filter make output
#
# Usage:      &filter_make();
#             &filter_make("mail",&basename($0),"$name","@compilers");
#
# Authors:
#
# Created:    __-___-____
#
# Changes:    21-Apr-2010 SL: error strings changed
#             22-Apr-2010 SL: length of $sep and $fin set to 80
#             19-May-2010 SL: changes wrt multi-compiler usage
#             13-Aug-2010 SL: consider .c files to plot "compiling $nam"
#             03-Nov-2010 SL: get warning messages
#             09-Dec-2010 SL: print info if ok=2
#             02-Feb-2011 SL: print list of compiled programs
#             26-Feb-2011 SL: string to get PGM modified, print $HOST in mail
#             12-Sep-2011 SL: ifort patterns added
#
# Remarks:    - Just call filter_make befor writing to STDOUT
#             - When output filtering is done: close STDOUT;
#             - use #RFR# at end of line to remove the line for a release
#
# ==============================================================================

sub filter_make {

  use File::Basename qw(basename);

  my($sep)  = "-"x80;
  my($fin)  = "*"x80;
  my(%err)  = ();
  my(%warn) = ();
  my($info) = " ";
  my($ok)   = 1;

# Try to establish filter (fork)
# ------------------------------
  return if my $pid = open(STDOUT,"|-");
  die "Cannot fork: $!\n" unless defined $pid;

# Catch all output
# ----------------
  my($nam,$compiler,$msg) = ("","","");
  my(@pgm,@msg) = ();
  while(<STDIN>) {

# Get compiler name
# -----------------
    if(/^Compiler: /) {
      $compiler = " (Compiler: ".(split " ")[-1].")";
    }
# Compilation
# -----------
    elsif(/^cc /) {
      $nam = (split " ")[-1];
      print "\n$sep\nCompiling: $nam\n$sep\n";
    }
    elsif(/^fpp / || /^cp  / || /^cpp /) {
      $nam = (split " ")[-2];
      print "\n$sep\nCompiling: $nam\n$sep\n";
      push(@pgm,"$nam\n") if(/\/PGM\//);
    }
# Link programs
# -------------
    elsif(/^f95 +-o / ||
          /^g95 +-o / ||
          /^gfortran +-o / ||
          /^pgf90 +-o / ||
          /^ifort +-o /) {
      $nam = (split " ")[2];
      print "\n$sep\nLinking: $nam\n$sep\n";
    }
# Create library
# --------------
    elsif(/^ar r/) {
      $nam = (split " ")[2];
      print "\n$sep\nCreating library: $nam\n$sep\n";
    }
# Nothing to be done
# ------------------
    elsif(/^make: Nothing/) {
      $info .= $_;
      $ok = 2;
    }
# Up to date
# ----------
    elsif (/^make:.* is up to date.$/) {
      $info .= $_;
      $ok = 2;
    }
# ignore FLEXlm
# -------------
    elsif (/FLEXlm/) {
      $ok = 1;
    }
# Error
# -----
##    elsif (/^make: / || /error[: ]/i || /fatal[: ]/i) {
    elsif(/^make: / ||
          /^Error/ || / Error: / || / Error 1 / ||
          /: error: / || /^PGF90-S-/ || /^PGF90-F-/) {
      $err{$nam} .= $_;
      $ok = 0;
    }
# Warnings
# --------
    elsif(/^Warning/ ||
          /^PGF90-I-/ ||
          /: Info: / ||
          /\) remark: /) {
      $warn{$nam} .= $_;
    }

# Print the line
# --------------
    print;

# Report errors or success
# ------------------------
    if(/Compilation finished/) {
      print localtime(time)."\n";
      if(keys %err) {
        $msg = "\nERROR: Compilation not successful!$compiler";
        push(@msg,$msg);
        print "\n$fin$msg\n$fin\n";
        foreach(sort keys %err) {
          print "\n$sep\n$_\n$sep\n$err{$_}";
#          print "\n=== $_ ","="x(75-length($_)-5),"\n$err{$_}";
        }
        print "\n";
        %err = ();
      }
      elsif($ok == 1) {
        $msg = "\nCompilation successful!$compiler";
        push @msg,$msg;
        print "\n$fin$msg\n$fin\n";
      }
      elsif($ok == 2) {
        push(@msg,$info);
        $info = ();
      }
      if(keys %warn) {
        foreach(sort keys %warn) {
          print "\n$sep\n$_\n$sep\n$warn{$_}";
        }
        print "\n";
        %warn = ();
      }
      print "\n";
    }
  }

  my(%pgm) = map { $_ => 1 } @pgm;
  @pgm = sort keys %pgm;
  push(@msg,"\n\nPrograms:\n @pgm\n") if(@pgm);

# Send message
# ------------
  if(@_ && @_ >= 4 && $_[0] eq "mail") {
    my(@mail) = (
      "'$_[1] $_[2]' finished!",
      "\n\n".
      "User = $ENV{USER}\@$ENV{HOST}\n".
      "C    = $ENV{C}\n".
      "Compilers: @_[3..$#_]\n".
      localtime(time)."\n\n@msg\n"
    );
    print "\n\n";
  }

  print "@msg\n\n" if(@msg && @msg > 1);

# Kill child
# ----------
  exit;
}

# ==============================================================================
#
# Name:       check4tab
#
# Purpose:    Check @_ for the existence of tab characters and return array.
#
# Example:    &check4tab(glob("$ENV{C}/*/FOR/*.{f,f90,inc,c})) && exit;
#             &check4tab("-b",glob("$ENV{C}/*/FOR/*.{f,f90,inc,c})) && exit;
#
# Authors:    S.Lutz
#
# Created:    10-Aug-2010
#
# Changes:    18-Aug-2010 SL: output format for $msg defined
#             02-Sep-2010 LO: F_PROJ may be void
#             04-Apr-2012 SL: check for trailing blanks
#
# ==============================================================================

sub check4tab {

  my(@tabs) = ();
  my($msg)  = "Tab characters";
  my($bl)   = 0;

  if($_[0] eq "-b") {
    $bl = shift(@_);
    $msg .= " and/or trailing blanks";
  }

  foreach my $fil(@_) {
    next unless(-f $fil && -r $fil && !-B $fil);
    open(TMP,"<$fil");
    my($line) = 0;
    $fil =~ s/$ENV{C}\/INC\/FOR/\$\{I\}/g;
    $fil =~ s/$ENV{C}\/LIB\/FOR/\$\{LG\}/g;
    $fil =~ s/$ENV{C}\/PGM\/FOR/\$\{FG\}/g;
    $fil =~ s/$ENV{C}\/TOOL\/FOR/\$\{FT\}/g;
    if(defined $ENV{F_PROJ} && $ENV{F_PROJ} ne ""){
      $fil=~s/$ENV{HOME}\/$ENV{F_PROJ}\/FOR/\$\{FP\}/g;
    }
    while(<TMP>) {
      $line++;
      if($_ =~ /\t/) {
        $_ =~ s/\t/<TAB>/g;
        push(@tabs,sprintf("%-26s %5i|%s",$fil,$line,$_));
      }
      if($bl && $_ =~ / $/) {
        $_ =~ s/ $/<SPC>/g;
        push(@tabs,sprintf("%-26s %5i|%s",$fil,$line,$_));
      }
    }
  close(TMP);
  }

  if(@tabs) {
    print "\n$msg found!\n\n @tabs\n";
    return(@tabs);
  }

}

# ==============================================================================
1;
