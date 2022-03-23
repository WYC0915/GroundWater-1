#!/usr/bin/perl
# ==============================================================================
#
# Purpose:    Generate Makefile for the Bernese Software
#
# Author:     L.Mervart
#
# Created:    09-Jul-2005
#
# Changes:    11-Dec-2007 RD: Add a list of pglot programs
#             27-Apr-2010 LM/SL: Multi-compiler support
#             28-Apr-2010 SL: Error if $opt_r is not a readable directory
#             10-Jun-2010 SL: STDOUT changed
#             09-Dec-2010 SL: Create Makefile for DEBUG option
#             27-Aug-2012 SL: Use &os from cmp_util
#             14-Sep-2012 SL: Use &compilers from cmp_util
#
# Copyright:  Astronomical Institute
#             University of Bern
#             Switzerland
#
# ==============================================================================

require 5;
use strict;
use File::Basename qw(basename fileparse fileparse_set_fstype);
use Getopt::Std;
use vars qw( $opt_p $opt_r $opt_g );

use lib "$ENV{X}/EXE";
use cmp_util qw(os compilers checkXG);

# List of plot programs
# ---------------------
my @plotPgm = ("PLTGEN");

# GLOBAL VARIABLES
##############################################################################
my($file,$include,$line,$module,$name,$object,$path,$suffix,$target);

my $endline = $/;
my @src_suffixes;
if($ENV{'F_VERS'} eq "DF95") {
  @src_suffixes = ( q/\.F/, q/\.F90/, q/\.f/, q/\.f90/ );
}
else {
  @src_suffixes = ( q/\.F/, q/\.F90/, q/\.f/, q/\.f90/, q/\.c/);
}
my @inc_suffixes = ( q/\.H/, q/\.fh/, q/\.h/, q/\.inc/ );

my(%os) = &os;
if($ENV{'OS'} =~ /win/i) { fileparse_set_fstype("MSWin32"); }

my %delim_match = ( q/'/ => q/'/,
                    q/"/ => q/"/,
                    q/</ => q/>/ );

my %obj_of_module;    # hash returning name of object file containing module
my %modules_used_by;  # hash of modules used by a given source file (hashed against the corresponding object)
my %includes_in;      # hash of includes in a given source file (hashed against the corresponding object)
my %scanned;          # list of directories/files already scanned
my %src_of;           # hash returning sourcefile from object
my @src_pgm;
my @obj_lib;

# Add Trailing Slash
##############################################################################
sub ensureTrailingSlash {
  local $/ = $os{sl};
  chomp $_[0];
  $_[0] .= $os{sl};
}

# Print formatted List
##############################################################################
format MAKEFILE =
^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< \~
$line
.

sub print_list {
  $line = "@_";
  local $: = " \t\n";
  while(length $line > 254) {
    write MAKEFILE;
  }
  print MAKEFILE $line unless $line eq '';
  print MAKEFILE "\n";
}

# Scan File for USE, MODULE, and INCLUDE Statements
##############################################################################
sub scanfile {
  my $object = shift;
  my $file = shift;
  local $/ = $endline;

  if($scanned{$file}) {
    return;
  }

  my $isProgram = 0;

  open FILE, $file or die "\aERROR opening file $file of object $object: $!\n";
  foreach $line(<FILE>) {
    if($line =~ /^\s*module\s+(\w*)/ix) {
      if($1) {
        my $module = lc $1;
        if($obj_of_module{$module} && $module ne "procedure") {
          die "\a\nAMBIGUOUS: Module $module is associated with $file as well as $src_of{$obj_of_module{$module}}.\n";
        }
        $obj_of_module{$module} = $object;
      }
    }
    if($line =~ /^\s*use\s*(\w*)/ix) {
      $modules_used_by{$object} .= ' ' . lc $1 if $1;
    }
    if($line =~ /^[\#\s]*include\s*(['""'<])([\w\.\/]*)$delim_match{$1}/ix) {
      $includes_in{$file} .= ' ' . $2 if $2;
    }
    if($line =~ /^\s*program\s+(\w*)/ix) {
      $isProgram = 1;
    }
  }
  close FILE;

  if($isProgram == 1) {
    push @src_pgm, $file;
  }
  elsif(index(":".join(":",@obj_lib).":",":".${object}.":") == -1) {
    push @obj_lib, $object unless $object eq "BDS.o";
  }

  $scanned{$file} = 1;
}

# MAIN PROGRAM
##############################################################################
getopts( 'r:p:g' ) && $opt_r ||
##     die "\aUsage: $0 -r bern_root [-p project_root] [-g]\n";
  die "\nNAME:     ".&basename($0)."\n".
      "\nPURPOSE:  Create Makefile for program compilation.\n".
      "\nSYNOPSIS: ".&basename($0)." -r BERN_ROOT [OPTIONS]\n".
      "\nOPTIONS:  -r    Path to software (e.g., \$C)\n".
      "\nEXAMPLE:  ".&basename($0)." -r \$C\n".
      "\n";

# Source Directories
# ------------------
my $C  = $opt_r; ensureTrailingSlash($C); chop $C;
unless(-d $C) {
  die "\n",
      " *** PL ".uc(basename($0,".pl"))."  (".localtime().")\n",
      "        '$opt_r' is not a readable directory.\n\n";
}
my $X  = $C.$os{sl}."GPS";
my $I  = $C.$os{sl}."INC".$os{sl}."FOR";
my $LG = $C.$os{sl}."LIB".$os{sl}."FOR";
my $FG = $C.$os{sl}."PGM".$os{sl}."FOR";

my $objDirName;
my $exeDirName;

# Make Makefile (single compiler)
# -------------------------------
sub makemake_private {

  my($compiler,$ext) = ("","");
  $compiler = $_[0];
  $ext      = $_[1] if($_[1]);

  my $mkfile   = 'Makefile';
  my $template = $X.$os{sl}."EXE".$os{sl}."Makefile.template";

  my @targets;

  if ($opt_p) {
   $objDirName = $opt_p.$os{sl}."OBJ_".$compiler.$ext;
   $exeDirName = $opt_p.$os{sl}."EXE_".$compiler.$ext;
   @targets = ($opt_p.$os{sl}."FOR");
  }
  else {
   $objDirName = $C.$os{sl}."LIB".$os{sl}."OBJ_".$compiler.$ext;
   $exeDirName = $C.$os{sl}."PGM".$os{sl}."EXE_".$compiler.$ext;
   @targets = ($FG,$LG,$I);
  }

  mkdir $exeDirName;
  mkdir $objDirName;
  mkdir $objDirName.$os{sl}.$os{fppDir};

  chdir $objDirName;

  my $LN_PGPLOT = $ENV{'OS'} eq 'UNIX' ?
                  "-L$ENV{'gtools'}/pgplot/lib -L/usr/lib -lpgplot -lX11" : "";

  # Open the Makefile
  # -----------------
  open MAKEFILE,(">$mkfile") or die "$! ($mkfile)\n";
  print "'$objDirName$os{sl}$mkfile'\n";
  print MAKEFILE "# ","="x78,"\n";
  print MAKEFILE "# Created by ".basename($0)." at ".localtime()."\n";
  print MAKEFILE "# ","="x78,"\n\n";
  print MAKEFILE "F_VERS  = $compiler\n\n";
  print MAKEFILE "F_DEBUG = YES\n\n" if($ext eq "c");
  print MAKEFILE "include $template\n\n";
  print MAKEFILE "all: all-make\n\n";
  print MAKEFILE "version:\n\t\$(FC) \$(FCVERS)\n\n";

  # Loop over all Targets Directories
  # ---------------------------------
  foreach $target(@targets) {
    ensureTrailingSlash($target) if(-d $target);

    if(!$scanned{$target}) {
      opendir DIR, $target;
      my @files = readdir DIR;

      # Loop over all sourcefiles in directory
      # --------------------------------------
      foreach(@files) {
        ($name,$path,$suffix) = fileparse("$target$_",@inc_suffixes);
        ($name,$path,$suffix) = fileparse("$target$_",@src_suffixes);
        next if $name =~ /^\.#/;
        $object = "$name$os{obj_suffix}";
        if($suffix && !$src_of{$object}) {
          $src_of{$object} = "$path$name$suffix";
        }
      }
      closedir DIR;
      $scanned{$target} = 1;
    }
  }
  delete $src_of{''};

  my @dirs    = keys %scanned;
  my @srcs    = values %src_of;
  my @objects = keys  %src_of;

  foreach $object(@objects) {
    &scanfile($object,$src_of{$object});
  }

  my %off_srcs;   # list of source files not in current directory
  my %includes;   # global list of includes
  my %used;       # list of object files that are used by others
  my @cmdline;

  # Write down Dependencies on includes and modules
  # -----------------------------------------------
  foreach $object(@objects) {
    my %is_used;
    my %obj_of_include;
    $is_used{$object} = 1;
    ($name,$path,$suffix) = fileparse($src_of{$object},@src_suffixes);
    @cmdline = "$object: $src_of{$object}";
    $off_srcs{$src_of{$object}} = 1 unless($path eq ".$os{sl}" or $path eq '');
    &get_include_list($object,$src_of{$object});
    foreach $module(split /\s+/,$modules_used_by{$object}) {
      $target = $obj_of_module{$module};
      if($target and !$is_used{$target}) {
        $is_used{$target} = 1;
        push @cmdline, "$target";
        $used{$target} = 1;
      }
    }
    &print_list(@cmdline);
    ensureTrailingSlash($path);
    my $source     = "$path$name$suffix";
    my $cpp_source = ".".$os{sl}.$os{fppDir}.$os{sl}.$name.$suffix;
    if($suffix eq ".f") {
      print MAKEFILE "\t\$(CPP) \$(CPPDEFS) $source $cpp_source\n";
      print MAKEFILE "\t\$(F77) \$(FFLAGS) \$(FPPDEFS) $cpp_source\n\n";
    }
    elsif($suffix eq ".f90") {
      print MAKEFILE "\t\$(CPP) \$(CPPDEFS) $source $cpp_source\n";
      print MAKEFILE "\t\$(FC) \$(FCFLAGS) \$(FPPDEFS) $cpp_source\n\n";
    }
    elsif($suffix eq ".c") {
      print MAKEFILE "\t\$(CC) \$(CFLAGS) \$(FPPDEFS) \$(CPPDEFS) $source\n\n";
    }
    else {
      die "Unknown suffix $suffix\n";
    }

    # subroutine to seek out includes recursively
    # -------------------------------------------
    sub get_include_list {
      my($incfile,$incname,$incpath,$incsuffix );
      my @paths;
      my $object = shift;
      my $file = shift;
      foreach(split /\s+/,$includes_in{$file}) {
        ($incname,$incpath,$incsuffix) = fileparse($_,@inc_suffixes);
        if($incsuffix) {
          undef $incpath if $incpath eq ".$os{sl}";
          if($incpath =~ /^[\/\\]/) {
            @paths = $incpath;
          } else {
            @paths = reverse($path,@dirs);
          }
          foreach(@paths) {
            local $/ = "$os{sl}"; chomp;
            my $newincpath = "$_/$incpath" if $_;
            undef $newincpath if $newincpath eq ".$os{sl}";
            $incfile = "$newincpath$incname$incsuffix";
            if(-f $incfile and $obj_of_include{$incfile} ne $object) {
              push @cmdline, "$newincpath$incname$incsuffix";
              $includes{$incfile} = 1;
              chomp($newincpath,$path);
              $off_srcs{$incfile} = 1 if $newincpath;
              $newincpath = '.' if $newincpath eq '';
              &scanfile($object,$incfile);
              $obj_of_include{$incfile} = $object;
              &get_include_list($object,$incfile);
              last;
            }
          }
        }
      }
    }
  }

  # Finish the Makefile
  # -------------------
  &print_list("OBJ_LIB =",@obj_lib);
  print MAKEFILE "\n";

  my $lib_name      = "BERN_".$compiler.$ext;
  my $lib_name_full = $C.$os{sl}."LIB".$os{sl}."lib".$lib_name.$os{lib_suffix};

  if(!$opt_p) {
    print MAKEFILE "$lib_name_full: \$(OBJ_LIB)\n";
    print MAKEFILE "\t\$(AR) \$(ARFLAGS)$os{spc}$lib_name_full \$(OBJ_LIB)\n\n";
  }

  my @programs;
  foreach my $pgm(@src_pgm) {
    ($name,$path,$suffix) = fileparse($pgm,@src_suffixes);
    my $objFile = "$name$os{obj_suffix}";
    my $exeFile;
    my $pgPlot = "";
    foreach my $pgmnam(@plotPgm) {
      if($name eq $pgmnam or $opt_g) {
        $pgPlot = $LN_PGPLOT;
        last;
      }
    }

    $exeFile = $exeDirName.$os{sl}.$name.$os{exe_suffix};
    push @programs, $exeFile;

    if($opt_p) {
      print MAKEFILE "$exeFile: $objFile \$(OBJ_LIB)\n";
      print MAKEFILE "\t\$(LD) \$(LDFLAGS)$os{spc}$os{optOut}$exeFile $objFile \$(OBJ_LIB) -L$C$os{sl}"."LIB -l$lib_name $pgPlot\n\n";
    }
    else {
      print MAKEFILE "$exeFile: $objFile $lib_name_full\n";
      print MAKEFILE "\t\$(LD) \$(LDFLAGS)$os{spc}$os{optOut}$exeFile $objFile $os{L_opt}$C$os{sl}" .
                     "LIB $os{l_opt}$os{lib_prefix}$lib_name $pgPlot\n\n";
    }
  }

  &print_list("PROGRAMS =",@programs);
  print MAKEFILE "\nall-make: \$(PROGRAMS)\n\n";

  close MAKEFILE;
}

my($utc) = sprintf("%02i%02iZ",(gmtime(time))[2],(gmtime(time))[1]);
print "\n".&basename($0)." started at: ".localtime(time)." ($utc)\n\n";

# Prepare compiler-specific directories
# -------------------------------------
foreach(&compilers) {
  &makemake_private($_);
  &makemake_private($_,"c") unless($opt_p);
}

# Check F_VERS/XG consistency
# ---------------------------
&checkXG unless($opt_p);

print "\n".&basename($0)." finished at: ".localtime(time)."\n\n";

# ==============================================================================

__END__
