#!/usr/bin/perl -w

# -------------------------------------------------------------------------
# Bernese GPS Software Version 5.0
# -------------------------------------------------------------------------
#
# Script:     RUN_PGMS
#
# Purpose:    Called by from BPE scripts written in shell.  Simply
#             copies some variables from the environment and then
#             calls BPEclient->run.
#
# Author:     D. Hunt, L. Mervart
#
# Created:    7-15-2002
#
# Changes:
#
# Copyright:  Astronomical Institute
#              University of Berne
#                  Switzerland
# -------------------------------------------------------------------------

use strict;
use lib "$ENV{BPE}";
use RUNBPE;

# Create a new BPE client object from a small set of environment variables.
# Use this object to call the 'run' routine (which runs the script with all BPE
# details handled)
my @vars = qw(DEBUG CAMP_PTH PGMNAM U BPE XQ CAMP_DRV CAMPAIGN MJD SESSION
              PRT_FILE SERVER_VARIABLES);
eval { RUNBPE->new(map { $_ => $ENV{$_} } @vars)->RUN_PGMS };
if ($@) {
  print "RUN_PGMS.pl: Exception in $ENV{PGMNAM}:\n$@\n";
  exit 1;
}
exit 0;
