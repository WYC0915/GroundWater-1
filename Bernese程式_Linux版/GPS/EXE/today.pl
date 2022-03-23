#!/usr/bin/perl
# ============================================================================
#
# Name    :   today
#
# Purpose :   Tool for Conversion of Dates (Using gps_date Module).
#
# Author  :   S.Schaer
#
# Created :   16-Nov-2004
#
# Changes :   05-Jun-2012 SL: Use specific subroutines from modules
#
# ============================================================================

use lib "$ENV{'X'}/EXE";
use Gps_Date qw(gps_date);
use Pod::Text::Termcap;

# Get arguments
@opt=@ARGV;

# Show help if requested
shwHelp() if $opt[0]=~/-h\s*/i;

if (@opt==0) {

# Print default (today) output
  print $str=&gps_date("-today") }
else {

# Add default (doy) option if necessary
  $optdoy=$opt[0]!~/-\D+\s*/ ? "-doy " : "";

# Convert seconds of GPS week into days if indicated
  $opt[2]="0 -d ".$opt[2]/86400 if $opt[0]=~/-wd/ && $opt[2]>6;

# Print output
  print $str=&gps_date("$optdoy@opt") }


# Show help
sub shwHelp {
  Pod::Text::Termcap->new()->parse_from_file($0);
  exit(0) }


__END__


=head1 DESCRIPTION

today.pl: Tool for Conversion of Dates (Using gps_date Module).

=head1 USAGE

today.pl [-h] [I<[-doy] DDD [YY]>] [-ymd I<YYYY MM DD>] [-wd I<WWWW D>] [-mjd I<MJD>]

=head1 EXAMPLES

=item today.pl

=item today.pl -1

=item today.pl 321

=item today.pl 321 2004

=item today.pl -ymd 2004 11 16

=item today.pl -wd 1297 172800

=item today.pl -mjd 53325 -hms 12 00 00

=item today.pl -t -h -12 -o %V%n

=head1 OPTIONS

=over 6

=item -doy

This option is selected by default. For negative I<DDD>, I<DDD> is
considered in a relative sense. I<YY>, or I<YYYY> is optional. If
called without any argument, date information is printed for today.

=item -wd

This option may be called in combination with seconds of GPS week:
I<WWWW SSSSSS> (instead of I<WWWW D>).

=item -h

Shows this help. Type -v to get details on gps_date.

=back

=head1 REMARK

All possible options concerning gps_date are supported!

=cut


