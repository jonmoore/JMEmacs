#!/usr/bin/perl
# (>>>FILE<<<)                   (>>>AUTHOR<<<)
# (>>>PNFS<<<)                   (>>>DATE<<<)

use warnings;
use strict;
$|=1;
use Data::Dumper;

use File::Path     qw( mkpath );
use File::Basename qw( fileparse basename dirname );
use File::Copy     qw( copy move );
use Fatal          qw( open close mkpath copy move );
use Cwd            qw( cwd abs_path );

use Env qw(HOME);

our $VERSION = 0.01;
my  $prog    = basename($0);

use Getopt::Std;
my %opt = ();
getopts('d', \%opt);
my $DEBUG   = $opt{d} || 1;   # TODO set default to 0 when in production


(>>>POINT<<<)






__END__

=head1 NAME

(>>>FILE<<<) - (( TODO insert brief description ))

=head1 SYNOPSIS

  (>>>FILE<<<) -[options] [arguments]

  Options:
     -d          debug

=head1 OPTIONS

=over 8

=item B<-d>

Turn on debug messages.

=back

=head1 DESCRIPTION

B<(>>>FILE<<<)> is a script which

(( TODO  insert explaination
   This is stub documentation created by template.el.  ))

=head1 AUTHOR

(>>>USER_NAME<<<), E<lt>(>>>EMAIL_DOT_EMACS<<<)E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)

=head1 BUGS

None reported... yet.

=cut
