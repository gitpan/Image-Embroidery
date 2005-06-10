package Image::Embroidery;

use 5.006;
use strict;
use warnings;
use Carp;
use IO::File;
use Bit::Vector;

use vars qw(
	$VERSION
	@ISA
	@EXPORT_OK
	$NORMAL
	$JUMP
	$COLOR_CHANGE
);

require Exporter;

@ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw($NORMAL $JUMP $COLOR_CHANGE) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

$VERSION = '0.2';

$NORMAL = 0;
$JUMP = 1;
$COLOR_CHANGE = 2;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $self  = {};

  $self->{'x_size'} = 0;
  $self->{'y_size'} = 0;
  $self->{'filename'} = '';
  $self->{'data'} = {};

  bless ($self, $class);
  return $self;
}

sub read_file {
  my ($self, $file, $type) = @_;

  unless(defined($file)) { carp("No filename provided"); return 0; }
  unless(-f "$file" and -r "$file") { carp("File $file unreadable or nonexistant"); return 0; }
  my $fh = IO::File->new($file) or carp("Unable to open $file") and return 0;
  
  $self->{'filename'} = $file;
  
  unless(defined($type)) { $type = 'tajima'; }

  if($type eq 'tajima') {
    return read_tajima_file($self, $fh);
  }
  else {
    carp("Request to read unknown file type!");
  }
}

# parse a Tajima DST file
sub read_tajima_file {
  my ($self, $fh) = @_;

  $self->{'data'} = {};
  my $field;
  my $stitch;

  my @x_incr = (  0,  0, 81,-81,  0,  0,  0,  0,
                  3, -3, 27,-27,  0,  0,  0,  0,
                  1, -1, 9,  -9,  0,  0,  0,  0
                );
  my @y_incr = (  0,  0,  0,  0,-81, 81,  0,  0,
                  0,  0,  0,  0,-27, 27, -3,  3,
                  0,  0,  0,  0, -9,  9, -1,  1
               );


  # i don't think the order of these header elements
  # can change, but i'll be flexible.
  while($fh->read($field, 2)) {
    # read the next character, which should be a colon
    # that separates the field name from the value. some
    # file generators forget the colon sometimes, so if
    # we don't get a colon back, we assume it's part of the data
    $fh->read(my $separator, 1);
    unless($separator eq ':') {
      $fh->seek(1,-1);
    }
    if($field eq 'LA') {
      $fh->read(my $label, 16);
      ($self->{'data'}{'LA'} = $label) =~ s/\s*$//;
    }
    elsif($field eq 'ST') {
      $fh->read($self->{'data'}{'ST'}, 7);
      $self->{'data'}{'ST'} = int($self->{'data'}{'ST'});
    }
    elsif($field eq 'CO') {
      $fh->read($self->{'data'}{'CO'}, 3);
      $self->{'data'}{'CO'} = int($self->{'data'}{'CO'});
    }
    elsif($field =~ /^([-+][XY])$/) {
      $fh->read(my $val, 5);
      $self->{'data'}{"$1"} = int($val);
    }
    elsif($field =~ /^([AM][XY])$/) {
      my $field_name = $1;
      $fh->read(my $val, 6);
      $val =~ s/ //g;
      if($val =~ /^[\+\-]?\s*\d+$/) {
        $self->{'data'}{"$field_name"} = int($val);
      }
      else { $self->{'data'}{"$field_name"} = 0; }
    }
    elsif($field eq 'PD') {
      $fh->read($self->{'data'}{'PD'}, 9);
    }
    elsif(unpack('H6', $field) eq '2020') { last; }
    else { carp("Invalid header field: $field"); return 0; }

    # eat the CR that follows each field (except the last one, in which
    # case we're eating a 0x20)
    $fh->read(my $junk, 1);
  }

  $self->{'data'}{'x_size'} = $self->{'data'}{'+X'} + $self->{'data'}{'-X'};
  $self->{'data'}{'y_size'} = $self->{'data'}{'+Y'} + $self->{'data'}{'-Y'};

  # skip to the end of the header
  $fh->seek(512, 0);

  # the file spec for Tajima DST indicates that bits 0 and 1 of a
  # stitch should always be '1', but since they don't mean anything,
  # we just require them to be consistent throughout the file.
  # we store the values in the first stitch that we find, then 
  # compare subsequent stitches to the first value we saw. 
  my $stitch_bit_0;
  my $stitch_bit_1;

  while($fh->read($stitch, 3)) {
    my $v = Bit::Vector->new(24);
    $v->from_Hex(unpack('H6', $stitch));

    if(defined($stitch_bit_0)) {
      unless($v->bit_test(1) == $stitch_bit_1 and
             $v->bit_test(0) == $stitch_bit_0) {
        carp("Possibly corrupt data file: ", unpack('H6', $stitch));
      }
    } else {
      $stitch_bit_0 = $v->bit_test(0);
      $stitch_bit_1 = $v->bit_test(1);
    }

    # bit 6 is off for jumps and normal stitches
    if(!$v->bit_test(6)) {
      my ($x, $y);
      foreach my $index(0..23) {
        $x += $x_incr[$index] if($v->bit_test($index));
        $y += $y_incr[$index] if($v->bit_test($index));
      }

      # bit 7 will be off for normal stitches, on for jumps
      push(@{$self->{'data'}{'pattern'}}, [ $v->bit_test(7), $x, $y ]);
    }
    elsif(!$v->bit_test(7)) {
      carp("Invalid operation code");
      return 0;
    }
    else {
      if($v->to_Hex() eq '0000C3') {
        push(@{$self->{'data'}{'pattern'}}, [ $COLOR_CHANGE ]);
      }
      # this is the 'stop' code. sometimes there is trailing data, so
      # stop reading now.
      elsif($v->to_Hex() eq '0000F3') {
        last;
      } else {
        carp("Invalid operation code");
        return 0;
      }
    }
  }
  return 1;
}

sub write_file {
  my ($self, $file, $type) = @_;

  unless(defined($self->{'data'}{'pattern'})) {
    carp("You do not have a pattern to write");
    return 0;
  }

  unless(defined($file)) { $file = $self->{'filename'}; }
  my $fh = IO::File->new($file, "w") or carp("Unable to write to $file") and return 0;

  unless(defined($type)) { $type = 'tajima'; }

  if($type eq 'tajima') {
    return write_tajima_file($self, $fh);
  }
  else {
    carp("Request to write unknown file type!");
    return 0;
  }
}

# output a Tajima DST file
sub write_tajima_file {
  my ($self, $fh) = @_;

  printf $fh "LA:%-16s\r", $self->{'data'}{'LA'};
  printf $fh "ST:%07d\r", $self->{'data'}{'ST'};
  printf $fh "CO:%03d\r", $self->{'data'}{'CO'};
  for('+X', '-X', '+Y', '-Y') { printf $fh "$_:%05d\r", $self->{'data'}{$_}; }
  foreach my $key ('AX', 'AY', 'MX', 'MY') {
    if($self->{'data'}{$key} < 0) { printf $fh "$key:-%5s\r", abs($self->{'data'}{$key}); }
    else { printf $fh "$key:+%5s\r", $self->{'data'}{$key}; }
  }
  printf $fh "PD:%9s", $self->{'data'}{'PD'};
  printf $fh ' 'x386;

  foreach my $entry (@{$self->{'data'}{'pattern'}}) {
    if($entry->[0] == $NORMAL or $entry->[0] == $JUMP) {
      print $fh pack('B24', get_move_record(@{$entry}));
    }
    else {
      print $fh pack('H6', '0000C3');
    }
  }

  # this is the 'stop' code
  print $fh pack('H6', '0000F3');
  
  $fh->close();

  return 1;
}

sub get_move_record {
   my ($jump,$x,$y) = @_;
   my ($b0, $b1, $b2);

   my %x = get_components($x);
   my %y = get_components($y);

   # byte 0
   $b0.=($y{  1}?'1':'0');
   $b0.=($y{ -1}?'1':'0');
   $b0.=($y{  9}?'1':'0');
   $b0.=($y{ -9}?'1':'0');
   $b0.=($x{ -9}?'1':'0');
   $b0.=($x{  9}?'1':'0');
   $b0.=($x{ -1}?'1':'0');
   $b0.=($x{  1}?'1':'0');

   # byte 1
   $b1.=($y{  3}?'1':'0');
   $b1.=($y{ -3}?'1':'0');
   $b1.=($y{ 27}?'1':'0');
   $b1.=($y{-27}?'1':'0');
   $b1.=($x{-27}?'1':'0');
   $b1.=($x{ 27}?'1':'0');
   $b1.=($x{ -3}?'1':'0');
   $b1.=($x{  3}?'1':'0');

   # byte 2
   $b2.=($jump?'1':'0');
   $b2.='0';
   $b2.=($y{ 81}?'1':'0');
   $b2.=($y{-81}?'1':'0');
   $b2.=($x{-81}?'1':'0');
   $b2.=($x{ 81}?'1':'0');
   $b2.='1';
   $b2.='1';

   # debug
   # print "x: $x => "; foreach (keys %x) { print "$_ "; } print "\n";
   # print "y: $y => "; foreach (keys %y) { print "$_ "; } print "\n";
   # print "$b0 $b1 $b2\n";

   return($b0.$b1.$b2);
}

sub get_components {
   my ($n) = @_;
   my ($s,%c);

   for my $p (reverse(0..4)) {
      if($n<0) { $n*=-1; $s=!$s; }
      my $m = 0;
      for my $q (0..$p-1) { $m+=3**$q; }
      if($n>=3**$p-$m) { $n-=3**$p; $c{($s?-1:1)*3**$p}=1; }
   }
   return(%c);
}

sub draw_logo {
  my ($self, $im, @colors) = @_;

  unless(defined($self->{'data'}{'pattern'})) {
    carp("You do not have a pattern to display");
    return 0;
  }

  unless(scalar(@colors) == $self->{'data'}{'CO'} + 1) {
    carp($self->{'data'}{'CO'} + 1, " colors required, ", scalar(@colors), " colors supplied");
    return 0;
  }

  my ($x, $y) = ($self->{'data'}{'+X'}, $self->{'data'}{'y_size'} - $self->{'data'}{'+Y'});
  my ($new_x, $new_y);
  
  foreach my $stitch (@{$self->{'data'}{'pattern'}}) {
    if($stitch->[0] == $NORMAL) {
      $new_x = $x + $stitch->[1];
      $new_y = $y - $stitch->[2];
      $im->line($x, $y, $new_x, $new_y, $colors[0]);
      $x = $new_x; $y = $new_y;
    }
    elsif($stitch->[0] == $JUMP) {
      $x = $x + $stitch->[1];
      $y = $y - $stitch->[2];
    }
    elsif($stitch->[0] == $COLOR_CHANGE) { shift @colors; }
  }
                                        
  return 1;
}

sub size {
  my ($self) = @_;
  return ($self->{'data'}{'x_size'}, $self->{'data'}{'y_size'});
}

sub get_color_changes {
  my ($self) = @_;
  return $self->{'data'}{'CO'};
}

sub get_stitch_count {
  my ($self) = @_;
  return $self->{'data'}{'ST'};
}

sub get_end_point {
  my ($self) = @_;
  return ($self->{'data'}{'AX'}, $self->{'data'}{'AY'});
}

sub get_abs_size {
  my ($self) = @_;
  return ($self->{'data'}{'+X'}, $self->{'data'}{'-X'},
          $self->{'data'}{'+Y'}, $self->{'data'}{'-Y'});
}


1;
__END__

=head1 NAME

Image::Embroidery - Parse and display embroidery data files

=head1 SYNOPSIS

  use Image::Embroidery;

  # Constructor
  $emb = Image::Embroidery->new();

=head1 ABSTRACT

Parse and display embroidery data files

=head1 DESCRIPTION

This module can be used to read, write and (with GD)
display embroidery data files. It currently only supports
Tajima DST files, but if there is any interest it could
be expanded to deal with other formats. In its current form
it isn't ideal for creating or modifying patterns, but
I'm reluctant to put much effort into it until someone
tells me they are using it.

=head1 EXAMPLES

This is an example of using the module to manipulate a
data file and write out the changes.

    use Image::Embroidery qw(:all);

    $emb = Image::Embroidery->new();

    $emb->read_file( '/path/to/embroidery.dst' ) or
       die "Failed to read data file: $!";
    
    # fiddle with the data structure some. this would make
    # the 201st entry a normal stitch that went 5 units right,
    # and 7 units up
    $emb->{'data'}{'pattern'}[200] = [ $NORMAL, 5, 7 ];

    # supply a new file name, or use the default of 
    # the original file name
    $emb->write_file( '/path/to/new_embroidery.dst' ) or
        die "Failed to write data file: $!";


This example demonstrates using GD to create an image
file using Image::Embroidery.

    use Image::Embroidery;
    use GD;
    
    $emb = Image::Embroidery->new();
    
    $emb->read_file( '/path/to/embroidery.dst' ) or
        die "Failed to read data file: $!";

    $im = new GD::Image( $emb->size() );
    
    # the first color you allocate will be the background color
    $black = $im->colorAllocate(0,0,0);

    # the order in which you allocate the rest is irrelevant
    $gray = $im->colorAllocate(128,128,128);
    $red = $im->colorAllocate(255,0,0);
    
    # the order you specify the colors is the order in which they
    # will be used. you must specify the correct number of colors
    $emb->draw_logo($im, $gray, $red);

    open(IMG, ">", "/path/to/embroidery.png");
    print IMG $im->png;
    close(F);

=head1 METHODS

=over 4

=item I<draw_logo( $gd_image_object, @colors )>

Write an image of the stored pattern to the supplied 
GD::Image object. You must supply the correct number of
colors for the pattern. Color arguments are those returned by
GD::Image::colorAllocate. Returns 0 on failure, 1 on success.

=item I<get_color_changes()>

Return the number of colors changes in the pattern. To
get the total number of colors in the pattern, add 1
to the value returned by this function.

=item I<get_stitch_count()>

Return the total number of stitches in the pattern.

=item I<get_end_point()>

Returns the position of the last point in the pattern,
relative to the starting point.

=item I<get_abs_size()>

Returns the distance from the starting point to
the edges of the pattern, in the order +X, -X, +Y, -Y.

=item I<read_file( $filename, [ $format ] )>

Read an embroidery data file in the specified file format.
If the format is omitted, the default is 'tajima'. Returns
0 on failure, 1 on success.

=item I<size()>

Returns the X and Y size of the pattern.

=item I<write_file( [ $filename ], [ $format ] )>

Output the contents of the object's pattern to the specified
file, using the specified file format. If the filename
is omitted, the default filename will be the last
file that was successfully read using I<read_file()>. If
the format is omitted, the default is 'tajima'. Returns
0 on failure, 1 on success.

=head1 FILE FORMATS

Currently, only the Tajima DST file format is supported. It
can be specifed to the I<read_file()> and I<write_file()>
routines using the string 'tajima', if you feel like being
specific.

=head1 AUTHOR

Kirk Baucom, E<lt>kbaucom@schizoid.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Kirk Baucom

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
