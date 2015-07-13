
my %replacements = ();

open REPS, "phrases.txt";
while (<REPS>) {
    chomp;
    my $source = $_;
    my $destination = $source;
    $destination =~ s/ /_/g;

    $replacements{$source} = $destination;
}
close REPS;

#my $first_row = <>;
#chomp $first_row;

my %sender_cache = ();

sub l1_diff {
    my $left = shift @_;
    my $right = shift @_;
    
    my $difference = 0;
    foreach my $w (keys %{$left}) {
	if (defined $right->{$w}) {
	    $difference += abs($right->{$w} - $left->{$w});
	}
	else {
	    $difference += $left->{$w};
	}
    }
    foreach my $w (keys %{$right}) {
	if (! defined $left->{$w}) {
	    $difference += $right->{$w};
	}
    }

    return $difference;
}

while (<>) {
    chomp;
    my @fields = split /;/, $_;

    my $text = $fields[18];

    
    # remove titles
    $text =~ s/^\Q$title\E//;
    
    # drop HTML
    $text =~ s/<[^>]+>//g;
    
    # drop URLs
    $text =~ s/http\S+//g;

    # broken UTF-8
 
    $text =~ s/(\p{L})[^\x00-\x7F]\p{P}+(\p{L})/$1'$2/g;
    $text =~ s/[^\x00-\x7F]\W+/ /g;

    
    $title =~ s/(\p{L})[^\x00-\x7F]\p{P}+(\p{L})/$1'$2/g;
    $title =~ s/[^\x00-\x7F]\W+/ /g;

    # boilerplate
    $text =~ s/\QNote: Comment formatted to fit web page.\E//;
    $text =~ s/Note: Reply formatted to fit web page.//;

    foreach my $source (keys %replacements) {
	my $target = $replacements{$source};
	$text =~ s/\Q$source\E/$target/gi;
    }

    $fields[0] = $text;

    print $text . "\n";
    # print join("\t", @fields) . "\n"; 
    
    # push @{$sender_cache{ $fields[0] }}, \%current_words;
}
