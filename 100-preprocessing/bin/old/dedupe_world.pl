my @previous_messages = ();
my @previous_lengths = ();

binmode STDOUT, ":utf8";

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

my $row = 0;

while (<>) {
    chomp;
    my @fields = split /\t/, $_;

    my $text = $fields[17];

    ## check for duplicates
    my %current_words = ();
    my @tokens = split /\s+/, $text;
    foreach my $token (@tokens) {
	   $current_words{$token}++;
    }
    
    my $length = scalar @tokens;

    my $is_duplicate = 0;
    for (my $doc = 0; $doc < scalar @previous_messages; $doc++) {
    	if (abs($length - $previous_lengths[$doc]) < 3 && 
    	    l1_diff(\%current_words, $previous_messages[$doc]) < 3) {
    	    $is_duplicate = 1;
    	    last;
    	}
    }

    if (! $is_duplicate) {
    	print join("\t", @fields) . "\n";
    	push @previous_messages, \%current_words;
    	push @previous_lengths, $length;
    }

    # periodically throw a counter to stderr so that the user doesn't get worried
    $row++;
    if ($row % 1000 == 0) { print STDERR "$row\n"; }
}
