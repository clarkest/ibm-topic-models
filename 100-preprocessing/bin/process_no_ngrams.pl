binmode STDOUT, ":utf8";

#my %abbrev_replacements = ();
my @source = ();
my @target = ();

#my %phrase_replacements = ();
open REPS, "lists/phrases.txt";
while (<REPS>) {
    chomp;
    # my $source = $_;
    my $destination = $_;
    $destination =~ s/ /_/g;

    #$phrase_replacements{$source} = $destination;
    push (@source, $_);
    push (@target, $destination);
    #print "$source to $destination\n";
}
close REPS;

#for my $i (0..$#source) {
#    print "$source[$i] to $target[$i]\n"
#}
#exit;
my @jobtitles = ();

open MANAGERS, "lists/manager_patterns.txt";
while (<MANAGERS>) {
    chomp;
    push @jobtitles, $_;
}
close REPS;

my $first_row = <>;
print $first_row;

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
    my @fields = split /\t/, $_;

    my $job_resp = $fields[14];
    my $role = "Other";
    foreach my $title (@jobtitles) {
	if ($job_resp =~ /\Q$title\E/i) {
	    $role = "Manager";
	    last;
	}
    }
    $fields[7] = $role;

    my $title = $fields[15];
    my $text = $fields[18];
    
    # drop HTML
    $text =~ s/<[^>]+>//g;
    
    # drop URLs
    $text =~ s/http\S+//g;
    
    # drop arrows
    $text =~ s/->//g;  
    $title =~ s/->//g; 

    # boilerplate
    $text =~ s/\QNote: Comment formatted to fit web page.\E//;

    # get rid of repeated white space in both, as it throws off matches
    $text =~ s/\h+/ /g;
    $title =~ s/\h+/ /g;

    # remove titles
    $text =~ s/^\Q$title\E//;

    # replace " with '
    $text =~ s/"/'/g;
    $title =~ s/"/'/g;

    #foreach my $source (keys %replacements) {
    for my $i (0..$#source) {
	   #my $target = $replacements{$source};
	   #$text =~ s/\Q$source\E/$target/gi;
       #$title =~ s/\Q$source\E/$target/gi;
       $text =~ s/\b$source[$i]\b/$target[$i]/gi;
       $title =~ s/\b$source[$i]\b/$target[$i]/gi;
    }

    $fields[18] = $text;
    $fields[15] = $title;

    ## check for duplicates
    my %current_words = ();
    my @tokens = split /\s+/, $text;
    foreach my $token (@tokens) {
	   $current_words{$token}++;
    }

    my $is_duplicate = 0;
    if (defined $sender_cache{ $fields[0] }) {
	foreach my $previous_message (@{$sender_cache{ $fields[0] }}) {
	    if (l1_diff(\%current_words, $previous_message) < 3) {
		$is_duplicate = 1;
		
		#my $previous_text = substr(join(" ", sort keys(%{$previous_message})), 0, 45);
		#my $current_text = substr(join(" ", sort keys(%current_words)), 0, 45);
		#print "$previous_text\t$current_text\n";
	    }
	}
    }

    if (! $is_duplicate) { print join("\t", @fields) . "\n"; }
    
    push @{$sender_cache{ $fields[0] }}, \%current_words;
}
