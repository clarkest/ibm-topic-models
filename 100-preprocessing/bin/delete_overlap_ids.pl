my $doc = 0;

my %terms = ();
open TERMS, shift @ARGV;
binmode TERMS, ":utf8";
while (<TERMS>) {
    chomp;
    my ($count, $term) = split /\t/, $_;
    $terms{$term} = 0;
}
close TERMS;

my @ids = ();
open IDS, shift @ARGV;
binmode IDS, ":utf8";
while (<IDS>) {
    chomp;
    push @ids, $_;
}
close IDS;

open IN, shift @ARGV;
binmode IN, ":utf8";
binmode STDOUT, ":utf8";

## first line
my $first_line = <IN>;
print $first_line;

while (<IN>) {
    chomp;
    ## ids are in the 17th column
    my @fields = split /\t/, $_;
    my $text = $fields[17];
    my $this_id = $fields[18];
    
    $text =~ s/\s+/ /g;
    
    foreach my $term (keys %terms) {
    	if ($text =~ /\Q$term\E/) {
    	    if ($terms{$term} > 0) { 
    		  $text =~ s/\Q$term\E/ /;
    	    }
    	    $terms{$term}++;
    	}
    }

    my $was_killed = 0;
    foreach my $id (@ids) {
        #print $this_id . ":" . $id . "::" . $was_killed . "\n";
        if ("$this_id" eq "$id") {
            $was_killed = 1;
        }
    }
    
    #print $this_id . "::" . $was_killed . "\n";

    $fields[17] = $text;
    # keep only those that weren't killed in the no-ngram version
    if ($was_killed == 0) {
        print join("\t", @fields) . "\n";
    } 

    # but let us know which ones we would have killed but didn't kill off
    if (($was_killed == 0) && (length $text <= 50)){
        print STDERR join("\t", @fields);
    }    

    # output the id of the document being killed off -- this will
    # be necessary to making sure that the nongrams and ngram corpuses
    # have the same doc set
    #print STDERR "$fields[4]"

    # periodically throw a counter to stderr so that the user doesn't get worried
    #$doc++;
    #if ($doc % 1000 == 0) { print STDERR "$doc\n"; }
}
close IN;
