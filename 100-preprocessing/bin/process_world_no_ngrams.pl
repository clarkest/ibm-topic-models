#binmode STDOUT, ":utf8";
#binmode STDIN, ":utf8";
use utf8;

my @drop_ids = ();
open DROPS, "lists/new_ids_dropped.txt";
while (<DROPS>) {
    chomp;
    
    my $destination = $_;
    $destination =~ s/ /_/g;

    push (@drop_ids, $_);
}
close DROPS;

my @jobtitles = ();

open MANAGERS, "lists/manager_patterns.txt";
while (<MANAGERS>) {
    chomp;
    push @jobtitles, $_;
}
close MANAGERS;

my $first_row = <>;
chomp $first_row;

$first_row =~ s/creation_time/creation_date/;
#$first_row =~ s/Translated Text/text/;
#$first_row =~ s/CommentID/id/;
#$first_row =~ s/Job Responsibilities/manager/;

## add split-off creation time field and manager field
print $first_row . "\tmanager\tcreation_time\n";

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
    my $drop=0;
    chomp;
    my @fields = split /\t/, $_;
    foreach my $id (@drop_ids) {
        if ($id eq $fields[18]) {
            $drop=1;
            print STDERR "dropping " . $id . "\n";
            last;
        }
    }

    if ($drop==0) {
        my $job_resp = $fields[11];
        my $role = "Other";
        foreach my $title (@jobtitles) {
        	if ($job_resp =~ /\Q$title\E/i) {
        	    $role = "Manager";
        	    last;
        	}
        }
        push @fields, $role;

        my $date_time = $fields[6];
        if ($date_time =~ /(\d+\/\d+\/\d+) (.*)/) {
    	   $fields[6] = $1;
    	   push @fields, $2;
        }

        my $title = $fields[12];
        my $text = $fields[17];

        # drop HTML
        $text =~ s/<[^>]+>//g;
        
        # drop URLs
        $text =~ s/http\S+//g;

        # drop arrows
        $text =~ s/->//g;  
        $title =~ s/->//g;  

        # for good measure, just kill off any remaining > or <
        $text =~ s/[>,<]//g;  
        $title =~ s/[>,<]//g;  

        # boilerplate
        $text =~ s/\QNote: Comment formatted to fit web page.\E//;
        $text =~ s/Note: Reply formatted to fit web page.//;

        # get rid of repeated white space in both, as it throws off matches
        $text =~ s/\h+/ /g;
        $title =~ s/\h+/ /g;

        # remove titles
        $text =~ s/^\Q$title\E//;

        # replace " with '
        $text =~ s/"/'/g;
        $title =~ s/"/'/g;

        # broken UTF-8
        $text =~ s/(\p{L})[\xe2]\p{P}+(\p{L})/$1'$2/g;
        #remove failed utf8 that are now ? marks
        # do it a few times to get the repeats, which violate the \B
        $text =~ s/\B\?+//g;
        #$text =~ s/\x{a20a}//g;
        # remove all non ascii
        $text =~ s/[^[:ascii:]]//g;

        $title =~ s/(\p{L})[\xe2]\p{P}+(\p{L})/$1'$2/g;
        $title =~ s/\B\?+//g;
        $title =~ s/[^[:ascii:]]//g;

        # check for duplicates -- BEFORE doing the text replacement.  
        ## this keeps the dupes reduction consistent between ngram and nongram
        ## implementations
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

        if (! $is_duplicate) { 
            $fields[17] = $text;
            $fields[12] = $title;
            print join("\t", @fields) . "\n"; 
        }
        
        push @{$sender_cache{ $fields[0] }}, \%current_words;
    }
}
