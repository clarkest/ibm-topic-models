binmode STDOUT, ":utf8";

my $first_row = <>;
print $first_row . "\tnew_id\n";

my $new_id = 1000000;

while (<>) {
    chomp;
    my @fields = split /\t/, $_;

    push(@fields, "world-" . $new_id);
    
    print join("\t", @fields) . "\n"; 
    
    $new_id++;
}
