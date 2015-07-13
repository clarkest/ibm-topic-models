while (<STDIN>) {
    s/(\p{L})â\p{P}+(\p{L})/$1'$2/g;
    s/â\W+/ /g;
    print;
}
