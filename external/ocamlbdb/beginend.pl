
$take=0;
while ($line=<>) {
if ($line=~/OBDB_BEGIN/) {
if ($take==1) {die "two BEGINs!";} else {$take=1;}
} elsif ($line=~/OBDB_END/) {
	if ($take==0) {die "END without matching BEGIN";} else {$take=0;}
} else {
	if ($take==1) {print "$line";}
}
}
