
while (<>) {
	if (($stuff) = $_ =~ /^\-\-(.+)/) {
	} else {
		print "$_";
	}
}

