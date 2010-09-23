
while (<>) {
	if (($stuff) = $_ =~ /^\-\-(.+)/) {
		print "$stuff\n";
	}
}

