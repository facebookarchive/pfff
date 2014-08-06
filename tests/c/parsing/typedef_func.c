
// this should work, but if have some weird code before then it fails :(

BIOS32si*
bios32open(char* id)
{
}


static Chan*
mouseattach(char *spec)
{
    return devattach('m', spec);
}
