#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

#include <iostream>
#include <google/protobuf/text_format.h>

#include "piqi.piqi.pb.h"


using namespace std;
using namespace google::protobuf;


int main(int argc, char *argv[])
{
    int fd = open("piqi.piqi.pb", O_RDONLY);

    ::piqi_org::piqi::piqi piqi;
    piqi.ParseFromFileDescriptor(fd);

    string s;
    TextFormat::PrintToString(piqi, &s);

    cout << s; //<< endl;

    close(fd);
    return 0;
}

