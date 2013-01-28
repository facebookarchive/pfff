#import <Foundation/NSArray.h>
#import <Foundation/NSString.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSEnumerator.h>
#import <stdio.h>

void print( NSArray *array ) {
    NSEnumerator *enumerator = [array objectEnumerator];
    id obj;

    while ( obj = [enumerator nextObject] ) {
        printf( "%s\n", [[obj description] cString] );
    }
}

int main( int argc, const char *argv[] ) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSArray *arr = [[NSArray alloc] initWithObjects:
                    @"Me", @"Myself", @"I", nil];
    NSMutableArray *mutable = [[NSMutableArray alloc] init];

    // enumerate over items
    printf( "----static array\n" );
    print( arr );

    // add stuff
    [mutable addObject: @"One"];
    [mutable addObject: @"Two"];
    [mutable addObjectsFromArray: arr];
    [mutable addObject: @"Three"];

    // print em
    printf( "----mutable array\n" );
    print( mutable );

    // sort then print
    printf( "----sorted mutable array\n" );
    [mutable sortUsingSelector: @selector( caseInsensitiveCompare: )];
    print( mutable );
    
    // free memory
    [arr release];
    [mutable release];
    [pool release];

    return 0;
}
