#import <stdio.h>
#import "Fraction.h"
#import "Complex.h"

int main( int argc, const char *argv[] ) {
    // create a new instance
    Fraction *frac = [[Fraction alloc] initWithNumerator: 3 denominator: 10];
    Complex *comp = [[Complex alloc] initWithReal: 5 andImaginary: 15];
    id <Printing> printable;
    id <NSCopying, Printing> copyPrintable;

    // print it
    printable = frac;
    printf( "The fraction is: " );
    [printable print];
    printf( "\n" );

    // print complex
    printable = comp;
    printf( "The complex number is: " );
    [printable print];
    printf( "\n" );

    // this compiles because Fraction comforms to both Printing and NSCopyable
    copyPrintable = frac;

    // this doesn't compile because Complex only conforms to Printing
    //copyPrintable = comp;

    // test conformance

    // true
    if ( [frac conformsToProtocol: @protocol( NSCopying )] == YES ) {
        printf( "Fraction conforms to NSCopying\n" );
    }

    // false
    if ( [comp conformsToProtocol: @protocol( NSCopying )] == YES ) {
        printf( "Complex conforms to NSCopying\n" );
    }

    // free memory
    [frac release];
    [comp release];

    return 0;
}
