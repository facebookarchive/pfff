#import "Fraction.h"
#import <stdio.h>

@implementation Fraction


-(Fraction*) initWithNumerator: (int) n denominator: (int) d {
    self = [super init];

    if ( self ) {
        [self setNumerator: n andDenominator: d];
    }

    return self;
}

@end
