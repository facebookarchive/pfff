#import <Foundation/NSObject.h>

@interface Fraction : NSObject {
    int numerator;
    int denominator;
}

-(void) setNumerator: (int) n andDenominator: (int) d;

@end
