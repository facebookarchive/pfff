#import <Foundation/NSObject.h>
#import "Printing.h"

@interface Fraction: NSObject <Printing, NSCopying> {
    int numerator;
    int denominator;
}

-(Fraction*) initWithNumerator: (int) n denominator: (int) d;
-(void) setNumerator: (int) d;
-(void) setDenominator: (int) d;
-(void) setNumerator: (int) n andDenominator: (int) d;
-(int) numerator;
-(int) denominator;
@end
