#import "FractionMath.h"

@implementation Fraction (Math)
-(Fraction*) add: (Fraction*) f {
    return [[Fraction alloc] initWithNumerator: numerator * [f denominator] +
                                                denominator * [f numerator]
                             denominator: denominator * [f denominator]];
}

-(Fraction*) mul: (Fraction*) f {
    return [[Fraction alloc] initWithNumerator: numerator * [f numerator]
                             denominator: denominator * [f denominator]];

}

-(Fraction*) div: (Fraction*) f {
    return [[Fraction alloc] initWithNumerator: numerator * [f denominator]
                             denominator: denominator * [f numerator]];
}

-(Fraction*) sub: (Fraction*) f {
    return [[Fraction alloc] initWithNumerator: numerator * [f denominator] -
                                                denominator * [f numerator]
                             denominator: denominator * [f denominator]];
}
@end
