@implementation ReportWriter

- (NSString *)passIndicatorString
{
  return _isPretty ? @"<green>\u2713<reset>" : @"~";
}

@end
