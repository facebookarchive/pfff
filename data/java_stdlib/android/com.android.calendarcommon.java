package com.android.calendarcommon;
class RecurrenceSetTest {
}
class RecurrenceSet {
  int FOLD_RE;
  int IGNORABLE_ICAL_WHITESPACE_RE;
  int exdates;
  int exrules;
  int rdates;
  int rrules;
  int FOLDING_SEPARATOR;
  int RULE_SEPARATOR;
  int TAG;
}
class RecurrenceProcessorTest {
  int performanceRrules;
  int METHOD_TRACE;
  int SPEW;
  int TAG;
}
class RecurrenceProcessor {
  int DAYS_IN_YEAR_PRECEDING_MONTH;
  int DAYS_PER_MONTH;
  class DaySet {
    int mMonth;
    int mYear;
    int mTime;
    int mDays;
    int mR;
  }
  int USE_BYLIST;
  int USE_ITERATOR;
  int SPEW;
  int TAG;
  int MAX_ALLOWED_ITERATIONS;
  int mDays;
  int mGenerated;
  int mStringBuilder;
  int mUntil;
  int mIterator;
}
class RRuleTest {
  int DEFAULT_END;
  int UTC;
  int PST;
  int METHOD_TRACE;
  int TAG;
}
class ICalendar {
  class ParserState {
    int index;
    int line;
  }
  class Parameter {
    int value;
    int name;
  }
  class Property {
    int mValue;
    int mParamsMap;
    int mName;
    int EXDATE;
    int EXRULE;
    int RDATE;
    int RRULE;
    int DURATION;
    int DTEND;
    int DTSTART;
  }
  class Component {
    int mPropsMap;
    int mChildren;
    int mParent;
    int mName;
    int VALARM;
    int VTIMEZONE;
    int VFREEBUSY;
    int VJOURNAL;
    int VTODO;
    int VEVENT;
    int VCALENDAR;
    int NEWLINE;
    int END;
    int BEGIN;
  }
  class FormatException {
  }
  int TAG;
}
class EventRecurrenceTest {
  int BAD_RRULES;
  int GOOD_RRULES;
  class Check {
    int values;
    int key;
  }
}
class EventRecurrence {
  class ParseWkst {
  }
  class ParseBySetPos {
  }
  class ParseByMonth {
  }
  class ParseByWeekNo {
  }
  class ParseByYearDay {
  }
  class ParseByMonthDay {
  }
  class ParseByDay {
  }
  class ParseByHour {
  }
  class ParseByMinute {
  }
  class ParseBySecond {
  }
  class ParseInterval {
  }
  class ParseCount {
  }
  class ParseUntil {
  }
  class ParseFreq {
  }
  class PartParser {
  }
  class InvalidFormatException {
  }
  int ONLY_ONE_UNTIL_COUNT;
  int VALIDATE_UNTIL;
  int ALLOW_LOWER_CASE;
  int sParseWeekdayMap;
  int sParseFreqMap;
  int PARSED_WKST;
  int PARSED_BYSETPOS;
  int PARSED_BYMONTH;
  int PARSED_BYWEEKNO;
  int PARSED_BYYEARDAY;
  int PARSED_BYMONTHDAY;
  int PARSED_BYDAY;
  int PARSED_BYHOUR;
  int PARSED_BYMINUTE;
  int PARSED_BYSECOND;
  int PARSED_INTERVAL;
  int PARSED_COUNT;
  int PARSED_UNTIL;
  int PARSED_FREQ;
  int sParsePartMap;
  int bysetposCount;
  int bysetpos;
  int bymonthCount;
  int bymonth;
  int byweeknoCount;
  int byweekno;
  int byyeardayCount;
  int byyearday;
  int bymonthdayCount;
  int bymonthday;
  int bydayCount;
  int bydayNum;
  int byday;
  int byhourCount;
  int byhour;
  int byminuteCount;
  int byminute;
  int bysecondCount;
  int bysecond;
  int wkst;
  int interval;
  int count;
  int until;
  int freq;
  int startDate;
  int SA;
  int FR;
  int TH;
  int WE;
  int TU;
  int MO;
  int SU;
  int YEARLY;
  int MONTHLY;
  int WEEKLY;
  int DAILY;
  int HOURLY;
  int MINUTELY;
  int SECONDLY;
  int TAG;
}
class DurationTest {
}
class Duration {
  int seconds;
  int minutes;
  int hours;
  int days;
  int weeks;
  int sign;
}
class DateException {
}
