package gnu.javax.print;
class PrinterDialog {
  int messages;
  int atts;
  int onlyPageDialog;
  int attributes;
  int flavor;
  int selectedService;
  int defaultService;
  int services;
  int appearance_panel;
  int pagesetup_panel;
  int general_panel;
  int cancel_bt;
  int ok_bt;
  class AppearancePanel {
    int chromaticy_panel;
    int sides_panel;
    int jobAttr_panel;
    int quality_panel;
    class Color {
      int color;
      int bw;
    }
    class SidesPanel {
      int duplex;
      int calendar;
      int oneside;
    }
    class JobAttributes {
      int model;
      int priority;
      int cover;
      int username_tf;
      int jobname_tf;
      int priority_lb;
      int username;
      int jobname;
    }
    class Quality {
      int group;
      int high;
      int normal;
      int low;
    }
  }
  class PageSetupPanel {
    int margins_panel;
    int orientation_panel;
    int media_panel;
    class Margins {
      int bottom_tf;
      int top_tf;
      int right_tf;
      int left_tf;
      int bottom;
      int top;
      int right;
      int left;
    }
    class MediaTypes {
      int source;
      int size;
      int source_lb;
      int size_lb;
    }
    class Orientation {
      int rev_landscape;
      int rev_portrait;
      int landscape;
      int portrait;
    }
  }
  class GeneralPanel {
    int copies;
    int printrange_panel;
    int printserv_panel;
    class PrintServices {
      int fileRedirection_cb;
      int services_cob;
      int attributes;
      int infoValue;
      int typValue;
      int statusValue;
      int info;
      int typ;
      int status;
      int name;
    }
    class PrintRange {
      int to_tf;
      int from_tf;
      int pages_rb;
      int all_rb;
      int to;
    }
    class CopiesAndSorted {
      int copiesModel;
      int copies_lb;
      int copies;
      int sort;
    }
  }
}
class PrintUriException {
  int uri;
  int reason;
}
class PrintFlavorException {
  int flavors;
}
class PrintAttributeException {
  int values;
  int categories;
}
class CupsServer {
  int password;
  int username;
  int uri;
}
class CupsPrintServiceLookup {
  int server;
}
class CupsPrintService {
}
class CupsMediaMapping {
  int ippByCups;
}
class CupsIppOperation {
  int CUPS_MOVE_JOB;
  int CUPS_GET_PPDS;
  int CUPS_GET_DEVICES;
  int CUPS_SET_DEFAULT;
  int CUPS_REJECT_JOBS;
  int CUPS_ACCEPT_JOBS;
  int CUPS_DELETE_CLASS;
  int CUPS_ADD_MODIFY_CLASS;
  int CUPS_GET_CLASSES;
  int CUPS_DELETE_PRINTER;
  int CUPS_ADD_MODIFY_PRINTER;
  int CUPS_GET_PRINTERS;
  int CUPS_GET_DEFAULT;
}
