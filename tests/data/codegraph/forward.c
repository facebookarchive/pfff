struct buf;


struct use_buf {
  struct buf *a;
};

struct buf {
  struct use_buf *n;
};

