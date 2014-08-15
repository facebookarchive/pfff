
typedef int uchar;
typedef int u32int;
typedef int ushort;

struct Fcall
{
	uchar	type;
	u32int	fid;
	ushort	tag;
	union {
		struct {
			u32int	msize;		/* Tversion, Rversion */
			char	*version;	/* Tversion, Rversion */
		};
		struct {
			ushort	oldtag;		/* Tflush */
		};
    };
};
