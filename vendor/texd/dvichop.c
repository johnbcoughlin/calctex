/* $Id: dvichop.c,v 1.2 2001/09/22 08:45:24 jfine Exp $ */
/*  -*- linux-c -*- */
/*

dvichop: input file is broken in a sequence of smaller dvi files

Copyright (c) Jonathan Fine, 2001
mailto:jfine@activetex.org

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License; either version 2
of the License, or (at your option) any later version.

USEAGE:
    dvichop <source>

DESCRIPTION:
    The file <source> is broken in a sequence of smaller dvi files,
    using marker pages as dividers.  At the end of each output file,
    a signal can be sent.  This program is designed to work with TeX's
    output dvi stream.

SEE ALSO:
    This program is called by texd and hence itex.  Instant Preview
    also uses texd.

BUGS:
    Dummy page written if dvi file would otherwise have zero pages.
    Some programs (dvitype, xdvi) fail if the dvi file has no pages.
    No configuration file: e.g. output always goes to "../dvi/nnnnn.dvi".

TO DO:
    It runs under Linux.  Does it port to Unix?
    Use a socket instead of a pipe.
    Port to Windows and the Mac.
    Buffering would improve performance.
    Follow Don Knuth's "tex.web" more closely.

*/

/*  
  $Log: dvichop.c,v $
  Revision 1.2  2001/09/22 08:45:24  jfine
  Marker page interface changed.  Output written to input directory.
  Output written to temp file, which is then renamed.

*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>

#include "dviop.h"


#ifndef lint
static char vcid[] = "$Id: dvichop.c,v 1.2 2001/09/22 08:45:24 jfine Exp $";
#endif /* lint */

/**
 * data types
 */
typedef unsigned char byte;


/**
 * we use global variables and constants
 * (this is a simple program)
 */
FILE *in_file;                  /* input dvi file */
FILE *out_file;                 /* output dvi file */
char *in_name;                  /* NULL, or name of input file */
char out_name[256];		/* name of final output file */
char tmp_name[256];		/* name of temporary file */

#define IF_VER (99)		/* interface version (experimental) */

int page_type;                  /* type of the current page */

                                /* possible values for page_type */
enum {
        PT_NULL,                /* initial value of page_type */
        PT_INIT,                /* communicate version and other data */
        PT_DEINIT,              /* sign off, for graceful exit */
        PT_OPEN,		/* use dvi file as output stream */
        PT_CLOSE,               /* close output stream */
        PT_COPY                 /* copy this page to output (default) */
};

                                /* allow us to write case PT_MARKER */
#define PT_MARKER PT_INIT: case PT_DEINIT: case PT_OPEN: case PT_CLOSE

int count[10];                  /* page numbers of the current page */

#define magic1 count[0]         /* marker pages have special value */
#define magic2 count[1]         /* page type (marker pages only) */

#define if_ver count[2]         /* interface version (PT_FIRST only) */
#define out_num count[2]        /* output file number (PT_CLOSE only) */
#define out_pid count[3]        /* process to inform (PT_CLOSE only) */

#define max_v count[4]          /* largest height-plus-depth (PT_CLOSE only) */
#define max_h count[5]          /* largest width (PT_CLOSE only) */

unsigned int in_page_no;        /* number of completely processed pages */

int last_bop;                   /* location of previous bop in the div file */
int out_page_no;                /* number of pages written to output file */
int max_push;                   /* deepest nest of push command so far */

#define MAX_DIMEN (07777777777) /* largest possible dimension (and count) */
#define MAX_FNT (2000)          /* maximum possible number of fonts */

byte preamble[15];              /* 15 bytes, zero length comment */

struct {
        byte data[14];          /* 14 bytes of data */
        byte *name;             /* name of font */
} fnt_num[MAX_FNT];

byte fnt_use[MAX_FNT];          /* store font usage for current output dvi file */


/** 
 * some utility functions 
 */
void die(char *);               /* called on error: something bad has gone wrong */
                                /* functions that return void die() on failure */

#define do_nothing() {}         /* empty statement */


/** 
 * main(), functions called by main()
 */
void init_main(int argc, char *argv[]); /* process command line, initialise globals */
void deinit_main(void);         /* release resources */

void open_in_file(void);        /* open the input dvi stream */
void close_in_file(void);       /* close the input dvi stream */

void read_preamble(void);       /* save for use when writing dvi files */
void skip_postamble(void);      /* we don't use the postamble */
void skip_post_post(void);      /* we don't use the post-postamble */

void read_first_page(void);     /* first page in dvi file is for system use */
void read_last_page(void);      /* last page in dvi file is for system use */

void write_dvi(void);           /* open out, write to out, and close out */


int main(int argc, char *argv[])
{
        init_main(argc, argv);

        open_in_file();
        read_preamble();

        read_first_page();
	
        while (PT_OPEN == page_type)
                write_dvi();

        read_last_page();

        skip_postamble();
        skip_post_post();
        close_in_file();

        deinit_main();
        return 0;
}


/**
 * init_main(), deinit_main()
 */
void init_main(int argc, char *argv[])
{
        int i;


        if (2 != argc) {
                in_name = NULL;
                die("useage: dvichop <source>\n");
        }

        in_page_no = 0;
        in_name = argv[1];
        sprintf(tmp_name, "./%s.dvi.tmp", in_name);
        page_type = PT_NULL;

        for (i = 0; i < MAX_FNT; i++) {
                fnt_num[i].name = NULL;
        }
}

void deinit_main(void)
{
        int i;

        for (i = 0; i < MAX_FNT; i++)
                free(fnt_num[i].name);
}


/**
 * open_in_file(), close_in_file(), open_out_file(), close_out_file(),
 * rename_out_file()
 * output written to "./nnn.dvi"
*/
void open_in_file(void)
{
        in_file = fopen(in_name, "rb");
        if (NULL == in_file)
                die("error opening input file\n");
}

void close_in_file(void)
{
        if (0 != fclose(in_file))
                die("error closing input file\n");
}

void open_out_file(void)
{
        out_file = fopen(tmp_name, "wb");
        if (NULL == out_file)
                die("unable to open output file\n");
}

void close_out_file(void)
{
        if (0 != fclose(out_file))
                die("error closing output file\n");
}

void rename_out_file(void)
{
        sprintf(out_name, "./%i.dvi", out_num);
        if (0 != rename(tmp_name, out_name))
                die("error renaming output file\n");
}


/**
 * read_preamble() and write_preamble()
 * the preamble has a comment, which is discarded
 */
void read_preamble(void)
{
        byte dummy[256];

        if (15 != fread(preamble, 1, 15, in_file))
                die("error reading preamble\n");
        if (preamble[14] != fread(dummy, 1, preamble[14], in_file))
                die("error reading preamble\n");
        preamble[14] = 0;
}

void write_preamble(void)
{
        if (15 != fwrite(preamble, 1, sizeof preamble, out_file))
                die("error writing preamble\n");
}


/**
 * read_first_page(), read_last_page()
 */
void read_eop(void);
void read_bop(void);

void read_first_page(void)
{
        read_bop();

        if (PT_INIT != page_type)
                die("expecting page of type PT_INIT\n");

        if (IF_VER != if_ver)
                die("unknown interface\n");

        read_eop();
        read_bop();
}

void read_last_page(void)
{
        if (PT_DEINIT != page_type)
                die("expecting page of type PT_DEINIT\n");

        read_eop();
}


/**
 * skip_postamble(), skip_post_post()
 */
void skip_postamble(void)
{
        while (EOF != getc(in_file))
                do_nothing();
}

void skip_post_post(void)
{
        do_nothing();
}


/**
 * die()
 */
void die(char *msg)
{
        if (NULL == in_name)
                fprintf(stderr, "\ndvichop: %s", msg);
        else
                fprintf(stderr, "\ndvichop:%s:%u %s", in_name, in_page_no, msg);
        exit(EXIT_FAILURE);
}


int read_quad(void);
void write_quad(int);


int get_dviop(void)
{
        int c;

        do {
                c = getc(in_file);
                if (EOF == c)
                        die("unable to get dviop\n");
        } while (dviop_nop == c);
        return c;
}


void read_bop(void)
{
        int i;

        if (dviop_bop != get_dviop())
                die("expecting dviop_bop\n");

        for (i = 0; i < 10; i++)
                count[i] = read_quad();

        read_quad();            /* discard location of previous bop */

        if (MAX_DIMEN == magic1)
                switch (magic2) {
                case PT_MARKER:
                        page_type = magic2;
                        break;

                default:
                        die("marker page of unknown type\n");
        } else
                page_type = PT_COPY;
}


void read_eop(void)
{
        if (dviop_eop != get_dviop())
                die("expecting dviop_eop\n");
        ++in_page_no;
}


int read_quad(void)
{
        int i;
        byte a, b, c, d;

        a = getc(in_file);
        b = getc(in_file);
        c = getc(in_file);
        d = getc(in_file);

        if (a < 128) {
                i = ((a * 256 + b) * 256 + c) * 256 + d;
        } else {
                i = (((a - 256) * 256 + b) * 256 + c) * 256 + d;
        }

        return i;
}


void write_dummy_page(void)
{
        int this_bop;
        int i;

        this_bop = ftell(out_file);

        putc(dviop_bop, out_file);
        for (i = 0; i < 10; i++)
                write_quad(0);
        write_quad(last_bop);
        putc(dviop_eop, out_file);

        last_bop = this_bop;
        ++out_page_no;
}


/**
 * write_dvi(), functions called by write_dvi()
 */
void init_write_dvi(void);      /* initialise */
void deinit_write_dvi(void);    /* release resources */

void open_out_file(void);
void close_out_file(void);

void write_preamble(void);
void write_postamble(void);

void copy_page(void);           /* copy page to output */

void write_dvi(void)
{
        if (PT_OPEN != page_type)
                die("internal error: bad call of write_dvi()\n");

        init_write_dvi();

        open_out_file();
        write_preamble();

        read_eop();
        read_bop();

        while (PT_COPY == page_type) {
                copy_page();
                read_bop();
        }

        if (0 == out_page_no)
                write_dummy_page();

        write_postamble();
        close_out_file();

        if (PT_CLOSE != page_type)
                die("expecting PT_CLOSE\n");


	rename_out_file();
	if (0 != out_pid)
                        kill(out_pid, SIGUSR1);

	read_eop();
        deinit_write_dvi();
	read_bop();
}


/**
 * init_write_dvi(), deinit_write_dvi()
 */
void init_write_dvi(void)
{
        int i;

        for (i = 0; i < MAX_FNT; i++)
                fnt_use[i] = 0;

        out_page_no = 0;
        last_bop = -1;
        max_push = 0;
}

void deinit_write_dvi(void)
{
        out_name[0] = '0';
}


/**
 * copy_page()
 */

void write_fnt_dfns(void);

void write_postamble(void)
{
        int post_locn;

        post_locn = ftell(out_file);
        putc(dviop_post, out_file);

        write_quad(last_bop);

        /* numerator, denominator, and magnification */
        fwrite(&preamble[2], 1, 12, out_file);

        write_quad(max_v);      /* largest height-plus-depth */
        write_quad(max_h);      /* largest width */

        putc(max_push / 256, out_file);
        putc(max_push % 256, out_file);

        putc(out_page_no / 256, out_file);
        putc(out_page_no % 256, out_file);

        write_fnt_dfns();

        /* finish off the dvi file */
        putc(dviop_post_post, out_file);
        write_quad(post_locn);
        putc(2, out_file);      /* identifies dvi format */

        /* final file size multiple of 4, at least 4 bytes of padding */
        {
                int i = 4 - ftell(out_file) % 4;

                if (i < 4)
                        i += 4;

                while (i-- > 0)
                        putc(223, out_file);
        }
}


/* needed to write pointer to previous dviop_bop */
void write_quad(int val)
{
        byte a, b, c, d;

        if (val >= 0)
                a = val / (256 * 256 * 256);
        else {

                val = val + 64 * 256 * 256 * 256;
                val = val + 64 * 256 * 256 * 256;
                a = (val / (256 * 256 * 256)) + 128;
        }

        val %= 256 * 256 * 256;
        b = val / (256 * 256);
        val %= 256 * 256;
        c = val / 256;
        d = val % 256;

        putc(a, out_file);
        putc(b, out_file);
        putc(c, out_file);
        putc(d, out_file);
}


int dvi_peek(void);

/* take a look at what's next in the dvi stream */
int dvi_peek()
{
        int c;

        while (dviop_nop == (c = getc(in_file)))
                do_nothing();
        if (c != EOF)
                ungetc(c, in_file);
        return c;
}


/* clearly, could be more efficient */
void copy_n(size_t n)
{
        while (n--)
                putc(getc(in_file), out_file);
}

/* copy single dviop from in to out */
void copy_dviop(void);

void read_fnt_def(int);
void write_fnt_def(int);

void copy_page(void)
{
        int op;
        int i;
        int this_bop;
        int curr_push = 0;

        this_bop = ftell(out_file);

        putc(dviop_bop, out_file);
        for (i = 0; i < 10; i++)
                write_quad(count[i]);
        write_quad(last_bop);

        while (1) {
                op = dvi_peek();

                switch (op) {

                case dviop_push:
                        ++curr_push;
                        if (curr_push > max_push)
                                max_push = curr_push;
                        goto copy_the_op;

                case dviop_pop:
                        --curr_push;
                        goto copy_the_op;

                case dviop_misc:
                        goto finish_page;

                case dviop_fnt_def1:
                        getc(in_file);
                        read_fnt_def(getc(in_file));
                        break;

                case dviop_fnt_def2:
                        getc(in_file);
                        read_fnt_def(getc(in_file) * 256 + getc(in_file));
                        break;

                case dviop_fnt_num:
                        i = op - dviop_fnt_num_0;

                        if (!fnt_use[i]) {
                                write_fnt_def(i);
                                fnt_use[i] = 1;
                        }

                        goto copy_the_op;

                case dviop_fnt1:
                        op = getc(in_file);

                        i = getc(in_file);
                        if (!fnt_use[i]) {
                                write_fnt_def(i);
                                fnt_use[i] = 1;
                        }

                        putc(op, out_file);
                        putc(i, out_file);
                        break;

                case dviop_fnt2:
                        op = getc(in_file);

                        i = getc(in_file) * 256 + getc(in_file);

                        if (!fnt_use[i]) {
                                write_fnt_def(i);
                                fnt_use[i] = 1;
                        }

                        putc(op, out_file);
                        putc(i / 256, out_file);
                        putc(i % 256, out_file);
                        break;

                case EOF:
                        goto finish_page;

                default:
                      copy_the_op:
                        copy_dviop();
                        break;
                }
        }

      finish_page:
        if (op != dviop_eop)
                die("Bad page\n");

        putc(getc(in_file), out_file);  /* transfer the eop */
        last_bop = this_bop;
        ++out_page_no;
}


void copy_dviop(void)
{
        int op = getc(in_file);
        int c;
        unsigned int n = 0;     /* size of special */

        putc(op, out_file);

        switch (op) {

        case dviop_misc:

        case dviop_set_char:
        case dviop_fnt_num:
        case dviop_size1:
                break;

        case dviop_size2:
                putc(getc(in_file), out_file);
                break;

        case dviop_size3:
                copy_n(2);
                break;

        case dviop_size4:
                copy_n(3);
                break;

        case dviop_size5:
                copy_n(4);
                break;

        case dviop_size9:
                copy_n(8);
                break;

        case dviop_xxx4:
                c = getc(in_file), putc(c, out_file);
                n = c * 256 * 256 * 256;
                /* fall through */

        case dviop_xxx3:
                c = getc(in_file), putc(c, out_file);
                n += c * 256 * 256;
                /* fall through */

        case dviop_xxx2:
                c = getc(in_file), putc(c, out_file);
                n += c * 256;
                /* fall through */

        case dviop_xxx1:
                c = getc(in_file), putc(c, out_file);
                n += c;

                copy_n(n);
                break;

        default:
                die("dvichop: can't do this\n");
                break;
        }
}


void read_fnt_def(int i)
{
        size_t size;

        if (i >= MAX_FNT)
                die("font number out of bounds\n");

        if (NULL != fnt_num[i].name)
                die("font number already in use\n");

        fread(fnt_num[i].data, 1, 14, in_file);
        size = fnt_num[i].data[12] + fnt_num[i].data[13];
        fnt_num[i].name = malloc(size);
        fread(fnt_num[i].name, 1, size, in_file);
}

void write_fnt_def(int i)
{
        size_t size;

        if (i < 256) {
                putc(dviop_fnt_def1, out_file);
                putc(i, out_file);
        } else if (i < 256 * 256) {
                putc(dviop_fnt_def2, out_file);
                putc(i / 256, out_file);
                putc(i % 256, out_file);
        } else
                die("font definition out of bounds\n");

        fwrite(fnt_num[i].data, 1, 14, out_file);
        size = fnt_num[i].data[12] + fnt_num[i].data[13];
        fwrite(fnt_num[i].name, 1, size, out_file);
}


/* TeX82 writes the low numbers last! why? "tex.web" does not explain */
void write_fnt_dfns()
{
        int i;

        for (i = 0; i < MAX_FNT; i++)
                if (fnt_use[i])
                        write_fnt_def(i);
}

/* end of dvichop.c */
