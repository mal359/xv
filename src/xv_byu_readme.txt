Documentation for xv_byu

D.G. Long
BYU MERS Lab
4 Sep 1999

The xv utility permits display and modification of image files is
ported to a wide variety of platforms. The xv_byu utility is a 
slightly modified xv program which can read and disply BYU .SIR 
format files.  The current version is based on xv-3.10a.

Running xv_byu:

xv_byu is identical to xv but can load (but not save) BYU .SIR
files.  It adds an optional argument (-scale) to permit scaling
the image to a specified grayscale range.

To run with default display scale
  xv_byu sirfile.sir

To run with optional display scale
  xv_byu sirfile.sir -scale low_value high_value


This directory contains the following:

xv_byu_readme.txt    This files
xv-3.10a.tar.gz      Gzipped tar file of the xv-3.10a distribution
Makefile             Modified makefile from the xv-3.10a distribution
xv.h                 Modified include file from the xv-3.10a distribution
xv.c                 Modified main program from the xv-3.10a distribution
xvloadsir.c          New subroutine to read .sir format files
greeni.sir           Sample .sir file

Installation proceedure:

xv-10a is ported to a variety of platforms and has an automated
installation procedure.  After unzipping and untarring the distribution,
either copy the following files or modify the following files in the 
xv-10a directory, using the examples give.  Add xvloadsir.c to the 
xv-10a directory.  Modify (if needed) the #def line for SWAP to insure 
proper endian-ness in xvloadsir.c.  Run make and then rename the
executable xv to xv_byu and install in location as desired.


Changes to xv.h:  add the following line

#define RFT_SIR      20

immediately after the line

#define RFT_FITS     19


Changes to xv.c:

add the line

 float gSIRlow = 0.0, gSIRhigh = 0.0;  /* special variables for SIR files */

right after 

#define DEFCMTGEOM  "-10+300"      /* default position of comment window */


add the line

   case RFT_SIR:     rv = LoadSIR   (fname, pinfo);         break;

right after the lines

#ifdef GS_PATH
  case RFT_PS:      rv = LoadPS    (fname, pinfo, quick);    break;
#endif


add the lines

     else if (!argcmp(argv[i],"-scale",4,0,&pm))         /* grayscale range
                                                          for SIR files */
       { 
         if (++i<argc) gSIRlow=atof(argv[i]);
         if (++i<argc) gSIRhigh=atof(argv[i]);
	 fprintf(stdout,"got SIR scale arguments %f %f\n",gSIRlow,gSIRhigh);
       }

right after the line

    else if (!argcmp(argv[i],"-wloop",3,1,&waitloop));	/* waitloop */


add the lines

   else if (magicno[8] == 0 && magicno[9] >= 20) {
     rv = RFT_SIR;
   }

right after the lines

#ifdef GS_PATH
  else if (strncmp((char *) magicno, "%!",     (size_t) 2)==0 ||
	   strncmp((char *) magicno, "\004%!", (size_t) 3)==0)   rv = RFT_PS;
#endif


Changes to Makefile: add xvloadsir.o to one line
     xvxwd.o xvfits.o xvloadsir.o
 instead of
     xvxwd.o xvfits.o
