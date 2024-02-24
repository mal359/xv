#include "copyright.h"

#include "xv.h"

/* This code augments the xv program to permit xv to read SIR format
   files.  Since SIR files are (in effect) floating point values,
   values have to be converted to 8 bit values for display.  Since
   xv doesn't a simple parameter passing mechanism we can use to set
   the grayscale range, we use the display range stored in the file.

   written by DGL  25 May 1998 at BYU
   revised by DGL   4 Nov 2000 at BYU + version 3.0 header

   changes have to be made to the xv.h header 
    (add line: #define RFT_SIR      20)

   and to the ReadFileType routine
    else if (magicno[8] == 0 && magicno[9] >= 20) {
     rv = RFT_SIR;
    }

   and the ReadPicFile routine
       case RFT_SIR:     rv = LoadSIR   (fname, pinfo);         break;

   to use this routine

*/

#define SWAP 1                /* swap bytes (little endian: dec, pc) */
#undef SWAP                   /* no byte swapping (big endian: HP,SUN) */

#define REL_BEGIN 0           /* fseek relative to beginning of file */
#define SIR_HEADER_SIZE 512   /* SIR file header size in bytes */

int read_sir_header3(FILE *imf, int *nhead, int *ndes, int *nhtype,
		     int *idatatype, int *nsx, int *nsy, 
		     float *xdeg, float *ydeg, 
		     float *ascale, float *bscale, float *a0, float *b0, 
		     int *ixdeg_off, int *iydeg_off, int *ideg_sc,
		     int *iscale_sc, int *ia0_off, int *ib0_off, int *i0_sc,
		     int *ioff, int *iscale, int *iyear, 
		     int *isday, int *ismin, int *ieday, int *iemin, 
		     int *iregion, int *itype, int *iopt,
		     int *ispare1, int *ispare2, int *ispare3,
		     float *anodata, float *v_min, float *v_max,
		     char *sensor, char *title, char *type, char *tag,
		     char *crproc, char *crtime, int maxdes, 
		     char *descrip, int *ldes, int maxi, short *iaopt,
		     int *nia);

#ifdef SWAP
void swap(short *, int);
#endif

void print_head(FILE *omf, int nhead, int ndes, int nhtype, int idatatype,
                int nsx, int nsy, float xdeg, float ydeg, float ascale,
                float bscale, float a0, float b0, 
                int ioff, int iscale, int iyear, int isday, int ismin,
                int ieday, int iemin, int iregion, int itype, int iopt,
                int ipol, int ifreqhm, int ispare1,
                float anodata, float v_min, float v_max,
                char *sensor, char *title, char *type, char *tag,
                char *crproc, char *crtime, 
                char *descrip, int ldes, short *iaopt, int nia);

char *strclip(char *str);


static int   bmpError   PARM((char *, char *));


#define FERROR(fp) (ferror(fp) || feof(fp))

/*******************************************/
int LoadSIR(fname, pinfo)
     char    *fname;
     PICINFO *pinfo;
/*******************************************/
{
  FILE         *fp;
  int          i;
  byte         *pic8;
  char         *bname;

  float smin, smax;
  float s, soff;
  float scale, scaleoffset;

/* SIR file header information */

  float xdeg, ydeg, ascale, bscale, a0, b0;  /* SIR header location info */
  int iopt;
  float v_min, v_max, anodata;
  int nsx, nsy, ioff, iscale, iyear, isday, ismin, ieday, iemin;
  int ixdeg_off, iydeg_off, ideg_sc, iscale_sc, ia0_off, ib0_off, i0_sc;
  int iregion, itype, nhead, ndes, nhtype, idatatype, ldes, nia;
  int ipol, ifreqhm, ispare1;
  char title[101], sensor[41], crproc[101], type[139], tag[101], crtime[29];

#define MAXDES 1024
  static char descrip[MAXDES+1];
#define MAXI 128
  short iaopt[MAXI];

  int k,j;
  float amag, am;
  
  int ierr;
  short *a;

#ifdef SWAP
  union 
  {
    short i2[2];
    float f2;
  } un;
#endif

  /* returns '1' on success */


  bname = BaseName(fname);

  fp = xv_fopen(fname,"r");
  if (!fp) return (bmpError(bname, "couldn't open file"));

/* get SIR image header information */

  ierr = read_sir_header3(fp, &nhead, &ndes, &nhtype,
		    &idatatype, &nsx, &nsy, 
		    &xdeg, &ydeg, &ascale, &bscale, &a0, &b0, 
		    &ixdeg_off, &iydeg_off, &ideg_sc, &iscale_sc, &ia0_off, &ib0_off, &i0_sc,
		    &ioff, &iscale, &iyear, &isday, &ismin, &ieday, &iemin, 
		    &iregion, &itype, &iopt, &ipol, &ifreqhm, &ispare1,
		    &anodata, &v_min, &v_max,
		    sensor, title, type, tag, crproc, crtime, 
		    MAXDES, descrip, &ldes, MAXI, iaopt, &nia);  
  if (ierr < 0) return(bmpError(bname,"*** Error reading SIR header from file"));

  WaitCursor();

/* write out SIR file header information */

  fprintf(stdout,"\nSIR file header: '%s'\n",fname);
  print_head(stdout, nhead, ndes, nhtype, idatatype, nsx, nsy, 
	     xdeg, ydeg, ascale, bscale, a0, b0, 
	     ioff, iscale, iyear, isday, ismin, ieday, iemin, 
	     iregion, itype, iopt, ipol, ifreqhm, ispare1,
	     anodata, v_min, v_max,
	     sensor, title, type, tag, crproc, crtime, 
	     descrip, ldes, iaopt, nia);  

  WaitCursor();

  rewind(fp);

  /* allocate read buffer */

  a =(short *) malloc(sizeof(short)*nsx*2);
  if (a == NULL) {
     fprintf(stderr,"*** ERROR: memory allocation failure...\n");
     return(-1);
  }

  /* create pic8 array */

  pic8 = (byte *) calloc((size_t) nsx * nsy, (size_t) 1);
  if (!pic8) return(bmpError(bname, "couldn't malloc 'pic8'"));

  WaitCursor();

  /* load up the image */

  smin = v_min;
  smax = v_max;

  fprintf(stdout,"\nGreyscale conversion range:  Min: %f, Max:%f\n\n",smin,smax);

  s = 1.0 / ((float) iscale);
  soff = 32767.0/ ((float) iscale) + ((float) ioff);
  if (idatatype == 1) soff = 128.0/ ((float) iscale) + ((float) ioff);

  scale = (smax-smin);
  if (scale > 0.) scale = 255./ scale;
  scaleoffset = smin;

  /* read SIR image data, converting internal representation to bytes for XV to display */

  fseek(fp,SIR_HEADER_SIZE*nhead,REL_BEGIN);

  for (i = 0; i < nsy; i++) {
    k = (nsy - i - 1) * nsx;

    switch(idatatype) {
    case 1:     /* bytes */
      fread((char *)a, sizeof(char), nsx, fp) ;
      for (j = 0; j < nsx; j++){
	amag = ((float) a[j]) * s + soff; /* pixel value */

	/* scale floating point to byte values */
	am = scale * (amag - scaleoffset);
	if(am > 255.) am = 255.;		/* check overflow */
	if(am < 0.) am = 0.;		/* check underflow */
	*(pic8+k+j) = (char)((int)(am));    /* byte array */
      }
      break;
      
    case 4:     /* floats */
#ifdef SWAP
      fread((char *)a, sizeof(short), 2*nsx, fp) ;
      swap(a,nsx);
      for (j = 0; j < nsx; j++){
	un.i2[1]=a[j*2];
	un.i2[0]=a[j*2+1];
	amag = un.f2;

	/* scale floating point to byte values */
	am = scale * (amag - scaleoffset);
	if(am > 255.) am = 255.;		/* check overflow */
	if(am < 0.) am = 0.;		/* check underflow */
	*(pic8+k+j) = (char)((int)(am));    /* byte array */
      }
#else
      fread((char *) a, sizeof(float), nsx, fp);
      for (j = 0; j < nsx; j++){
	amag = ((float *) a)[j];

	/* scale floating point to byte values */
	am = scale * (amag - scaleoffset);
	if(am > 255.) am = 255.;		/* check overflow */
	if(am < 0.) am = 0.;		/* check underflow */
	*(pic8+k+j) = (char)((int)(am));    /* byte array */
      }

#endif
      break;
      
    default:    /* 0 or 2: read two byte integers */
      fread((char *)a, sizeof(short), nsx, fp) ;
#ifdef SWAP	
      swap(a,nsx);
#endif
      
      for (j = 0; j < nsx; j++){
	amag = ((float) a[j]) * s + soff; /* pixel value */

	/* scale floating point to byte values */
	am = scale * (amag - scaleoffset);
	if(am > 255.) am = 255.;		/* check overflow */
	if(am < 0.) am = 0.;		/* check underflow */
	*(pic8+k+j) = (char)((int)(am));    /* byte array */
      }
    }

  }


  fclose(fp);

  pinfo->w = nsx;
  pinfo->h = nsy;
  pinfo->normw = pinfo->w;
  pinfo->normh = pinfo->h;
  pinfo->frmType = F_GIF;
  pinfo->colType = F_FULLCOLOR;

  sprintf(pinfo->fullInfo, "SIR range %f to %f, %dx%d %s",
	  smin,smax,nsx,nsy,fname);
  sprintf(pinfo->shrtInfo, "SIR range %f to %f, %dx%d",
	  smin,smax,nsx,nsy);

  pinfo->comment = (char *) NULL;
  /* pinfo->comment = descrip; */


  /* load grey color map */

  for (i=0; i<255; i++) {
    pinfo->b[i] = i;
    pinfo->g[i] = i;
    pinfo->r[i] = i;
  }
  pinfo->pic  = pic8;
  pinfo->type = PIC8;

  return 1;

 ERROR:
  fclose(fp);
  return 0;
}  
  


/*******************************************/
static int bmpError(fname, st)
     char *fname, *st;
{
  SetISTR(ISTR_WARNING,"%s:  %s", fname, st);
  return 0;
}

/* standard read SIR header routine (version 3) */
	    
int read_sir_header3(FILE *imf, int *nhead, int *ndes, int *nhtype,
		    int *idatatype, int *nsx, int *nsy, 
		    float *xdeg, float *ydeg, 
		    float *ascale, float *bscale, float *a0, float *b0, 
		    int *ixdeg_off, int *iydeg_off, int *ideg_sc,
		    int *iscale_sc, int *ia0_off, int *ib0_off, int *i0_sc,
		    int *ioff, int *iscale, int *iyear, 
		    int *isday, int *ismin, int *ieday, int *iemin, 
		    int *iregion, int *itype, int *iopt,
		    int *ipol, int *ifreqhm, int *ispare1,
		    float *anodata, float *v_min, float *v_max,
		    char *sensor, char *title, char *type, char *tag,
		    char *crproc, char *crtime, int maxdes, 
		    char *descrip, int *ldes, int maxi, short *iaopt,
		    int *nia)
{
   /* v3 read header of BYU SIR file --- written by D.Long Oct 2000 */

/*  Header consists of a variable number of 512 byte blocks.  The
    block contains scaling information and strings:
    note that character strings may or may not be null terminated in file

    The first block consists of 256 short integers.  Indexing them
    from 1..256 in an array temp, they contain:


	temp(1)	= nsx			! pixels in x direction
	temp(2) = nsy			! pixels in y direction
        temp(3) <= xdeg			! span of x
	temp(4) <= ydeg			! span of y
	temp(5) = nhtype                ! header type (old<15,20,30)
	temp(6) <= ascale               ! x scaling
	temp(7) <= bscale               ! y scaling
	temp(8) <= a0                   ! x (or lon) origin
         note: longitudes should be in the range -180 to 180
	temp(9) <= b0                   ! y (or lat) origin
         note: latitudes should be in the range -90 to 90

     scaling for prorjection parameters are generally:

        temp(3) = nint((xdeg + float(ixdeg_off)) * float(ideg_sc))
        temp(4) = nint((ydeg + float(iydeg_off)) * float(ideg_sc))
        temp(6) = nint(ascale * float(iscale_sc))
        temp(7) = nint(bscale * float(iscale_sc))
        temp(8) = nint((a0 + float(ia0_off)) * float(i0_sc))
        temp(9) = nint((b0 + float(ib0_off)) * float(i0_sc))

     with the following projection specific exceptions:

	if (iopt.eq.1.or.iopt.eq.2) then		! lambert
           temp(6) = nint(float(iscale_sc)/ascale)
           temp(7) = nint(float(iscale_sc)/bscale)
	if (iopt.eq.11.or.iopt.eq.12.or.iopt.eq.13) then ! EASE grid
           temp(6) = nint(float(iscale_sc)*anint(10.*ascale*
     *                         25.067525/6371.228)*0.05)
           temp(7) = nint(float(iscale_sc)*anint(10.*bscale/
     *                         25.067525)*0.05)

	temp(10) = ioff			! offset to be added to scale val
	temp(11) = iscale		! scale factor ival=(val-ioff)/iscale
	temp(12) = iyear		! year for data used
	temp(13) = isday		! starting JD
	temp(14) = ismin		! time of day for first data (in min)
	temp(15) = ieday		! ending JD
	temp(16) = iemin		! time of day for last data (in min)
	temp(17) = iopt			! projection type
					!  -1 = no projection, image only
					!   0 = rectalinear lat/lon
					!   1 = lambert equal area
					!   2 = lambert equal area (local rad)
					!   5 = polar stereographic
					!  11 = EASE north equal area grid
					!  12 = EASE south equal area grid
					!  13 = EASE cylindrical grid
	temp(18) = iregion		! region id code
	temp(19) = itype		! image type code
                                       ! some standard values: (0=unknown)
                                       ! 1 = scatterometer A (dB)
                                       ! 2 = scatterometer B (dB/deg)
                                       ! 3 = radiometer Tb (K)
                                       ! 9 = topography (m)
	temp(20)-temp(39) 40 chars of sensor
        temp(40) = iscale_sc            ! ascale/bscale scale factor
        temp(41) = nhead                ! number of 512 byte header blocks
        temp(42) = ndes                 ! number of 512 byte blocks description
        temp(43) = ldes                 ! number of bytes of description
        temp(44) = nia                  ! number of optional integers
        temp(45) = ipol                 ! polarization (0=n/a,1=H,2=V)
        temp(46) = ifreqhm              ! frequency in 100's MHz (0 if n/a)
        temp(47) = ispare1              ! spare
        temp(48) = idatatype            ! data type code 0,2=i*2,1=i*1,4=float

       the value of idata type determines how data is stored and how
       anodata, vmin, and vmax are stored.

       if idatatype = 1 data is stored as bytes (minv=128)
       if idatatype = 2 data is stored as 2 byte integers (minv=32766)
       if idatatype = 4 data is stored as IEEE floating point

       if idatatype = 1,2 anodata,vmin,vmax are stored as 2 byte integers
         in temp(49)..temp(51)  minv, ioff and iscal used to convert
         integers or bytes into floating point values
         nodata, vmin, and vmax must be representable with ioff and iscale
            temp(*) = (value-ioff)*iscale-minv
            value = float(temp(*)+minv)/float(iscale)+ioff
       idatatype=2 is considered the SIR standard format

       if idatatype = f anodata,vmin,vmax are stored as floating points
         in temp(42)..temp(57) and minv, ioff and iscale are ignored here
         and when reading the file.
         floating point numbers are NOT standard across platforms and
         are therefore not recommended

        temp(49) <= anodata           ! value representing no data
        temp(50) <= vmin              ! minimum useful value from creator prg
        temp(51) <= vmax              ! maximum useful value from creator prg
        temp(52,53) = anodata         ! IEEE floating value of no data
        temp(54,55) = vmin            ! IEEE floating minimum useful value
        temp(56,57) = vmax            ! IEEE floating maximum useful value

        temp(58)-temp(126) 150 chars of type
        temp(127) = ixdeg_off         ! xdeg offset
        temp(128) = iydeg_off         ! ydeg offset
        temp(129)-temp(168) 80 chars of title
        temp(169) = ideg_sc           ! xdeg,ydeg scale factor
        temp(170)-temp(189) 40 chars of tag
        temp(190) = ia0_off           ! b0 offset 
        temp(191)-temp(240) 100 chars of crproc
        temp(241) = ib0_off           ! b0 offset 
        temp(242)-temp(255) 28 chars of crtime
        temp(256) = i0_sc             ! a0,b0 scale factor

     optional header blocks:

       ndes header blocks of 512 bytes: chars of description
       nhead-ndes-1 header blocks of 512 bytes: values of iaopt
        by convention, first value iaopt is a code telling how to interpret
        the rest of the array if nia>0.  Usage of additional blocks is
        user dependent and non-standard.

       remainder of file is image data in a multiple of 512 byte blocks

       one,two byte integer scaling (idatatype=1,2) is
          intval = (fvalue-ioff)*iscale-minv
          fvalue = float(intval+minv)/float(iscale)+ioff
       no scaling of float values for (idatatype=4)

*/

   short in, temp[256];
   int nch,i;
   float soff;

   union {
     short i2[2];
     float f2;
   } un;

   rewind(imf);  /* make sure to be start of file */

/* read first header block */

   if (fread(temp, sizeof(short), 256, imf) == 0) return(-1);
#ifdef SWAP
   swap(temp, 256);
#endif

/* decode nsx */

   *nsx = temp[0];
   *nsy = temp[1];
   *iopt  = temp[16];
   *nhtype = temp[4];

   if (*nhtype < 20) *nhtype=1;

   if (*nhtype < 30) {  /* set version 3.0 parameters to default version 2.0 values */

     switch (*iopt){
     case -1: /* image only */
       *ideg_sc=10;
       *iscale_sc=1000;
       *i0_sc=100;
       *ixdeg_off=0;
       *iydeg_off=0;
       *ia0_off=0;
       *ib0_off=0;
       break;
     case 0: /* rectalinear lat/lon */
       *ideg_sc=100;
       *iscale_sc=1000;
       *i0_sc=100;
       *ixdeg_off=-100;
       *iydeg_off=0;
       *ia0_off=0;
       *ib0_off=0;
       break;
     case 1: /* lambert */
     case 2:
       *ideg_sc=100;
       *iscale_sc=1000;
       *i0_sc=1;
       *ixdeg_off=0;
       *iydeg_off=0;
       *ia0_off=0;
       *ib0_off=0;
       break;
     case 5: /* polar stereographic */
       *ideg_sc=100;
       *iscale_sc=100;
       *i0_sc=1;
       *ixdeg_off=-100;
       *iydeg_off=0;
       *ia0_off=0;
       *ib0_off=0;
       break;
     case 11: /* EASE grid */
     case 12:
     case 13:
       *ideg_sc=10;
       *iscale_sc=1000;
       *i0_sc=10;
       *ixdeg_off=0;
       *iydeg_off=0;
       *ia0_off=0;
       *ib0_off=0;
       break;
     default: /* unknown */
       *ideg_sc=100;
       *iscale_sc=1000;
       *i0_sc=100;
       *ixdeg_off=0;
       *iydeg_off=0;
       *ia0_off=0;
       *ib0_off=0;
     }

   } else {  /* get projection offset and scale factors from file */
     
     *iscale_sc = temp[ 39];
     *ixdeg_off = temp[126];
     *iydeg_off = temp[127];
     *ideg_sc   = temp[168];
     *ia0_off   = temp[189];
     *ib0_off   = temp[240];
     *i0_sc     = temp[255];

   }

   /* decode default projection parameters */

   *xdeg = temp[2] / (float) *ideg_sc - *ixdeg_off;
   *ydeg = temp[3] / (float) *ideg_sc - *iydeg_off;
   *ascale = temp[5] / (float) *iscale_sc;
   *bscale = temp[6] / (float) *iscale_sc;
   *a0 = temp[7] / (float) *i0_sc - *ia0_off;
   *b0 = temp[8] / (float) *i0_sc - *ib0_off;

   /* handle special cases which depend on transformation option */

   switch (*iopt){
   case -1: /* image only */
     break;
   case 0:  /* rectalinear lat/lon */
     break;
   case 1:  /* lambert */
   case 2:
     *ascale = (float) *iscale_sc / (float) temp[5];
     *bscale = (float) *iscale_sc / (float) temp[6];
     break;
   case 5:  /* polar stereographic */
     break;
   case 11: /* EASE grid */
   case 12:
   case 13:
     *ascale = 2.0L*((double) temp[5] / (double) *iscale_sc) * 6371.228L/25.067525L;
     *bscale = 2.0L*((double) temp[6] / (double) *iscale_sc) * 25.067525L;
     break;
   default:
     fprintf(stderr,"\n *** Unrecognized SIR option in read_sir_header3 ***\n");
   }


   *ioff = temp[9];
   *iscale = temp[10];
   if (*iscale == 0) *iscale=1;

   *iyear = temp[11];
   *isday = temp[12];
   *ismin = temp[13];
   *ieday = temp[14];
   *iemin = temp[15];

   *iregion = temp[17];
   *itype = temp[18];

   *nhead = temp[40];
   if (*nhead == 0) *nhead=1;
   *ndes = temp[41];
   *ldes = temp[42];
   *nia = temp[43];
   *ipol = temp[44];
   *ifreqhm = temp[45];
   *ispare1 = temp[46];
   *idatatype = temp[47];
   if (*idatatype == 0) *idatatype = 2;

   if (*iscale == 0) *iscale=1;
   soff = 32767.0/(float) *iscale;
   if (*idatatype == 1) soff = 128.0/(float) *iscale;

   *anodata = (float) temp[48]/(float) *iscale + *ioff + soff;
   *v_min = (float) temp[49]/(float) *iscale + *ioff + soff;
   *v_max = (float) temp[50]/(float) *iscale + *ioff + soff;

   if (*idatatype == 4) {

#ifdef SWAP
     un.i2[1]=temp[51];
     un.i2[0]=temp[52];
     *anodata = un.f2;
     un.i2[1]=temp[53];
     un.i2[0]=temp[54];
     *v_min = un.f2;
     un.i2[1]=temp[55];
     un.i2[0]=temp[56];
     *v_max = un.f2;
#else
     un.i2[0]=temp[51];
     un.i2[1]=temp[52];
     *anodata = un.f2;
     un.i2[0]=temp[53];
     un.i2[1]=temp[54];
     *v_min = un.f2;
     un.i2[0]=temp[55];
     un.i2[1]=temp[56];
     *v_max = un.f2;
#endif

   }
   
   for (in = 0; in < 20; in++) {
      sensor[2*in] = temp[19+in] % 256;
      sensor[2*in+1] = temp[19+in]/256; 
   }
   sensor[40] = '\0';

   for (in = 0; in < 69; in++) {
      type[2*in] = temp[57+in] % 256;
      type[2*in+1] = temp[57+in]/256;
   }
   type[138] = '\0';

   for (in = 0; in < 40; in++) {
      title[2*in] = temp[128+in] % 256;
      title[2*in+1] = temp[128+in]/256;
   }
   title[80] = '\0';

   for (in = 0; in < 20; in++) {
      tag[2*in] = temp[169+in] % 256;
      tag[2*in+1] = temp[169+in]/256;
   }
   tag[40] = '\0';

   for (in = 0; in < 50; in++) {
      crproc[2*in] = temp[190+in] % 256;
      crproc[2*in+1] = temp[190+in]/256;
   }
   crproc[100] = '\0';

   for (in = 0; in < 14; in++) {
      crtime[2*in] = temp[241+in] % 256;
      crtime[2*in+1] = temp[241+in]/256;
   }
   crtime[28] = '\0';

   if (*nhtype == 1) {  /* a really old style header */
     *nhead=1;
     *ndes=0;
     *ldes=0;
     *nia=0;
     type[0]='\0';
     tag[0]='\0';
     crproc[0]='\0';
     crtime[0]='\0';
     *nhtype=20;   /* upgrade style on read */
   }

   if (*nhead > 1) {   /* read additional header blocks */
      if (*ndes > 0) {
	if (*ldes >= maxdes)
	  fprintf(stderr,"*** read_sir_head3 warning: file description too short (needed: %d avail: %d)\n",*ldes, maxdes);

	 fseek(imf, 512, REL_BEGIN);
	 nch=(maxdes < *ldes ? maxdes : *ldes);
	 nch=0;
	 for (i = 0; i < *ndes; i++) {
	   if (fread(temp, sizeof(short), 256, imf) == 0) return(-1);
#ifdef SWAP
	   swap(temp, 256);
#endif
	   for (in = 0; in < 256; in++) {
	     if (nch < maxdes) descrip[nch] = temp[in] % 256;
	     nch++;
	     if (nch < maxdes) descrip[nch] = temp[in]/256;
	     nch++;
	   }
	 }
      }
      if (maxdes > 0) descrip[maxdes-1] = '\0';

      if (*nhead-*ndes-1 > 0) {
	 fseek(imf, 512*(*ndes+1), REL_BEGIN);
	 if (*nia >= maxi)
	   fprintf(stderr,"*** read_sir_head3 warning: header extra ints too short (needed: %d avail: %d)\n",*nia, maxi);

	 nch=(maxi < *nia ? maxi : *nia);
	 if (fread(iaopt, sizeof(short), nch, imf) == 0) return(-1);
#ifdef SWAP
	 swap(iaopt, nch);
#endif
      }
   }

   return(0);
}


/****** print out sir file header information to output file *****/

char *strnc(char*, char*, int);

void print_head(FILE *omf, int nhead, int ndes, int nhtype, int idatatype, int nsx,
		int nsy, float xdeg, float ydeg, float ascale,
		float bscale, float a0, float b0, 
		int ioff, int iscale, int iyear, int isday, int ismin,
		int ieday, int iemin, int iregion, int itype, int iopt,
		int ipol, int ifreqhm, int ispare1,
		float anodata, float v_min, float v_max,
		char *sensor, char *title, char *type, char *tag,
		char *crproc, char *crtime, 
		char *descrip, int ldes, short *iaopt, int nia)
{
  int i;
  char stmp[150];


  if (nhtype < 20) fprintf(omf," (Old style header) %d\n",nhtype);
  fprintf(omf,"  Title:   '%s'\n",strnc(title,stmp,150));
  fprintf(omf,"  Sensor:  '%s'\n",strnc(sensor,stmp,150));
  if (nhtype > 16) {
     fprintf(omf,"  Type:    '%s'\n",strnc(type,stmp,150));
     fprintf(omf,"  Tag:     '%s'\n",strnc(tag,stmp,150));
     fprintf(omf,"  Creator: '%s'\n",strnc(crproc,stmp,150));
     fprintf(omf,"  Created: '%s'\n",strnc(crtime,stmp,150));
  }
  fprintf(omf,"  Size: %d x %d    Total:%d",nsx,nsy, nsx*nsy);
  fprintf(omf,"  Offset: %d  Scale: %d\n",ioff,iscale);
  fprintf(omf,"  Year: %d  JD range: %d-%d",iyear,isday,ieday);
  fprintf(omf,"  Region Number: %d  Type: %d  Form: %d\n",iregion,itype,iopt);
  if (nhtype > 16) {
     fprintf(omf,"  Polarization: %d  Frequency: %f MHz\n",ipol,ifreqhm*0.1);
     fprintf(omf,"  Datatype: %d  Headers: %d  Ver:%d\n",idatatype,nhead,nhtype);
     fprintf(omf,"  Nodata: %f   Vmin: %f  Vmax: %f\n",anodata,v_min,v_max);
     if (ldes > 0)
	fprintf(omf,"  Description: (%d) '%s'\n",ldes,strnc(descrip,stmp,150));
     if (nia > 0) {
	fprintf(omf,"  Extra Ints: %d\n",nia);
	for (i=0; i < nia; i++)
	   fprintf(omf,"     %d %d",i,iaopt[i]);
     }	
  } else {
    v_min=-32.0;
    v_max=0.;
  }

  switch(iopt) {
   case -1:
     fprintf(omf,"  Rectangular image-only projection: \n");
     fprintf(omf,"   Xspan,  Yspan:  %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Xscale, Yscale: %f , %f\n",ascale,bscale);
     fprintf(omf,"   Xorg,   Yorg:   %f , %f\n",a0,b0);
     break;

   case 0:
     fprintf(omf,"  Rectangular Lat/Long projection: \n");
     fprintf(omf,"   Size (deg):     %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Lon, Lat scale: %f , %f (pix/deg)\n",ascale,bscale);
     fprintf(omf,"   Offsets:        %f , %f\n",a0,b0);
     break;

   case 2:
     fprintf(omf,"  Lambert form: (local radius)\n");
     break;
   case 1:
     if (iopt==1) fprintf(omf,"  Lambert projection: (fixed radius)\n");
     fprintf(omf,"   Center point:      %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Lon, Lat scale:    %f , %f (km/pix)\n",1./ascale,1./bscale);
     fprintf(omf,"   Lower-Left Corner: %f , %f\n",a0,b0);
     break;
     break;


   case 5:
     fprintf(omf,"  Polar sterographic form: \n");
     fprintf(omf,"   Center Lon,Lat:    %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   X,Y scales:        %f , %f (km/pix)\n",ascale,bscale);
     fprintf(omf,"   Lower-Left Corner: %f , %f\n",a0,b0);
     break;

   case 11:
   case 12:
     fprintf(omf,"  EASE polar azimuthal form: \n");
     fprintf(omf,"   Map center (col,row): %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   A,B scales:           %f , %f\n",ascale,bscale);
     fprintf(omf,"   Map origin (col,row): %f , %f\n",a0,b0);
     break;

   case 13:
     fprintf(omf,"  EASE cylindrical form: \n");
     fprintf(omf,"   Map center (col,row): %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   A,B scales:           %f , %f\n",ascale,bscale);
     fprintf(omf,"   Map origin (col,row): %f , %f\n",a0,b0);
     break;

   default:
     fprintf(omf,"  Unrecognized SIR file option: \n");
     fprintf(omf,"   Xspan,  Yspan:  %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Xscale, Yscale: %f , %f\n",ascale,bscale);
     fprintf(omf,"   Xorg,   Yorg:   %f , %f\n",a0,b0);
     break;
  }

  fprintf(omf,"  Image Min, Max: %f , %f\n\n",v_min,v_max);
  fflush(omf);
  
  return;
}

void print_head3(FILE *omf, int nhead, int ndes, int nhtype, int idatatype, int nsx,
		int nsy, float xdeg, float ydeg, float ascale,
		float bscale, float a0, float b0, 
		int ixdeg_off, int iydeg_off, int ideg_sc,
		int iscale_sc, int ia0_off, int ib0_off, int i0_sc,
		int ioff, int iscale, int iyear, int isday, int ismin,
		int ieday, int iemin, int iregion, int itype, int iopt,
		int ipol, int ifreqhm, int ispare1,
		float anodata, float v_min, float v_max,
		char *sensor, char *title, char *type, char *tag,
		char *crproc, char *crtime, 
		char *descrip, int ldes, short *iaopt, int nia)
{
  int i;
  char stmp[150];


  if (nhtype < 20) fprintf(omf," (Old style header) %d\n",nhtype);
  fprintf(omf,"  Title:   '%s'\n",strnc(title,stmp,150));
  fprintf(omf,"  Sensor:  '%s'\n",strnc(sensor,stmp,150));
  if (nhtype > 16) {
     fprintf(omf,"  Type:    '%s'\n",strnc(type,stmp,150));
     fprintf(omf,"  Tag:     '%s'\n",strnc(tag,stmp,150));
     fprintf(omf,"  Creator: '%s'\n",strnc(crproc,stmp,150));
     fprintf(omf,"  Created: '%s'\n",strnc(crtime,stmp,150));
  }
  fprintf(omf,"  Size: %d x %d    Total:%d",nsx,nsy, nsx*nsy);
  fprintf(omf,"  Offset: %d  Scale: %d\n",ioff,iscale);
  fprintf(omf,"  Year: %d  JD range: %d-%d",iyear,isday,ieday);
  fprintf(omf,"  Region Number: %d  Type: %d  Form: %d\n",iregion,itype,iopt);
  if (nhtype > 16) {
     fprintf(omf,"  Polarization: %d  Frequency: %f MHz\n",ipol,ifreqhm*0.1);
     fprintf(omf,"  Datatype: %d  Headers: %d  Ver:%d\n",idatatype,nhead,nhtype);
     fprintf(omf,"  Nodata: %f   Vmin: %f  Vmax: %f\n",anodata,v_min,v_max);
     if (ldes > 0)
	fprintf(omf,"  Description: (%d) '%s'\n",ldes,strnc(descrip,stmp,150));
     if (nia > 0) {
	fprintf(omf,"  Extra Ints: %d\n",nia);
	for (i=0; i < nia; i++)
	   fprintf(omf,"     %d %d",i,iaopt[i]);
     }	
  } else {
    v_min=-32.0;
    v_max=0.;
  }

  switch(iopt) {
   case -1:
     fprintf(omf,"  Rectangular image-only projection: \n");
     fprintf(omf,"   Xspan,  Yspan:  %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Xscale, Yscale: %f , %f\n",ascale,bscale);
     fprintf(omf,"   Xorg,   Yorg:   %f , %f\n",a0,b0);
     break;

   case 0:
     fprintf(omf,"  Rectangular Lat/Long projection: \n");
     fprintf(omf,"   Size (deg):     %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Lon, Lat scale: %f , %f (pix/deg)\n",ascale,bscale);
     fprintf(omf,"   Offsets:        %f , %f\n",a0,b0);
     break;

   case 2:
     fprintf(omf,"  Lambert form: (local radius)\n");
     break;
   case 1:
     if (iopt==1) fprintf(omf,"  Lambert projection: (fixed radius)\n");
     fprintf(omf,"   Center point:      %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Lon, Lat scale:    %f , %f (km/pix)\n",1./ascale,1./bscale);
     fprintf(omf,"   Lower-Left Corner: %f , %f\n",a0,b0);
     break;
     break;


   case 5:
     fprintf(omf,"  Polar sterographic form: \n");
     fprintf(omf,"   Center Lon,Lat:    %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   X,Y scales:        %f , %f (km/pix)\n",ascale,bscale);
     fprintf(omf,"   Lower-Left Corner: %f , %f\n",a0,b0);
     break;

   case 11:
   case 12:
     fprintf(omf,"  EASE polar azimuthal form: \n");
     fprintf(omf,"   Map center (col,row): %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   A,B scales:           %f , %f\n",ascale,bscale);
     fprintf(omf,"   Map origin (col,row): %f , %f\n",a0,b0);
     break;

   case 13:
     fprintf(omf,"  EASE cylindrical form: \n");
     fprintf(omf,"   Map center (col,row): %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   A,B scales:           %f , %f\n",ascale,bscale);
     fprintf(omf,"   Map origin (col,row): %f , %f\n",a0,b0);
     break;

   default:
     fprintf(omf,"  Unrecognized SIR file option: \n");
     fprintf(omf,"   Xspan,  Yspan:  %f , %f\n",xdeg,ydeg);
     fprintf(omf,"   Xscale, Yscale: %f , %f\n",ascale,bscale);
     fprintf(omf,"   Xorg,   Yorg:   %f , %f\n",a0,b0);
     break;
  }
  fprintf(omf,"  Transform scale factors: \n");
  fprintf(omf,"   Xdeg_off, Ydeg_off, scale: %d %d %d\n",ixdeg_off, iydeg_off, ideg_sc);
  fprintf(omf,"   Xscale, Yscale:            %d\n",iscale_sc);
  fprintf(omf,"   Xorg, Yorg, scale:         %d %d %d\n",ia0_off,ib0_off, i0_sc);

  fprintf(omf,"  Image Visible Min, Max: %f , %f\n\n",v_min,v_max);
  fflush(omf);
  
  return;
}

/* routine to truncate off trailing spaces for printing */

char *strnc(char *in, char *out, int cnt)
{
  int l;
  
  strncpy(out, in, cnt);
  for (l=strlen(out); l > 0; l--)
    if (*(out+l) != ' ' && *(out+l) != '\0') {
      *(out+l+1)='\0';
      return(out);
    }
  return(out);
}


#ifdef SWAP

void swap(short *i, int n)
{
   char *s, t;
   int j;
   
   for (j = 0; j < n; j++) {
     s = (char *) (i+j);
     t = *s;
     *s = *(s+1);
     *(s+1) = t;
   }
   return;
}

#endif

char *strclip(char *str)
{
  int l;
  
  for (l=strlen(str); l > 0; l--)
    if (*(str+l) != ' ' && *(str+l) != '\0') {
      *(str+l+1)='\0';
      return(str);
    }
  return(str);
}
