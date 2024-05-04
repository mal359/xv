#ifndef BANNED_H
#define BANNED_H

/******************************************************************************
 *                      B A N N E D    F U N C T I O N S                      *
 *									      *
 * The following functions aren't considered memory- or thread-safe in 2024.  *
 * This is a big problem for any program which handles image manipulation.    *
 * XV is venerable; it's pretty and it does the job. But the last "official"  *
 * release is also 30 years old. There's a lot of cruft, ugly hacks, etc.     *
 * In short, some once-acceptabe functions aren't to be used.                 *
 *                                                                            *
 * References:							              *
 * The GNU C Library Reference Manual					      *
 * github.com/git/git/blob/master/banned.h               	              *
 * learn.microsoft.com/en-us/previous-versions/bb288454(v=msdn.10)	      *
 * web.cs.hacettepe.edu.tr/~abc/teaching/bbm461/slides/BufferOverflows.pdf    *
 *									      *
 *								     MAL 2024 *
 ******************************************************************************/
  
#define strcat(dest, src) 
	#error Use of strcat() is prohibited!
#define strcpy(dest, src) 
	#error Use of strcpy() is prohibited!
#define strlen(dest) 
	#error Use of strlen() is prohibited!
#define strncpy(dest, src, n) 
	#error Use of strncpy() is prohibited!
#define strtok(dest, delim) 
	#error Use of strtok() is prohibited!
#define strtok_r(dest, delim, saveptr) 
	#error Use of strtok_r() is prohibited!

#define sprintf(dest, format, ...) 
	#error Use of sprintf() is prohibited!
#define vsprintf(dest, format, ap) 
	#error Use of vsprintf() is prohibited!

#define asctime(dest) 
	#error Use of asctime() is prohibited!
#define asctime_r(dest, tm) 
	#error Use of asctime_r() is prohibited!
#define ctime(dest) 
	#error Use of ctime() is prohibited!
#define ctime_r(dest, timep) 
	#error Use of ctime_r() is prohibited!
#define gmtime(dest) 
	#error Use of gmtime() is prohibited!
#define localtime(dest) 
	#error Use of localtime() is prohibited!

#define memcpy(dest, src, size) 
	#error Use of memcpy() is prohibited!

#define siggetmask(mask) 
	#error Use of siggetmask() is prohibited!
#define sigstack(ss, oss) 
	#error Use of sigstack() is prohibited!

#define getpw(uid) 
	#error Use of getpw() is prohibited!

#define mktemp(template) 
	#error Use of mktemp() is prohibited!
#define tempnam(dir, pfx) 
	#error Use of tempnam() is prohibited!
#define tmpnam(s) 
	#error Use of tmpnam() is prohibited!
#define tmpnam_r(s) 
	#error Use of tmpnam_r() is prohibited!

#define gets(s) 
	#error Use of gets() is prohibited!
#define getwd(buf) 
	#error Use of getwd() is prohibited!

#endif /* BANNED_H */
