/* dska.c */

/* TMS 3202x assembler */

#define VERSION "0.03"

/*
 *  $Revision: 0.17 $
 *  $Date: 1994/10/12 01:02:53 $
 *  $Log: dska.c,v $
 * Revision 0.17  1994/10/12  01:02:53  lloyd
 * allow variable address register designators
 * allow $1234 hex constants
 *
 * Revision 0.16  1994/10/07  23:20:07  lloyd
 * fix banz default decrement mode
 *
 * Revision 0.15  1994/08/02  06:02:39  lloyd
 * fix up initial emit mode setting
 *
 * Revision 0.14  1994/07/16  11:09:37  lloyd
 * clean up some opcode table redundancies
 *
 * Revision 0.13  1994/06/11  11:44:13  lloyd
 * fix ldpk instruction
 *
 * Revision 0.12  1994/06/08  07:44:29  lloyd
 * fix spm
 *
 * Revision 0.11  1994/06/01  10:58:26  lloyd
 * fix blk* inst bug
 *
 * Revision 0.10  1994/05/30  12:15:07  lloyd
 * fix .long bug
 *
 * Revision 0.9  1994/05/27  21:57:37  lloyd
 * add checks for required operands
 *
 * Revision 0.8  1994/05/21  06:21:32  lloyd
 * clean up symbol processing, recognize duplicate labels
 * recognize '*' in col 1 as comment line
 *
 * Revision 0.7  1994/05/21  05:13:02  lloyd
 * some more tweaks for turbo-C compatibility
 *
 * Revision 0.6  1994/05/21  04:28:45  lloyd
 * fix up .floats and add .double
 *
 * Revision 0.5  1994/05/21  01:00:11  lloyd
 * add float, bfloat, efloat psudos
 * .double still not done
 *
 * Revision 0.4  1994/05/20  06:02:47  lloyd
 * add blkd, blkp inst defs
 *
 * Revision 0.3  1994/05/20  02:59:55  lloyd
 * fix in, out instns
 *
 * Revision 0.2  1994/05/19  09:37:11  lloyd
 * fix include file name bug
 * fix integer overflows on turbo-c
 *
 * Revision 0.1  1994/05/19  05:32:20  lloyd
 * add conditional assembly
 * add .qxx and .lqxx psudo-ops
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <math.h>

#define true (0==0)
#define false (0!=0)

#include "instab.h"

long eval0 (char **op);

#define MAXSYMS 1000
#define MAXSYMLEN 32

struct
  {
    char symname[MAXSYMLEN];
    long symval;
    int fixed;
  }
symtab[MAXSYMS];

int num_syms = 0;

int error_count = 0;

static char digits[16] = "0123456789abcdef";

int curpos = 0;
int altpos = 0;
int curtext = true;

#define MAXLINE 512
char binline[MAXLINE] = "";
char lstline[MAXLINE + 50];

FILE *infp;
FILE *binout;
FILE *lstout;

char *filename;

int lino;

int list_on = true;

#define MAX_INCL_NEST 5
FILE *includes[MAX_INCL_NEST];
int inc_lino[MAX_INCL_NEST];
char inc_filenam[MAX_INCL_NEST][MAXLINE];

int inc_level = 0;

void
err_msg (char *fmt,...)
{
  va_list ap;
  char format[256];

  sprintf (format, "%s:%d:%s\n", filename, lino, fmt);
  va_start (ap, fmt);
  vprintf (format, ap);
  va_end (ap);
  va_start (ap, fmt);
  vfprintf (lstout, format, ap);
  va_end (ap);
  error_count++;
}

int
getline (char line[MAXLINE])
{
  while (fgets (line, MAXLINE, infp) == NULL)
    {
      if (ferror (infp))
	{
	  perror ("reading input file");
	  err_msg ("getline: error reading input");
	  return 1;
	}
      if (inc_level > 0)
	{
	  /* resume file following include */
	  fclose (infp);
	  inc_level--;
	  infp = includes[inc_level];
	  lino = inc_lino[inc_level];
	  filename = inc_filenam[inc_level];
	}
      else
	{
	  err_msg ("end of input (.end missing?)");
	  return 1;
	}
    }
  /* trim trailing ctrl chars */
  while (strlen (line) && line[strlen (line) - 1] <= ' ')
    line[strlen (line) - 1] = 0;
  lino++;
  return 0;
}

void
do_include (char *arg)
{
  char *fn;
  FILE *fp;

  if (inc_level == MAX_INCL_NEST)
    {
      err_msg ("include files nested too deep");
      return;
    }

  while (isspace (*arg))
    arg++;
  if (*arg == '"')
    {
      fn = ++arg;
      while (*arg && *arg != '"')
	arg++;
    }
  else
    {
      fn = arg;
      while (*arg && (!isspace (*arg)) && *arg != ';')
	arg++;
    }
  *arg = 0;
  fp = fopen (fn, "r");
  if (fp == NULL)
    {
      perror ("can't open file");
      err_msg ("can't include %s.", fn);
    }
  else
    {
      includes[inc_level] = infp;
      inc_lino[inc_level] = lino;
      inc_level++;
      strcpy (inc_filenam[inc_level], fn);
      filename = inc_filenam[inc_level];
      infp = fp;
      lino = 0;
    }
}

void
usage (void)
{
  fprintf (stderr, "usage: dska source.asm binout.dsk print.lst\n");
  exit (1);
}

char *
decode_field (char *lp, char *field)
{
  int i = 0;

  while (isspace (*lp))
    lp++;
  while (*lp && !isspace (*lp) && ++i < MAXSYMLEN)
    {
      if (*lp == ';')
	*lp = 0;
      else if (*lp == ':')	/* some people think they go on labels */
	lp++;			/* ignore em */
      else
	{
	  if (!isalnum (*lp) && *lp != '_' && *lp != '$' && *lp != '.')
	    err_msg ("unexpected char in field `%c'", *lp);
	  *field++ = *lp++;
	}
    }
  *field = 0;
  while (*lp && !isspace (*lp))
    {
      if (*lp == ';')
	*lp = 0;
      else
	lp++;
    }
  while (isspace (*lp))
    lp++;
  return lp;
}

void
decode_line (char *line, char *label, char *operation, char *operands)
{
  char *lp = line;

  if (*lp == '*' || *lp == ';')
    {
      *label = 0;
      *operation = 0;
      *operands = 0;
      return;
    }
  else if (!isspace (*lp))	/* labels start in col 1 */
    {
      lp = decode_field (lp, label);
      if (strlen (label) && label[strlen (label) - 1] == ':')
	label[strlen (label) - 1] = 0;
    }
  else
    *label = 0;
  lp = decode_field (lp, operation);
  strcpy (operands, lp);
}

int
lookup (char *operation)
{
  /* loop up instruction or psudo-op */
  int i = 0;
  char *cp = operation;
  while (*cp)
    {
      *cp = tolower (*cp);
      cp++;
    }
  while (instab[i].mnem != NULL)
    {
      if (strcmp (operation, instab[i].mnem) == 0)
	return i;
      i++;
    }
  err_msg ("unknown operation");
  return -1;
}

void
do_if_skip (int list_on)
{
  /* skip over conditionally not assembled lines */
  int level = 0;
  char line[MAXLINE];
  char label[MAXSYMLEN];
  char operator[MAXSYMLEN];
  char operands[MAXLINE];
  do
    {
      if (list_on)
	fputs (lstline, lstout);
      if (getline (line))
	{
	  err_msg (".endif missing");
	  break;
	}
      sprintf (lstline, "%4d  %4.4s  %4.4s %4.4s\t%s\n",
	       lino, "", "", "", line);
      decode_line (line, label, operator, operands);
      switch (instab[lookup (operator)].fmt)
	{
	case psu_if:
	  level++;
	  break;
	case psu_else:
	  if (level == 0)
	    level--;
	  break;
	case psu_endif:
	  level--;
	  break;
	default:
	  break;
	}
    }
  while (level >= 0);
}

void
new_sym (char *name, long val, int fixed)
{
  /* add a symbol value to symbol table */
  int i;
  for (i = 0; i < num_syms; i++)
    {
      if (strcmp (name, symtab[i].symname) == 0)
	{
	  if ((fixed || symtab[i].fixed) && val != symtab[i].symval)
	    err_msg ("redefine fixed symbol %s, was 0%lxh to 0%lxh",
		     name, symtab[i].symval, val);
	  symtab[i].symval = val;
	  if (fixed)
	    symtab[i].fixed = true;
	  return;
	}
    }
  if (num_syms == MAXSYMS)
    err_msg ("symbol table overflow");	/* this message only shows once */
  else if (num_syms < MAXSYMS)
    {
      strcpy (symtab[num_syms].symname, name);
      symtab[num_syms].symval = val;
      symtab[num_syms].fixed = fixed;
    }
  num_syms++;
}

void
do_label (char *name, long val)
{
  if (*name)
    new_sym (name, val, true);
}

long
factor (char **op)
{
  char *cp = *op;
  long val;

  while (isspace (*cp))
    cp++;
  if (*cp == 0)
    return 0;			/* null expression is zero! */
  if (*cp == '(')
    {
      cp++;
      val = eval0 (&cp);
      if (*cp != ')')
	err_msg ("expression expecting `)'");
      else
	cp++;
    }
  else if (isdigit (*cp) || *cp == '-' || *cp == '+' ||
	   (*cp == '$' && isdigit (cp[1])))
    {
      char str[MAXSYMLEN];
      char *sp = str;
      char type;
      if (*cp == '-' || *cp == '+' || *cp == '$')
	*sp++ = *cp++;
      while (isalnum (*cp))
	{
	  char ch = *cp++;
	  if (sp >= str + MAXSYMLEN)
	    {
	      err_msg ("number too big");
	      return 0;
	    }
	  *sp++ = tolower (ch);
	}
      *sp = 0;
      type = str[strlen (str) - 1];
      if (str[0] == '$')
	val = strtol (str + 1, NULL, 16);
      else if (type == 'h')
	val = strtol (str, NULL, 16);
      else if (type == 'b' && str[1] != 'x')
	val = strtol (str, NULL, 2);
      else
	val = strtol (str, NULL, 0);
    }
  else if (*cp == '"')
    {
      cp++;
      val = *cp++;
      if (*cp != '"')
	err_msg ("character const quote expected");
      else
	cp++;
    }
  else if (isalpha (*cp) || *cp == '$' || *cp == '_' || *cp == '.')
    {
      char sym[MAXSYMLEN];
      char *sp = sym;
      int indx;
      while (isalnum (*cp) || *cp == '$' || *cp == '_' || *cp == '.')
	{
	  if (sp == sym + MAXSYMLEN)
	    err_msg ("symbol too long");
	  else if (sp < sym + MAXSYMLEN)
	    *sp = *cp;
	  sp++;
	  cp++;
	}
      *sp = 0;
      for (indx = 0; indx < num_syms; indx++)
	{
	  if (strcmp (sym, symtab[indx].symname) == 0)
	    {
	      val = symtab[indx].symval;
	      break;
	    }
	}
      if (indx == num_syms)
	{
	  err_msg ("symbol not found %s.", sym);
	  val = 0;
	}
    }
  else
    {
      err_msg ("expression unrecognizable %s at %s.", *op, cp);
      val = 0;
    }
  while (isspace (*cp))
    cp++;
  *op = cp;
  return val;
}

long
term (char **op)
{
  char *cp = *op;
  long fac = factor (&cp);
  while (*cp == '*' || *cp == '/')
    {
      char op = *cp++;
      long f = factor (&cp);
      if (op == '*')
	fac *= f;
      else
	{
	  if (f == 0)
	    err_msg ("divide by zero");
	  else
	    fac /= f;
	}
    }
  *op = cp;
  return fac;
}

long
eval0 (char **opp)
{
  char *op = *opp;
  long val = term (&op);
  while (*op == '+' || *op == '-')
    {
      char opr = *op++;
      long v = term (&op);
      if (opr == '+')
	val += v;
      else
	val -= v;
    }
  *opp = op;
  return val;
}

long
eval (char *op)
{
  long val = eval0 (&op);
  if (*op)
    err_msg ("extra stuff not handled");
  return val;
}

int
eval1 (char *op)
{
  long val = eval (op);
  /* old turbo-c bug here */
  if (val >= (long) 0x00010000L || val < (long) 0xffff8000L)
    err_msg ("integer overflow %s=%ld", op, val);
  return (int) val;
}

double
evalf (char *op)
{
  return atof (op);
}

char *
get_operand (char *opl, char *op)
{
  while (isspace (*opl))
    opl++;
  while (*opl && *opl != ';' && *opl != ',')
    *op++ = *opl++;
  *op = 0;
  if (*opl == ',')
    opl++;
  if (*opl == ';')
    *opl = 0;
  return opl;
}

char *
get_required_operand (char *opl, char *op)
{
  char *cp = get_operand (opl, op);
  if (*op == 0)
    err_msg ("missing operand");
  return cp;
}

void
pass_1 (void)
{
  char label[MAXSYMLEN];
  char operator[MAXSYMLEN];
  char operands[MAXLINE];
  char line[MAXLINE];
  char operand[MAXLINE];
  char *op;
  int done = false;
  int pos = 0;
  int altpos = 0;
  int text_mode = true;
  int op_indx;
  int ent = 0;

  fprintf (binout, "K_DSKA_" VERSION "_%s\n", filename);

  lino = 0;
  do
    {
      if (getline (line))
	break;
      new_sym ("$", pos, false);
      decode_line (line, label, operator, operands);
      op_indx = lookup (operator);
      switch (instab[op_indx].fmt)
	{
	case op_nul:
	case psu_listoff:
	case psu_liston:
	  do_label (label, pos);
	  break;
	case psu_entry:
	  get_operand (operands, operand);
	  if (*operand)
	    ent = eval1 (operand);
	  else
	    ent = pos;
	  break;
	case fmt0:
	case fmtMD:
	case fmtK8:
	case fmtD:
	case fmtK9:
	case fmtK13:
	case fmtXMD:
	case fmtSMD:
	case fmtRMD:
	case fmtR:
	case fmtFO:
	case fmtRK8:
	case fmtCM:
	  do_label (label, pos);
	  pos++;
	  break;
	case fmtSW:
	case fmtDW:
	case fmtDWi:
	case fmtRW:
	case fmtMDD:
	  do_label (label, pos);
	  pos += 2;
	  break;
	case psu_data:
	  do_label (label, pos);
	  if (text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = false;
	    }
	  break;
	case psu_text:
	  do_label (label, pos);
	  if (!text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = true;
	    }
	  break;
	case psu_ds:
	  do_label (label, pos);
	  if (text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = false;
	    }
	  get_operand (operands, operand);
	  if (operand[0])
	    pos = eval1 (operand);
	  break;
	case psu_ps:
	  do_label (label, pos);
	  if (!text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = true;
	    }
	  get_operand (operands, operand);
	  if (operand[0])
	    pos = eval1 (operand);
	  break;
	case psu_space:
	  do_label (label, pos);
	  get_required_operand (operands, operand);
	  pos += (int) ((eval (operand) + 15) / 16);
	  break;
	case psu_set:
	  if (*label == 0)
	    err_msg (".set operation must have a label");
	  else
	    {
	      get_required_operand (operands, operand);
	      new_sym (label, eval (operand), false);
	    }
	  break;
	case psu_include:
	  do_include (operands);
	  break;
	case psu_byte:
	  do_label (label, pos);
	  op = operands;
	  while (true)
	    {
	      if (*op == '"')
		{
		  op++;
		  while (*op && *op != '"')
		    {
		      op++;
		      pos++;
		    }
		  do
		    {
		      op++;
		    }
		  while (isspace (*op));
		  if (*op != ',')
		    break;
		  op++;
		}
	      else
		{
		  op = get_operand (op, operand);
		  if (operand[0] == 0)
		    break;
		  pos++;
		}
	    }
	  break;
	case psu_string:
	  {
	    int odd;
	    do_label (label, pos);
	    odd = false;
	    op = operands;
	    while (true)
	      {
		if (*op == '"')
		  {
		    op++;
		    while (*op && *op != '"')
		      {
			op++;
			if (odd)
			  pos++;
			odd = !odd;
		      }
		    do
		      {
			op++;
		      }
		    while (isspace (*op));
		    if (*op != ',')
		      break;
		    op++;
		  }
		else
		  {
		    op = get_operand (op, operand);
		    if (operand[0] == 0)
		      break;
		    if (odd)
		      pos++;
		    odd = !odd;
		  }
	      }
	    if (odd)
	      pos++;
	  }
	  break;
	case psu_word:
	case psu_qxx:
	  do_label (label, pos);
	  op = operands;
	  while (true)
	    {
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      pos++;
	    }
	  break;
	case psu_long:
	case psu_lqxx:
	case psu_float:
	case psu_efloat:
	  do_label (label, pos);
	  op = operands;
	  while (true)
	    {
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      pos += 2;
	    }
	  break;
	case psu_bfloat:
	  do_label (label, pos);
	  op = operands;
	  while (true)
	    {
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      pos += 3;
	    }
	  break;
	case psu_double:
	  do_label (label, pos);
	  op = operands;
	  while (true)
	    {
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      pos += 4;
	    }
	  break;
	case psu_if:
	  get_required_operand (operands, operand);
	  if (!eval (operand))
	    do_if_skip (false);
	  break;
	case psu_else:
	  do_if_skip (false);
	  break;
	case psu_endif:
	  break;
	case psu_end:
	  do_label (label, pos);
	  done = true;
	  break;
	default:
	  err_msg ("not recognized or not implemented operation %s",
		   operator);
	  break;
	}
    }
  while (!done);
  /* emit entry point */
  fprintf (binout, "1%04X7%04XF\n", ent, ent);
}

void
dumpline (void)
{
  /* flush emited code to binary output file and calculate checksum */
  char *cp = binline + 5;
  int cs = 0;
  char *tcp;

  if (binline[0] == 0)
    return;
  while (*cp)
    {
      int n = 0;
      int j;
      cp++;
      for (j = 0; j < 4; j++)
	{
	  char ch = tolower (*cp);
	  tcp = strchr (digits, ch);
	  n = (n << 4) + (int) (tcp - digits);
	  cp++;
	}
      cs += n;
    }
  sprintf (cp, "7%04XF", cs & 0xffff);
  fputs (binline, binout);
  fputc ('\n', binout);
  binline[0] = 0;
}

void
emit (int pos, int val, int textmode)
{
  char *cp;

  if (textmode != curtext)
    {
      int temp = curpos;
      dumpline ();
      curtext = textmode;
      curpos = altpos;
      altpos = temp;
    }
  if (curpos + 1 != pos)
    dumpline ();

  if (binline[0] == 0)
    sprintf (binline, "9%04X", pos);
  cp = binline + strlen (binline);
  /* cut back to 16 bits in case of 32 bit sign extensions */
  val &= 0xffff;
  sprintf (cp, "%c%04X", textmode ? 'B' : 'M', val);
  if (strlen (binline) > 40)
    dumpline ();
  curpos = pos;
  if (lstline[12] == ' ')
    cp = lstline + 12;
  else
    cp = lstline + 17;
  if (*cp == ' ')
    {
      int i;
      for (i = 0; i < 4; i++)
	{
	  *cp++ = digits[(val >> 12) & 0xf];
	  val <<= 4;
	}
    }
}

int
decode_ind (char *op)
{
  /* decode the indirect operand specifier fields */
  int len;

  while (isspace (*op))
    op++;
  len = strlen (op);
  while (len != 0 && isspace (op[--len]))
    op[len] = 0;
  if (*op == 0)
    return 0;
  if (strcmp (op, "*") == 0)
    return 0;
  if (strcmp (op, "*-") == 0)
    return 0x10;
  if (strcmp (op, "*+") == 0)
    return 0x20;
  if (strcmp (op, "*0-") == 0)
    return 0x50;
  if (strcmp (op, "*0+") == 0)
    return 0x60;
  if (strcmp (op, "*BR0-") == 0)
    return 0x40;
  if (strcmp (op, "*BR0+") == 0)
    return 0x70;
  if (strcmp (op, "*br0-") == 0)
    return 0x40;
  if (strcmp (op, "*br0+") == 0)
    return 0x70;
  err_msg ("unrecognizable indirect field %s", op);
  return 0;
}

void
init_ar ()
{
  new_sym ("ar0", 0, true);
  new_sym ("ar1", 1, true);
  new_sym ("ar2", 2, true);
  new_sym ("ar3", 3, true);
  new_sym ("ar4", 4, true);
  new_sym ("ar5", 5, true);
  new_sym ("ar6", 6, true);
  new_sym ("ar7", 7, true);
  new_sym ("AR0", 0, true);
  new_sym ("AR1", 1, true);
  new_sym ("AR2", 2, true);
  new_sym ("AR3", 3, true);
  new_sym ("AR4", 4, true);
  new_sym ("AR5", 5, true);
  new_sym ("AR6", 6, true);
  new_sym ("AR7", 7, true);
}

int
decode_ar1 (char *op)
{
  long val = eval (op);
  if (val < 0 || val > 7)
    err_msg ("address register range error in `%s', value %ld should be 0..7",
	     op, val);
  return (int) val & 7;
}

int
decode_ar2 (char *op)
{
  int val;
  int len;

  while (isspace (*op))
    op++;
  len = strlen (op);
  while (len && isspace (op[--len]))
    op[len] = 0;

  if (*op == 0)
    return 0;

  val = decode_ar1 (op);

  if (val < 0)
    return 0;
  else
    return val + 8;
}

void
pass_2 (void)
{
  char label[MAXSYMLEN];
  char operator[MAXSYMLEN];
  char operands[MAXLINE];
  char ind[MAXSYMLEN];
  char nextAR[MAXSYMLEN];
  char line[MAXLINE];
  char operand[MAXLINE];
  char *op;
  int done = false;
  int pos = 0;
  int altpos = 0;
  int text_mode = true;
  int op_indx;
  int op_code;

  lino = 0;
  do
    {
      if (getline (line))
	break;
      sprintf (lstline, "%4d  %04x  %4.4s %4.4s\t%s\n",
	       lino, pos, "", "", line);
      new_sym ("$", pos, false);
      decode_line (line, label, operator, operands);
      op_indx = lookup (operator);
      op_code = instab[op_indx].ins;
      switch (instab[op_indx].fmt)
	{
	default:
	  err_msg ("not recognized or not implemented operation");
	  break;
	case fmt0:
	  do_label (label, pos);
	  emit (pos, op_code, text_mode);
	  pos++;
	  break;
	case fmtK8:
	  get_required_operand (operands, operand);
	  emit (pos, op_code + (eval1 (operand) & 0xff), text_mode);
	  pos++;
	  break;
	case fmtK9:
	  get_required_operand (operands, operand);
	  emit (pos, op_code + ((eval1 (operand) >> 7) & 0x1ff), text_mode);
	  pos++;
	  break;
	case fmtK13:
	  get_required_operand (operands, operand);
	  emit (pos, op_code + (eval1 (operand) & 0x1fff), text_mode);
	  pos++;
	  break;
	case fmtCM:
	  get_required_operand (operands, operand);
	  emit (pos, op_code + (eval1 (operand) & 3), text_mode);
	  pos++;
	  break;
	case fmtFO:
	  get_required_operand (operands, operand);
	  emit (pos, op_code + (eval1 (operand) & 1), text_mode);
	  pos++;
	  break;
	case fmtD:
	  get_operand (operands, ind);
	  emit (pos, op_code + decode_ind (ind), text_mode);
	  pos++;
	  break;
	case fmtSW:
	  op = get_required_operand (operands, operand);
	  op = get_operand (op, ind);
	  emit (pos, op_code + ((eval1 (ind) & 15) << 8), text_mode);
	  emit (pos + 1, eval1 (operand), text_mode);
	  pos += 2;
	  break;
	case fmtDW:
	  op = get_required_operand (operands, operand);
	  op = get_operand (op, ind);
	  get_operand (op, nextAR);
	  emit (pos,
		op_code + decode_ind (ind) + decode_ar2 (nextAR),
		text_mode);
	  emit (pos + 1, eval1 (operand), text_mode);
	  pos += 2;
	  break;
	case fmtDWi:
	  op = get_required_operand (operands, operand);
	  op = get_operand (op, ind);
	  if (*ind == 0)
	    strcpy (ind, "*-");
	  get_operand (op, nextAR);
	  emit (pos,
		op_code + decode_ind (ind) + decode_ar2 (nextAR),
		text_mode);
	  emit (pos + 1, eval1 (operand), text_mode);
	  pos += 2;
	  break;
	case fmtMDD:
	  op = get_required_operand (operands, operand);
	  op = get_required_operand (op, ind);
	  get_operand (op, nextAR);
	  if (ind[0] == '*')
	    emit (pos,
		  op_code + decode_ind (ind) + decode_ar2 (nextAR) + 0x80,
		  text_mode);
	  else
	    emit (pos, op_code + (eval1 (ind) & 0x7f), text_mode);
	  emit (pos + 1, eval1 (operand), text_mode);
	  pos += 2;
	  break;
	case fmtMD:
	  op = get_required_operand (operands, operand);
	  get_operand (op, nextAR);
	  if (operand[0] == '*')
	    emit (pos,
		  op_code + 0x80 +
		  decode_ind (operand) + decode_ar2 (nextAR),
		  text_mode);
	  else
	    emit (pos, op_code + (eval1 (operand) & 0x7f), text_mode);
	  pos++;
	  break;
	case fmtXMD:
	  op = get_required_operand (operands, ind);
	  op = get_operand (op, operand);	/* shift */
	  get_operand (op, nextAR);
	  op_code += ((eval1 (operand) & 7) << 8);
	  if (ind[0] == '*')
	    emit (pos,
		  op_code + decode_ind (ind) + decode_ar2 (nextAR) + 0x80,
		  text_mode);
	  else
	    emit (pos, op_code + (eval1 (ind) & 0x7f), text_mode);
	  pos++;
	  break;
	case fmtSMD:
	  op = get_required_operand (operands, ind);
	  op = get_operand (op, operand);	/* shift, bit or port number */
	  get_operand (op, nextAR);
	  op_code += ((eval1 (operand) & 15) << 8);
	  if (ind[0] == '*')
	    emit (pos,
		  op_code + decode_ind (ind) + decode_ar2 (nextAR) + 0x80,
		  text_mode);
	  else
	    emit (pos, op_code + (eval1 (ind) & 0x7f), text_mode);
	  pos++;
	  break;
	case fmtR:
	  get_required_operand (operands, operand);
	  emit (pos, op_code + decode_ar1 (operand), text_mode);
	  pos++;
	  break;
	case fmtRMD:
	  op = get_required_operand (operands, operand);	/* other AR */
	  op = get_required_operand (op, ind);
	  get_operand (op, nextAR);
	  op_code += (decode_ar1 (operand) << 8);
	  if (ind[0] == '*')
	    emit (pos,
		  op_code + decode_ind (ind) + decode_ar2 (nextAR) + 0x80,
		  text_mode);
	  else
	    emit (pos, op_code + (eval1 (ind) & 0x7f), text_mode);
	  pos++;
	  break;
	case fmtRK8:
	  op = get_required_operand (operands, operand);
	  op_code += (decode_ar1 (operand) << 8);
	  op = get_required_operand (op, operand);
	  emit (pos, op_code + (eval1 (operand) & 0xff), text_mode);
	  pos++;
	  break;
	case fmtRW:
	  op = get_required_operand (operands, operand);
	  emit (pos, op_code + (decode_ar1 (operand) << 8), text_mode);
	  op = get_required_operand (op, operand);
	  emit (pos + 1, eval1 (operand), text_mode);
	  pos += 2;
	  break;
	case psu_byte:
	  do_label (label, pos);
	  op = operands;
	  while (true)
	    {
	      if (*op == '"')
		{
		  op++;
		  while (*op && *op != '"')
		    {
		      emit (pos, *op, text_mode);
		      op++;
		      pos++;
		    }
		  do
		    {
		      op++;
		    }
		  while (isspace (*op));
		  if (*op != ',')
		    break;
		  op++;
		}
	      else
		{
		  op = get_operand (op, operand);
		  if (operand[0] == 0)
		    break;
		  emit (pos, eval1 (operand), text_mode);
		  pos++;
		}
	    }
	  break;
	case psu_string:
	  do_label (label, pos);
	  {
	    int val = 0;
	    int odd = false;
	    op = operands;
	    while (true)
	      {
		if (*op == '"')
		  {
		    op++;
		    while (*op && *op != '"')
		      {
			if (odd)
			  {
			    emit (pos, val + *op, text_mode);
			    pos++;
			  }
			else
			  val = *op << 8;
			odd = !odd;
			op++;
		      }
		    do
		      {
			op++;
		      }
		    while (isspace (*op));
		    if (*op != ',')
		      break;
		    op++;
		  }
		else
		  {
		    op = get_operand (op, operand);
		    if (operand[0] == 0)
		      break;
		    if (odd)
		      {
			emit (pos, val + (eval1 (operand) & 0xff), text_mode);
			pos++;
		      }
		    else
		      val = eval1 (operand) << 8;
		    odd = !odd;
		  }
	      }
	    if (odd)
	      {
		emit (pos, val, text_mode);
		pos++;
	      }
	  }
	  break;
	case psu_word:
	  op = operands;
	  while (true)
	    {
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      emit (pos, eval1 (operand), text_mode);
	      pos++;
	    }
	  break;
	case psu_long:
	  op = operands;
	  while (true)
	    {
	      long val;
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      val = eval (operand);
	      emit (pos, (int) (val >> 16), text_mode);
	      emit (pos + 1, (int) (val & 0xffff), text_mode);
	      pos += 2;
	    }
	  break;
	case psu_efloat:
	  op = operands;
	  while (true)
	    {
	      double val;
	      int exp;
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      val = evalf (operand);
	      val = frexp (val, &exp);
	      if (val != 0.0 || exp != 0)
		exp--;
	      emit (pos, (int) ldexp (val, 15), text_mode);
	      emit (pos + 1, exp, text_mode);
	      pos += 2;
	    }
	  break;
	case psu_float:
	  op = operands;
	  while (true)
	    {
	      double val;
	      int exp;
	      long mant;
	      int sign;
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      val = evalf (operand);
	      val = frexp (val, &exp);
	      if (val != 0.0)
		exp = ((exp + 0x07e) & 0xff) << 7;
	      sign = 0;
	      if (val < 0.0)
		{
		  sign = 0x8000;
		  val = -val;
		}
	      mant = (long) ldexp (val, 25);
	      mant = (mant >> 1) + (mant & 1);	/* round off */
	      mant &= 0x7fffffL;
	      emit (pos, (int) mant, text_mode);
	      emit (pos + 1, (int) (sign + exp + (mant >> 16)), text_mode);
	      pos += 2;
	    }
	  break;
	case psu_double:
	  op = operands;
	  while (true)
	    {
	      double val;
	      int exp;
	      unsigned long mant, mant1;
	      int sign;
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      val = evalf (operand);
	      val = frexp (val, &exp);
	      if (val != 0.0)
		exp = ((exp + 0x03fe) & 0x7ff) << 4;
	      sign = 0;
	      if (val < 0.0)
		{
		  sign = 0x8000;
		  val = -val;
		}
	      mant1 = (unsigned long) ldexp (val, 21);
	      val -= ldexp ((double) mant1, -21);
	      mant1 &= 0x000fffffL;
	      mant = (unsigned long) ldexp (val, 53);
	      emit (pos, (int) mant, text_mode);
	      emit (pos + 1, (int) (mant >> 16), text_mode);
	      emit (pos + 2, (int) mant1, text_mode);
	      emit (pos + 3, (int) (sign + exp + (mant1 >> 16)), text_mode);
	      pos += 4;
	    }
	  break;
	case psu_bfloat:
	  op = operands;
	  while (true)
	    {
	      double val;
	      int exp;
	      long mant;
	      op = get_operand (op, operand);
	      if (operand[0] == 0)
		break;
	      val = evalf (operand);
	      val = frexp (val, &exp);
	      if (val != 0.0 || exp != 0)
		exp--;
	      mant = (long) ldexp (val, 31);
	      emit (pos, (int) mant, text_mode);
	      emit (pos + 1, (int) (mant >> 16), text_mode);
	      emit (pos + 2, exp, text_mode);
	      pos += 3;
	    }
	  break;
	case psu_qxx:
	  op = operands;
	  {
	    while (true)
	      {
		op = get_operand (op, operand);
		if (operand[0] == 0)
		  break;
		emit (pos, (int) ldexp (evalf (operand), op_code), text_mode);
		pos++;
	      }
	  }
	  break;
	case psu_lqxx:
	  op = operands;
	  {
	    while (true)
	      {
		long val;
		op = get_operand (op, operand);
		if (operand[0] == 0)
		  break;
		val = (long) ldexp (evalf (operand), op_code);
		emit (pos, (int) (val >> 16), text_mode);
		emit (pos, (int) (val & 0xffff), text_mode);
		pos += 2;
	      }
	  }
	  break;
	case psu_data:
	  do_label (label, pos);
	  if (text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = false;
	    }
	  break;
	case psu_text:
	  do_label (label, pos);
	  if (!text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = true;
	    }
	  break;
	case psu_ds:
	  do_label (label, pos);
	  if (text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = false;
	    }
	  get_operand (operands, operand);
	  if (operand[0])
	    pos = eval1 (operand);
	  break;
	case psu_ps:
	  do_label (label, pos);
	  if (!text_mode)
	    {
	      int t = pos;
	      pos = altpos;
	      altpos = t;
	      text_mode = true;
	    }
	  get_operand (operands, operand);
	  if (operand[0])
	    pos = eval1 (operand);
	  break;
	case psu_space:
	  get_required_operand (operands, operand);
	  pos += (int) ((eval (operand) + 15) / 16);
	  break;
	case psu_set:
	  if (*label == 0)
	    err_msg (".set operation must have a label");
	  else
	    {
	      long val;
	      int i;
	      char *cp = lstline + 20;
	      get_required_operand (operands, operand);
	      val = eval (operand);
	      new_sym (label, val, false);
	      for (i = 0; i < 8; i++)
		{
		  *cp-- = digits[(int) (val & 15)];
		  val >>= 4;
		}
	    }
	  break;
	case psu_include:
	  do_include (operands);
	  break;
	case psu_if:
	  get_required_operand (operands, operand);
	  if (!eval (operand))
	    do_if_skip (list_on);
	  break;
	case psu_else:
	  do_if_skip (list_on);
	  break;
	case psu_endif:
	  break;
	case psu_liston:
	  list_on = true;
	  break;
	case psu_listoff:
	  list_on = false;
	  break;
	case psu_end:
	  do_label (label, pos);
	  done = true;
	  break;
	case psu_entry:
	  break;
	case op_nul:
	  do_label (label, pos);
	  break;
	}
      if (list_on)
	fputs (lstline, lstout);
    }
  while (!done);
  dumpline ();
  fprintf (binout, ":\n");
}

int
main (int argc, char **argv)
{

  printf ("DSK Assembler\n");
  if (argc != 4)
    usage ();

  infp = fopen (argv[1], "r");
  if (infp == NULL)
    {
      perror ("open input");
      return 2;
    }
  filename = argv[1];
  strcpy (inc_filenam[0], filename);

  binout = fopen (argv[2], "w");
  if (binout == NULL)
    {
      perror ("createing binout");
      return 2;
    }

  lstout = fopen (argv[3], "w");
  if (lstout == NULL)
    {
      perror ("creating listing");
      return 2;
    }

  init_ar ();
  pass_1 ();
  printf ("Pass 1 done\n");
  rewind (infp);
  pass_2 ();
  printf ("Pass 2 done, %d symbols, %d lines\n", num_syms, lino);

  if (fclose (infp) == EOF)
    {
      perror ("closing input file");
      error_count++;
    }

  if (fclose (binout) == EOF)
    {
      perror ("closing binary output file");
      error_count++;
    }

  if (fclose (lstout) == EOF)
    {
      perror ("closing listing");
      error_count++;
    }

  if (error_count)
    printf ("%d errors detected\n", error_count);

  return 0;
}

/* end of dska.c */
