/*
 *  IMS Open Corpus Workbench (CWB)
 *  Copyright (C) 1993-2006 by IMS, University of Stuttgart
 *  Copyright (C) 2007-     by the respective contributers (see file AUTHORS)
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2, or (at your option) any later
 *  version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
 *  Public License for more details (in the file "COPYING", or available via
 *  WWW at http://www.gnu.org/copyleft/gpl.html).
 */


#include "../cl/globals.h"
#include "../cl/corpus.h"
#include "../cl/cl.h"
#include "../cl/special-chars.h"

/** use 1 million buckets by default */
#define DEFAULT_BUCKETS 1000000

/** maximum value of N (makes life a little easier) */
#define MAX_N 32

/* TODO: rewrite hash entries so that K-tuples can be embedded in the HashEntry struct,
   e.g. as struct { HashEntry next; int freq; int tuple[1]; }; then allocate  sizeof(*HashEntry) + (K-1) * sizeof(int) bytes
   for each structure and access tuple as entry->tuple[i] or in a similar way */

/**
 * Structure representing hash entries.
 *
 * @see Hash
 */
typedef struct _hash_entry {
  int *tuple;               /**< K-tuple of lexicon IDs (with K == Hash.K) */
  int freq;                 /**< frequency of this K-tuple (so far) */
  struct _hash_entry *next;
} *HashEntry;


/**
 * A specialised hashtable for computing frequency distributions over tuples of lexicon IDs.
 */
struct _Hash {
  int N;                    /**< number of keys, including constraint-only keys */
  Attribute *(att[MAX_N]);  /**< list of the N attributes corresponding to the keys of the hash */
  int offset[MAX_N];        /**< list of optional corpus position offsets */
  int max_offset;           /**< largest offset of all keys (to avoid scanning past end of corpus */
  int is_structural[MAX_N]; /**< list of flags identifying s-attributes (all others are p-attributes) */
  CL_Regex regex[MAX_N];    /**< optional regex constraint (compiled regular expression) */
  int is_negated[MAX_N];    /**< whether regex constraint is negated (!=) */

  /* optional frequency values for corpus rows */
  Attribute *frequency_values;
  int *frequency;           /**< pre-computed integer values for the attribute keys */

  /* p-attributes */
  int *(id_list[MAX_N]);    /**< optional regex constraint (stored as a list of matching lexicon IDs) */
  int id_list_size[MAX_N];  /**< size of this list */

  /* s-attributes */
  int current_struc[MAX_N]; /**< number of current or next structure */
  int start_cpos[MAX_N];    /**< start of this structure (cpos) */
  int end_cpos[MAX_N];      /**< end of this structure (cpos) */
  int constraint_ok[MAX_N]; /**< whether constraint is satisfied (initialised at start_cpos, reset at end_cpos) */
  int virtual_id[MAX_N];    /**< virtual ID of a region's annotation string (constant within region) */
  char *source_base[MAX_N]; /**< base pointers to compute virtual IDs (= offsets) from annotation strings */

  int is_constraint[MAX_N]; /**< list of flags marking constraint keys ("?...") */
  int K;                    /**< number of non-constraint keys, i.e. the actual hash table stores K-tuples */
  int buckets;              /**< no. of buckets in hash table */
  HashEntry *table;         /**< array of HashEntry pointers == buckets (initialised to NULL, i.e. empty buckets) */
} Hash;

/* other global variables */
Corpus *C;                   /**< corpus we're working on */
char *reg_dir = NULL;        /**< registry directory (NULL -> use default) */
char *corpname = NULL;       /**< corpus name (command-line) */
int check_words = 0;         /**< if set, accept only 'regular' words in frequency counts */
CL_Regex regular_rx = NULL;  /**< regex object for use when check_words is true. @see scancorpus_word_is_regular */
char *progname = NULL;       /**< name of this program (from shell command) */
char *output_file = NULL;    /**< output file name (-o option) */
int frequency_threshold = 0; /**< frequency threshold for result table (-f option) */
char *frequency_att = NULL;  /**< p-attribute with frequency entries for corpus rows (when abusing corpus as frequency database) */
int global_start = 0;        /**< start scanning at this cpos (defaults to start of corpus) */
int global_end = -1;         /**< will be set up in main() unless changed with -e switch. @see global_start */
char *ranges_file = NULL;    /**< file with ranges to scan (pairs of corpus positions) */
FILE *ranges_fh = NULL;      /**< corresponding filehandle */
int quiet = 0;               /**< if set, don't show progress information on stderr */



/**
 * Prints a usage message and exits the program.
 */
void
scancorpus_usage(void)
{
  fprintf(stderr, "\n");
  fprintf(stderr, "Usage: cwb-scan-corpus [options] <corpus> <key1> <key2> ... \n\n");
  fprintf(stderr, "Computes the joint frequency distribution over <key1>, <key2>, ... .\n");
  fprintf(stderr, "Each key specifier takes the form:\n\n");
  fprintf(stderr, "    [?]<att>[+<n>][[!]=/<regex>/[cd]]\n\n");
  fprintf(stderr, "where <att> is a positional or structural attribute, <n> an optional\n");
  fprintf(stderr, "non-negative offset (number of tokens to the right), and <regex> an optional\n");
  fprintf(stderr, "regular expression that the key must match ('=') or not match ('!='). The\n");
  fprintf(stderr, "regex may be followed by 'c' (ignore case) and/or 'd' (ignore diacritics).\n");
  fprintf(stderr, "The optional '?' sign marks a \"constraint\" key which will not be included\n");
  fprintf(stderr, "in the resulting frequency distribution. Up to %d keys may be specified in total.\n\n", MAX_N);
  fprintf(stderr, "The output is a table of the form:\n");
  fprintf(stderr, "  <f>  TAB  <key1-value>  TAB  <key2-value>  TAB  ... \n\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  -r <dir>  use registry directory <dir>\n");
  fprintf(stderr, "  -b <n>    use <n> hash buckets [default: 1,000,000]\n");
  fprintf(stderr, "  -o <file> write frequency table to <file> [default"": standard output]\n");
                                                            /* 'default:' confuses Emacs C-mode */
  fprintf(stderr, "            (compressed with gzip if <file> ends in '.gz')\n");
  fprintf(stderr, "  -f <n>    include only items with frequency >= <n> in result table\n");
  fprintf(stderr, "  -F <att>  add up frequency values from p-attribute <att>\n");
  fprintf(stderr, "  -C        clean up data, i.e. accept only \"regular\" words\n");
  fprintf(stderr, "            (does not apply to constraint keys marked with '?')\n");
  fprintf(stderr, "  -s <n>    start scanning at corpus position <n>\n");
  fprintf(stderr, "  -e <n>    stop scanning at corpus position <n>\n");
  fprintf(stderr, "  -R <file> read list of corpus ranges to scan from <file>\n");
  fprintf(stderr, "  -q        quiet mode (no progress information on stderr)\n");
  fprintf(stderr, "  -h        this help page\n\n");
  fprintf(stderr, "Part of the IMS Open Corpus Workbench v" VERSION "\n\n");
  exit(1);
}

/**
 * Parses the command-line options of the program.
 *
 * @param argc  argc from main()
 * @param argv  argv from main()
 * @return      The value of global optind after the function has run.
 */
int
scancorpus_parse_options(int argc, char *argv[])
{
  extern int optind;
  extern char *optarg;
  int c;

  while ((c = getopt(argc, argv, "+r:b:o:f:F:Cs:e:R:qh")) != EOF) {
    switch (c) {
    case 'r':                        /* -r <dir> */
      if (reg_dir == NULL)
        reg_dir = optarg;
      else {
        fprintf(stderr, "Error: -r option used twice.\n");
        exit(1);
      }
      break;
    case 'b':                        /* -b <n> */
      Hash.buckets = atoi(optarg);
      break;
    case 'o':                        /* -o <file> */
      if (output_file == NULL)
        output_file = optarg;
      else {
        fprintf(stderr, "Error: -o option used twice.\n");
        exit(1);
      }
      break;
    case 'f':                        /* -f <n> */
      frequency_threshold = atoi(optarg);
      break;
    case 'F':                        /* -F <att> */
      if (frequency_att == NULL)
        frequency_att = optarg;
      else {
        fprintf(stderr, "Error: -F option used twice.\n");
        exit(1);
      }
      break;
    case 'C':                        /* -C */
      check_words = 1;
      break;
    case 's':                        /* -s <n> */
      global_start = atoi(optarg);
      break;
    case 'e':                        /* -e <n> */
      global_end = atoi(optarg);
      break;
    case 'R':                        /* -R <file> */
      if (ranges_file == NULL)
        ranges_file = optarg;
      else {
        fprintf(stderr, "Error: -R option used twice.\n");
        exit(1);
      }
      break;
    case 'q':
      quiet = 1;
      break;
    case 'h':                       /* -h */
    default:                        /* unknown option: print usage info */
      scancorpus_usage();
    }
  }

  return(optind);
}


/* hash utility functions */

/**
 * Checks whether a number is prime.
 *
 * @param n  number to check
 * @return   Boolean
 */
int
is_prime(int n)
{
  int i;
  for(i = 2; i*i <= n; i++)
    if ((n % i) == 0)
      return 0;
  return 1;
}

/**
 * Finds a prime number.
 *
 * @param n  lower bound for the generated prime.
 * @return   The first prime number greater than n,
 *           or 0 if no prime number was found within
 *           the sizeof a signed int.
 */
int
find_prime(int n)
{
  for( ; n > 0 ; n++)                /* will exit on int overflow */
    if (is_prime(n))
      return n;
  return 0;
}


/**
 * Computes a hash index for an N-tuple of ints.
 *
 * @param N      Size of the tuple.
 * @param tuple  The tuple itself: an array of ints.
 * @return       The hash index calculated.
 */
unsigned int
hash_index(int N, int *tuple)
{
  unsigned int result = 0;
  int i;

  for(i = 0 ; i < N; i++)
    result = (result * 33 ) ^ (result >> 27) ^ tuple[i];
  return result;
}


/**
 * Compares two N-tuples for equality.
 *
 * @param N    Size of the tuple.
 * @param t1   First tuple (array of ints of size N).
 * @param t2   Second tuple (array of ints of size N).
 * @return     Boolean: true if all ints are identical,
 *             otherwise false.
 */
int
tuples_eq(int N, int *t1, int *t2)
{
  int i;
  for (i = 0; i < N; i++) {
    if (t1[i] != t2[i]) return 0;
  }
  return 1;
}


/**
 * Finds an N-tuple in the global hash.
 *
 * @param tuple    The tuple to search for.
 * @param R_index  The index of the bucket containing the
 *                 located HashEntry.
 * @return         The located HashEntry.
 */
HashEntry
hash_find(int *tuple, int *R_index)
{
  unsigned int index;
  HashEntry entry;

  index = hash_index(Hash.K, tuple) % Hash.buckets;
  if (R_index != NULL) *R_index = index;
  entry = Hash.table[index];
  while (entry != NULL && !tuples_eq(Hash.K, tuple, entry->tuple)) {
    entry = entry->next;
  }
  return entry;
}


/**
 * Inserts an N-tuple into the global hash.
 *
 * If the N-tuple is already in the hash, its count
 * is incremented by f, but nothing is inserted.
 *
 * @param tuple  The tuple to add (array of ints).
 * @param f      The frequency of the tuple.
 */
void
hash_add(int *tuple, int f)
{
  HashEntry entry;
  int index;

  entry = hash_find(tuple, &index);
  if (entry != NULL) {          /* tuple already in hash -> increment count */
    entry->freq += f;
  }
  else {                        /* add new tuple (to bucket <index> */
    entry = (HashEntry) cl_malloc(sizeof(struct _hash_entry));
    entry->tuple = (int *) cl_malloc(Hash.K * sizeof(int));
    /* bcopy(tuple, entry->tuple, Hash.K * sizeof(int)); */
    memcpy(entry->tuple, tuple, Hash.K * sizeof(int));    /* we can just copy it verbatim */
    entry->freq = f;
    entry->next = Hash.table[index];
    Hash.table[index] = entry;
  }
}




/**
 * Check regularity of a token.
 *
 * A token is "regular" if it contains only letters, numbers and dashes
 * (with no dash at the start or end).
 *
 * "Regularity" is used as a filter on the corpus iff the -C option
 * is specified.
 *
 * @param s  String containing the token to check.
 * @return   True if the token is regular, otherwise false.
 */
int
scancorpus_word_is_regular(char *s)
{
  /* bad pointer or empty string or first char is hyphen? not regular */
  if (s == NULL || *s == '\0' || *s == '-')
    return 0;

  /* otherwise, different approach of utf8 versus iso8859 */
  if (C->charset == utf8)
    return cl_regex_match(regular_rx, s);
  else {
    char *p = s;
    while (*p) {
      /* each component of the string may be... */
      /* a sequence of digits: if so scroll through */
      if (*p >= '0' && *p <= '9')
        while (*p >= '0' && *p <= '9')
          p++;
      /* or a sequence fo letters: if so scroll through */
      else if (cl_iso_char_is_alphanumeric(*p, C->charset))
        while (cl_iso_char_is_alphanumeric(*p, C->charset))
          p++;
      /* otherwise this isn't the start of a valid component */
      else
        return 0;
      /* we are at the end of a component: if it is also the end of
       * the string this is regular; otherwise there must be a hyphen,
       * followed by another component */
      if (*p == '\0')
        return 1;
      if (*p++ != '-')
        return 0;
      /* else: there IS a hyphen, so we need to loop again */
    }
    /* we are at the end of the string: was the last character a hyphen? */
    if (*(p-1) == '-')
      return 0;
  }
  /* when we get here, the word ended in a '-' => not regular */
  return 0;

#if 0  /* the old version of this function. */
  char *p = s;
  while (*p) {                          /* each component of the word may be ... */
    if (*p >= '0' && *p <= '9') { /* ... a number */
      while (*p >= '0' && *p <= '9') p++;
    }
    else if (is_letter(*p)) {          /* ... or a word (i.e. consist entirely of letters) */
      while (is_letter(*p)) p++;
    }
    else {
      return 0;
    }
    /* if we're at end-of word, it is regular */
    if (*p == 0)
      return 1;
    /* otherwise there must be a hyphen '-', followed by another component */
    if (*p++ != '-')
      return 0;
  }
  /* when we get here, the word was either empty or ended in a '-' => not regular */
  return 0;
#endif
}



/**
 * Adds a key to global variable Hash.
 *
 * @param key  String specifying the key (passed by
 *             main() from a command-line argument)
 */
void
scancorpus_add_key(char *key)
{
  char buf[CL_MAX_LINE_LENGTH]; /* stores copy of <key> if we have to mess around with it */
  Attribute *att = NULL;        /* p-attribute object for attribute <att> */
  int offset = 0;               /* offset obtained from [+<n>] part of key specifier */
  char *regex = NULL;           /* <regex> from optional [=/<regex>/[cd]] part */
  int flags = 0;                /* optional ignore case and/or diacritics flags ([cd]) */
  int is_negated = 0;           /* regex constraint negated? */
  int is_constraint = 0;        /* just a constraint key? */
  int is_structural = 0;        /* positional or structural attribute? */
  char *p;
  int list_size, mark, point;

  if (key[0] == '?') {
    is_constraint = 1;
    strcpy(buf, key+1);
  }
  else
    strcpy(buf, key);

  regex = strchr(buf, '=');        /* check for "=/<regex>/" or "!=/<regex>/" */
  if (regex != NULL) {
    if (regex > buf) {
      if (regex[-1] == '!') {
        regex[-1] = '\0';
        is_negated = 1;
      }
    }
    *(regex++) = '\0';                /* terminate part of key before regex */
    if (*regex != '/') {
      fprintf(stderr, "Syntax error in regex part of key '%s'.\n", key);
      exit(2);
    }
    regex++;                        /* now <regex> should point to the actual regex part */
    p = strrchr(regex, '/');        /* find end of regex ('/'), and terminate regex string */
    if (p == NULL) {
      fprintf(stderr, "Syntax error in regex part of key '%s'.\n", key);
      exit(2);
    }
    *(p++) = '\0';
    if (strspn(p, "cd") != strlen(p)) {        /* may only have flags after the end-or-regex '/' */
      fprintf(stderr, "Syntax error in regex part of key '%s' (invalid flags).\n", key);
      exit(2);
    }
    if (strchr(p, 'c'))
      flags |= IGNORE_CASE;
    if (strchr(p, 'd'))
      flags |= IGNORE_DIAC;
  }

  p = strchr(buf, '+');                /* check for "+<n>" (before regex if present) */
  if (p != NULL) {
    *(p++) = '\0';                /* terminate <att> part */
    if (strspn(p, "0123456789") != strlen(p)) {        /* check that <n> is really a non-negative integer */
      fprintf(stderr, "Error: non-integer offset in key '%s'.\n", key);
      exit(2);
    }
    offset = atoi(p);
  }

  /* now <buf> points to the attribute name, <regex> is NULL or points to a regular expression,
     the optional <flags> are set up, and <offset> is 0 or set to <n> */
  if ((att = cl_new_attribute(C, buf, ATT_POS)) != NULL)
    is_structural = 0;
  else if ((att = cl_new_attribute(C, buf, ATT_STRUC)) != NULL)
    is_structural = 1;
  else {
    fprintf(stderr, "Error: can't open attribute %s.%s\n", corpname, buf);
    fprintf(stderr, "      (possibly a syntax error in key '%s')\n", key);
    exit(1);
  }
  Hash.att[Hash.N] = att;
  Hash.is_structural[Hash.N] = is_structural;
  Hash.offset[Hash.N] = offset;
  if (offset > Hash.max_offset)
    Hash.max_offset = offset;
  Hash.is_constraint[Hash.N] = is_constraint;
  Hash.is_negated[Hash.N] = is_negated;

  if (regex != NULL) {                /* optional regex constraint */
    Hash.regex[Hash.N] = cl_new_regex(regex, flags, cl_corpus_charset(C)); /* compile regular expression */
    if (Hash.regex[Hash.N] == NULL) {
      fprintf(stderr, "Error: can't compile regex /%s/\n", regex);
      fprintf(stderr, "      (possibly a syntax error in key '%s')\n", key);
      exit(1);
    }
    if (! is_structural) { /* p-attribute: compile regex into list of matching lexicon IDs */
      Hash.id_list[Hash.N] = cl_regex2id(att, regex, flags, &list_size);
      Hash.id_list_size[Hash.N] = list_size;
      if (check_words && !is_constraint) { /* reduce ID list to regular words with -C option (but not for constraint keys) */
        point = mark = 0;
        while (point < list_size) {
          if (scancorpus_word_is_regular(cl_id2str(att, Hash.id_list[Hash.N][point])))
            Hash.id_list[Hash.N][mark++] = Hash.id_list[Hash.N][point];
          point++;
        }
        Hash.id_list_size[Hash.N] = mark;
      }
      if (Hash.id_list_size == 0) {
        fprintf(stderr, "Warning: no matches for key '%s' -- scan results will be empty\n", key);
      }
    }
  }
  else {                        /* no constraint specified */
    Hash.id_list[Hash.N] = NULL;    /* p-attribute */
    Hash.id_list_size[Hash.N] = -1; /* need -1 to distinguish from empty constraint list (non-matching regex) */
    Hash.regex[Hash.N] = NULL;             /* s-attribute */
  }

  if (is_structural) {                /* additional setup for s-attribute */
    if (cl_max_struc(att) <= 0) {
      fprintf(stderr, "Error: s-attribute %s.%s is empty (aborted)\n", corpname, buf);
      exit(1);
    }
    if (!cl_struc_values(att) && !(is_constraint && regex == NULL)) {
      /* s-attributes without annotation allowed for special ``?head'' constraints to restrict scan to regions */
      fprintf(stderr, "Error: s-attribute %s.%s has no annotations (aborted)\n", corpname, buf);
      exit(1);
    }
    Hash.current_struc[Hash.N] = -1;
    Hash.start_cpos[Hash.N] = -1;
    Hash.end_cpos[Hash.N] = -1;
    Hash.constraint_ok[Hash.N] = 0;
    Hash.source_base[Hash.N] =  /* should be pointer to start of lexicon data (NULL marks special ``?head'' case) */
      (cl_struc_values(att)) ? cl_struc2str(att, 0) : NULL;
  }

  Hash.N++;
  if (! is_constraint) Hash.K++;
}


/**
 * Reads the next range of corpus positions.
 *
 * The ranges of corpus positions are taken either from
 * global settings (-s, -e) or from a specified file (-R).
 *
 * @param start Where to put the start of the next range.
 * @param end   Where to put the end of the next range.
 * @return   FALSE after last range, TRUE otherwise
 */
int
get_next_range(int *start, int *end)
{
  char buffer[CL_MAX_LINE_LENGTH];

  *start = *end = -1;                /* these values are returned on error or at end-of-input */
  if (ranges_fh) {
    if ((fgets(buffer, CL_MAX_LINE_LENGTH, ranges_fh) != NULL) &&
        (sscanf(buffer, "%d %d", start, end) == 2))
      return 1;
    else
      return 0;
  }
  else {
    if (global_start < 0 || global_end < 0) {
      return 0;
    }
    else {
      *start = global_start;
      *end = global_end;
      global_start = global_end = -1; /* so next call will return end-of-input */
      return 1;
    }
  }
}

/* *************** *\
 *      MAIN()     *
\* *************** */

/**
 * Main function for cwb-scan-corpus.
 *
 * @param argc   Number of command-line arguments.
 * @param argv   Command-line arguments.
 */
int
main (int argc, char *argv[])
{
  int argind;                      /* will be set to the index of first (non-option) argument in argv[] */
  int Csize = 0;                   /* corpus size (= number of tokens) */
  Attribute *word;                 /* need default p-attribute to compute corpus size */
  int cpos, next_cpos, start_cpos, end_cpos, previous_end;

  /* parse command line options */
  Hash.buckets = DEFAULT_BUCKETS;  /* must pre-initialise Hash.buckets */
  progname = argv[0];
  argind = scancorpus_parse_options(argc, argv);
  if ((argc - argind) < 2) {
    scancorpus_usage();                       /* not enough arguments -> print usage info */
  }

  /* initialise hash */
  Hash.N = 0;                      /* will be incremented when we process the arguments */
  Hash.K = 0;
  Hash.max_offset = 0;
  Hash.buckets = find_prime(Hash.buckets); /* make sure number of buckets is a prime */
  Hash.table = cl_calloc(Hash.buckets, sizeof(HashEntry));
  Hash.frequency_values = NULL;
  Hash.frequency = NULL;

  /* first argument: corpus */
  corpname = argv[argind++];
  C = cl_new_corpus(reg_dir, corpname);
  if (C == NULL) {
    fprintf(stderr, "Error: can't find corpus %s (in registry %s)\n", corpname, (reg_dir) ? reg_dir : cl_standard_registry());
    exit(1);
  }

  /* now we know the corpus (and its character set) we can initialise the global regular expression object, if needed */
  if (check_words) {
    int rx_flags;
    char *cleanup_rx_string;

    if (C->charset == utf8) {
      /* utf8: don't fold diacritics, but use Unicode character properties */
      if (NULL == (regular_rx = cl_new_regex("([\\pL\\pM]+|\\pN+)(-([\\pL\\pM]+|\\pN+))*", 0, C->charset)) ) {
        fprintf(stderr, "Error: can't initialise regex\n");
        exit(1);
      }
    }
    /* other character sets don't use regex engine: see scancorpus_word_is_regular */
  }

  /* remaining arguments are specifiers for keys forming N-tuple */
  while (argind < argc) {
    scancorpus_add_key(argv[argind]);
    argind++;
  }

  /* determine size of corpus */
  word = cl_new_attribute(C, "word", ATT_POS);
  if (word == NULL) {
    fprintf(stderr, "Error: can't load attribute %s.%s\n", corpname, "word");
    exit(1);
  }
  Csize = cl_max_cpos(word);

  /* check and adjust start and end cpos for scan */
  if (global_start < 0)
    global_start = 0;
  if (global_end < 0 || global_end >= Csize) /* initialise global_end to end of corpus (if -e flag wasn't used) */
    global_end = Csize - 1;

  /* if -R option was used, open file with ranges of corpus positions ("-" for stdin) */
  if (ranges_file) {
    if (strcmp(ranges_file, "-") == 0)
      ranges_fh = stdin;
    else {
      ranges_fh = fopen(ranges_file, "r");
      if (! ranges_fh) {
        perror(ranges_file);
        exit(1);
      }
    }
  }

  /* open attribute with frequency entries & precompute vector with numeric values (-F option) */
  if (frequency_att) {
    int freq_id_range, id, intval;
    char *strval;

    Hash.frequency_values = cl_new_attribute(C, frequency_att, ATT_POS);
    if (Hash.frequency_values == NULL) {
      fprintf(stderr, "Error: can't load attribute %s.%s\n", corpname, frequency_att);
      exit(1);
    }

    freq_id_range = cl_max_id(Hash.frequency_values);
    Hash.frequency = (int *) cl_malloc(freq_id_range * sizeof(int));
    for (id = 0; id < freq_id_range; id++) {
      strval = cl_id2str(Hash.frequency_values, id);
      intval = atoi(strval);
      if (intval <= 0) {
        fprintf(stderr, "Invalid frequency value '%s' in -F %s options. Aborted.\n", strval, frequency_att);
        exit(1);
      }
      Hash.frequency[id] = intval;
    }
  }

  if (! quiet)
    fprintf(stderr, "Scanning corpus %s for %d-tuples ... \n", corpname, Hash.N);

  /* loop over all the ranges to be scanned (which is just a single range without -R) */
  previous_end = -1;
  while (get_next_range(&start_cpos, &end_cpos)) {
    if (start_cpos <= previous_end) { /* this also ensures that start_cpos >= */
      fprintf(stderr, "Overlapping or unsorted ranges: [?, %d] and [%d, %d]. Aborted.\n",
              previous_end, start_cpos, end_cpos);
      exit(1);
    }
    if (end_cpos < start_cpos) {
      fprintf(stderr, "Invalid range [%d, %d] (inversion). Aborted.\n", start_cpos, end_cpos);
      exit(1);
    }
    previous_end = end_cpos;
    if (end_cpos >= Csize)        /* check that end_cpos is within allowed range [..., Csize-1] */
      end_cpos = Csize - 1;
    if (Hash.max_offset > 0)
      end_cpos -= Hash.max_offset; /* adjust end_cpos so that all tokens in the tuple fall within the specified range */
    if (end_cpos < start_cpos) {
      fprintf(stderr, "Warning: range [%d, %d] is too small for selected data (skipped).\n",
              start_cpos, end_cpos + Hash.max_offset);
    }

    /* start the scan loop for this range */
    for (cpos = start_cpos; cpos <= end_cpos; cpos = next_cpos) {
      int tuple[MAX_N];
      int i=0, k, accept;

      next_cpos = cpos + 1;        /* this device allows the code to "skip" to the next matching region for s-attribute constraints */

      if ((! quiet) && ((cpos & 0xffff) == 0)) {
        int cpK = cpos >> 10;
        int csK = Csize >> 10;
        fprintf(stderr, "Progress: %6dK / %dK   \r", cpK, csK);
        fflush(stderr);
      }

      accept = 1;
      k = 0;
      for (i = 0; i < Hash.N; i++) { /* don't abort when accept==0, because of side effects for s-attributes */
        int effective_cpos = cpos + Hash.offset[i];
        int id, size, bot, top, mid;
        int *idlist;
        char *str;

        if (! accept)                 /* once accept==0, no need to compute id's and check constraints */
          continue;

        if (! Hash.is_structural[i]) { /* p-attribute -> id = lexicon ID */
          id = cl_cpos2id(Hash.att[i], effective_cpos);

          size = Hash.id_list_size[i]; /* check for optional regex constraint */
          if (size > 0) {                /* constraint has been compiled into ID list */
            idlist = Hash.id_list[i]; /* check id against idlist[] (using binary search) */
            assert((idlist != NULL) && "Oops. Big internal bug.");
            bot = 0; top = size - 1;
            while (bot < top) {
              mid = (bot + top) / 2; /* split [bot, top] into [bot, mid] and [mid+1, top] */
              if (id <= idlist[mid])
                top = mid;
              else
                bot = mid + 1;
            }
            if (id == idlist[bot]) {   /* now id==idlist[bot==top], or id is not in list */
              if (Hash.is_negated[i])  /* a) id found -> reject if constraint is negated */
                accept = 0;            
            } 
            else {
              if (Hash.is_negated[i]) {/* b) id not found -> reject unless negated, otherwise must check -C flag */
                if (check_words && !Hash.is_constraint[i]) {
                   /* id matching negative constraint may not have been found in idlist[] filtered with -C flag,
                      so we need to check explicitly that the corresponding string is regular in this case */
                  str = cl_id2str(Hash.att[i], id);
                  if (!scancorpus_word_is_regular(str)) accept = 0;
                }
              }
              else accept = 0;
            }
          }
          else if (size == 0) {        /* empty list: constraint cannot be satisfied */
            accept = 0;
          }
          else if (check_words) {      /* no regex, but -C option specified: check now whether word is regular */
            str = cl_id2str(Hash.att[i], id);
            if (!scancorpus_word_is_regular(str)) accept = 0;
          }
        }
        else {                             /* s-attribute -> id = offset of annotation string in lexicon data */
          while (effective_cpos > Hash.end_cpos[i]) { /* jump to next region after point when necessary */
            Hash.current_struc[i]++;
            if (Hash.current_struc[i] >= cl_max_struc(Hash.att[i])) { /* finished with last struc */
              Hash.start_cpos[i] = Hash.end_cpos[i] = Csize; /* will never be reached */
              Hash.constraint_ok[i] = 0;   /* constraint cannot be fulfilled after end of last region */
              Hash.virtual_id[i] = -1;     /* no annotation (undef) */
            }
            else {                         /* update Hash data structure with information for next region */
              cl_struc2cpos(Hash.att[i], Hash.current_struc[i], &(Hash.start_cpos[i]), &(Hash.end_cpos[i]));
              if (Hash.source_base[i]) {
                str = cl_struc2str(Hash.att[i], Hash.current_struc[i]);
                Hash.virtual_id[i] = str - Hash.source_base[i];
              }
              else {
                str = "NULL";  /* s-attribute without annotation, allowed for special ``?head'' constrains */
                Hash.virtual_id[i] = -1;
              }
              Hash.constraint_ok[i] = 1;
              if (Hash.regex[i] != NULL) {
                if (cl_regex_match(Hash.regex[i], str)) {
                  if (Hash.is_negated[i]) Hash.constraint_ok[i] = 0;  /* negated regex matches -> reject */
                }
                else {
                  if (!Hash.is_negated[i]) Hash.constraint_ok[i] = 0; /* plain regex matches -> reject */
                }
              }
              if (check_words && !Hash.is_constraint[i] && !scancorpus_word_is_regular(str))   /* -C flag (ignored for constraint keys) */
                Hash.constraint_ok[i] = 0;
              /* may jump directly to next region when regex constraint is present (or for ``?head'' constraints) */ 
              if (Hash.regex[i] != NULL || Hash.source_base[i] == NULL) { 
                int jump_target;
                if (Hash.constraint_ok[i])
                  jump_target = Hash.start_cpos[i] - Hash.offset[i]; /* convert back from effective_cpos to cpos */
                else
                  jump_target = Hash.end_cpos[i] + 1 - Hash.offset[i]; /* jump past next region if it doesn't match the constraint */
                if (jump_target > next_cpos)   /* schedule jump to target after current iteration */
                  next_cpos = jump_target;
              }
            }
          }

          if (effective_cpos >= Hash.start_cpos[i]) { /* when in region, use relevant information in Hash data structure */
            id = Hash.virtual_id[i];
            if (Hash.regex[i] != NULL || check_words) /* apply stored constraint flag if regex or -C is in effect */
              if (! Hash.constraint_ok[i])            /* (should always be TRUE otherwise, so the condition may be redundant) */
                accept = 0;
          }
          else {                        /* outside region, ID is undef (-1) and any regex constraint fails */
            id = -1;
            if (Hash.regex[i] != NULL || Hash.is_constraint[i]) 
              accept = 0; /* pure constraint keys also fail outside regions */
            /* note that -C flag is _not_ applied here */
          }
        }

        if (! Hash.is_constraint[i]) {
          tuple[k++] = id;        /* build K-tuple for this corpus position */
        }
      }

      if (accept) {
        if (Hash.frequency_values) /* note that the frequency attribute is always used with offset 0 */
          hash_add(tuple, Hash.frequency[cl_cpos2id(Hash.frequency_values, cpos)]);
        else
          hash_add(tuple, 1);
      }
    } /* end of scan loop for current range */

  } /* end of loop over ranges */

  if (! quiet)
    fprintf(stderr, "Scan complete.                          \n");

  /* close ranges file (if -R option had been used) */
  if (ranges_fh && ranges_fh != stdin)
    fclose(ranges_fh);

  /* print hash contents to stdout or file (in hash-internal order) */
  {
    HashEntry entry;
    FILE *of;
    char pipe_cmd[CL_MAX_LINE_LENGTH];
    int bucket, i, k, l, is_pipe;
    char *str;

    is_pipe = 0;
    if (output_file != NULL) {
      l = strlen(output_file);
      if ((l > 3) && (strncasecmp(output_file + l - 3, ".gz", 3) == 0)) {
        sprintf(pipe_cmd, "gzip > %s", output_file); /* write .gz file through gzip pipe */
        of = popen(pipe_cmd, "w");
        if (of == NULL) {
          perror(pipe_cmd);
          fprintf(stderr, "Can't write compressed file %s. Aborted.\n", output_file);
          exit(1);
        }
        is_pipe = 1;
        if (! quiet)
          fprintf(stderr, "Writing frequency table to compressed file %s ... ", output_file);
      }
      else {
        of = fopen(output_file, "w");
        if (of == NULL) {
          perror(output_file);
          fprintf(stderr, "Can't create file %s. Aborted.\n", output_file);
          exit(1);
        }
        if (! quiet)
          fprintf(stderr, "Writing frequency table to %s ... ", output_file);
      }
    }
    else {
      of = stdout;
      if (! quiet)
        fprintf(stderr, "Printing frequency table on stdout ... \n");
    }
    fflush(stderr);

    for (bucket = 0; bucket < Hash.buckets; bucket++) {
      entry = Hash.table[bucket];
      while (entry != NULL) {
        if (entry->freq >= frequency_threshold) {
          fprintf(of, "%d", entry->freq);
          k = 0;
          for (i = 0; i < Hash.N; i++) {
            if (! Hash.is_constraint[i]) {
              if (! Hash.is_structural[i]) {
                str = cl_id2str(Hash.att[i], entry->tuple[k]);
              }
              else {
                if (entry->tuple[k] < 0) {
                  str = "";
                }
                else {
                  str = Hash.source_base[i] + entry->tuple[k];
                }
              }
              fprintf(of, "\t%s", str);
              k++;
            }
          }
          fprintf(of, "\n");
        }
        entry = entry->next;
      }
    }

    if (output_file != NULL) {
      if (is_pipe)
        pclose(of);
      else
        fclose(of);
    }
    if (! quiet)
      fprintf(stderr, "ok.\n");
  } /* endblock print hash contents to stdout */

  /* final act of cleanup */
  if (regular_rx)
    cl_delete_regex(regular_rx);

  exit(0);                        /* that was easy, wasn't it? */
}


