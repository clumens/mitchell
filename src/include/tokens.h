#ifndef _TOKENS_H
#define _TOKENS_H 1

#include <stdlib.h>
#include <wchar.h>

typedef struct {
   int type;

   union {
      long     integer;
      wchar_t *string;
   };
} token_t;

#define  IDENTIFIER  100      /* a word identifier */
#define  INTEGER     101      /* an integer */
#define  STRING      102      /* a string */

#define  COMMENT     200      /* # */
#define  LPAREN      201      /* ( */
#define  RPAREN      202      /* ) */
#define  LBRACK      203      /* [ */
#define  RBRACK      204      /* ] */
#define  COMMA       205      /* , */
#define  DBLQUOTE    206      /* " */

token_t *next_token (FILE *f);

#endif
