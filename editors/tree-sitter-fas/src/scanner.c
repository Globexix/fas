#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum TokenType {
  RAW_C_OPEN,
  RAW_C_BODY,
};

#define MAX_TAG 64

typedef struct {
  uint8_t length;
  char tag[MAX_TAG];
} Scanner;

static bool is_tag_start(int32_t c) {
  return c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool is_tag_char(int32_t c) {
  return is_tag_start(c) || (c >= '0' && c <= '9');
}

static bool is_blank(int32_t c) {
  return c == ' ' || c == '\t' || c == '\r' || c == '\f' || c == '\v';
}

static bool scan_open(Scanner *s, TSLexer *lexer) {
  while (is_blank(lexer->lookahead)) {
    lexer->advance(lexer, true);
  }
  if (lexer->lookahead != '<') {
    return false;
  }
  lexer->advance(lexer, false);
  if (lexer->lookahead != '<') {
    return false;
  }
  lexer->advance(lexer, false);
  if (!is_tag_start(lexer->lookahead)) {
    return false;
  }
  char tag[MAX_TAG];
  size_t length = 0;
  while (is_tag_char(lexer->lookahead)) {
    if (length == MAX_TAG) {
      return false;
    }
    tag[length++] = (char)lexer->lookahead;
    lexer->advance(lexer, false);
  }
  lexer->mark_end(lexer);
  memcpy(s->tag, tag, length);
  s->length = (uint8_t)length;
  lexer->result_symbol = RAW_C_OPEN;
  return true;
}

static bool scan_body(Scanner *s, TSLexer *lexer) {
  if (s->length == 0) {
    return false;
  }
  for (;;) {
    size_t matched = 0;
    bool still_matching = true;
    bool carriage_return = false;
    while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
      int32_t c = lexer->lookahead;
      if (still_matching && matched < s->length && c == (unsigned char)s->tag[matched]) {
        matched++;
      } else if (still_matching && matched == s->length && c == '\r' && !carriage_return) {
        carriage_return = true;
      } else {
        still_matching = false;
      }
      lexer->advance(lexer, false);
    }
    bool terminator = still_matching && matched == s->length;
    if (lexer->eof(lexer)) {
      if (!terminator) {
        return false;
      }
      lexer->mark_end(lexer);
      lexer->result_symbol = RAW_C_BODY;
      return true;
    }
    if (terminator) {
      lexer->mark_end(lexer);
      lexer->result_symbol = RAW_C_BODY;
      return true;
    }
    lexer->advance(lexer, false);
  }
}

void *tree_sitter_fas_external_scanner_create(void) {
  Scanner *s = calloc(1, sizeof(Scanner));
  return s;
}

void tree_sitter_fas_external_scanner_destroy(void *payload) { free(payload); }

unsigned tree_sitter_fas_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *s = payload;
  buffer[0] = (char)s->length;
  memcpy(buffer + 1, s->tag, s->length);
  return 1 + s->length;
}

void tree_sitter_fas_external_scanner_deserialize(void *payload, const char *buffer,
                                                  unsigned length) {
  Scanner *s = payload;
  s->length = 0;
  if (length == 0) {
    return;
  }
  s->length = (uint8_t)buffer[0];
  memcpy(s->tag, buffer + 1, s->length);
}

bool tree_sitter_fas_external_scanner_scan(void *payload, TSLexer *lexer,
                                           const bool *valid_symbols) {
  Scanner *s = payload;
  if (valid_symbols[RAW_C_OPEN]) {
    return scan_open(s, lexer);
  }
  if (valid_symbols[RAW_C_BODY]) {
    return scan_body(s, lexer);
  }
  return false;
}
