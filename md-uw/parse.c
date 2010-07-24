#include <stdio.h>

#include "md.h"

int check_next(FILE *f, unsigned char *buf, int len) {
  int i;

  for (i = 0; i < len; i++) {
    int c = fgetc(f);
    if ((c == EOF) ||
	(c != buf[i]))
      return 0;
  }
  return 1;
}

void error(char *msg) {
  fprintf(stderr, "Could not parse sysex file: %s\n", msg);
}

void print_params(unsigned char *params) {
  int i;
  for (i = 0; i < 8; i++) {
    printf("machine param %d: %x\n", i, params[i]);
  }
  for (; i < 16; i++) {
    printf("machine param %d: %x\n", i, params[i]);
  }
  for (; i < 24; i++) {
    printf("machine param %d: %x\n", i, params[i]);
  }
}

void decode_bytes(unsigned char *in, unsigned char *out, int count) {
  if (count > 7) {
    error("Can not decode more than 7 bytes in one go");
    return;
  }
  int i;
  for (i = 0; i < count; i++) {
    out[i] = in[i + 1];
    out[i] |= (in[0] & (1 << i)) << 7;
  }
}

int read_8bytes(unsigned char *buf, int len, FILE *f) {
  if (len > 7) {
    error("Cannot read more than 7 bytes in one go");
    return -1;
  }
  unsigned char tmp[len + 1];
  int res = fread(tmp, 1, len + 1, f);
  if (res != len + 1) {
    error("Could not read encoded bytes");
    return -1;
  }
  decode_bytes(tmp, buf, len);
  return len;
}

int read_bytes(unsigned char *buf, int len, FILE *f) {
  int i;
  for (i = 0; i < len; i += 7) {
    int tmplen = MIN(7, len - i);
    int res = read_8bytes(buf + i, tmplen, f);
    if (res != tmplen) {
      return -1;
    }
  }
  return len;
}

unsigned long read_big_endian_32bit(FILE *f) {
  unsigned char num[4];
  if (read_bytes(num, 4, f) != 4) {
    error("Could not read 32 bit integer");
    return -1;
  }
  return (num[0] | num[1] << 8 | num[2] << 16 | num[3] << 16);
}

int parse_global_message(FILE *f, md_global_t *global) {
error:
  return -1;
}

int parse_pattern_message(FILE *f, md_pattern_t *pattern) {
error:
  return -1;
}

int parse_song_message(FILE *f, md_song_t *song) {
error:
  return -1;
}

int parse_kit_message(FILE *f, md_kit_t *kit) {
  kit->version = fgetc(f);
  kit->revision = fgetc(f);
#if 0
  if ((kit->version == EOF) ||
      (kit->revision == EOF)) {
    error("Incomplete sysex file");
    goto error;
  }
#endif

  printf("Version: %d, Revision: %d\n", kit->version, kit->revision);
  
  kit->orig_pos = fgetc(f);
  int res = fread(kit->name, 1, 16, f);
  kit->name[16] = '\0';
  if (res != 16) {
    error("Could not read kitname");
    goto error;
  }

  printf("Kitname: %s\n", kit->name);

  
  res = fread(kit->params, 24, 16, f);
  if (res != 24 * 16) {
    error("Could not read track parameters");
    goto error;
  }

  res = fread(kit->levels, 1, 16, f);
  if (res != 16) {
    error("Could not read levels");
    goto error;
  }

  unsigned char models[16 * 4];
  res = read_bytes(models, sizeof(models), f);
  if (res != (16 * 4)) {
    error("Could not read models");
    goto error;
  }

  int i;
  for (i = 0; i < 16; i++) {
    kit->models[i] = models[i] | models[i*4 + 1] << 8;
  }

  return 0;

 error:
  return -1;
}

void parse(FILE *f) {
  if (!check_next(f, "\xF0\x00\x20\x3c\x02\x00", 6)) {
    error("Unknown sysex format");
    goto error;
  }

  printf("recognized machinedrum sysex file\n");
  
  int c = fgetc(f);
  if (c == EOF) {
    error("Incomplete sysex file");
    goto error;
  }

  switch (c) {
  case 0x50:
    {
      md_global_t global;
      parse_global_message(f, &global);
    }
    break;

  case 0x52:
    {
      md_kit_t kit;
      parse_kit_message(f, &kit);
    }
    break;
    
  case 0x67:
    {
      md_pattern_t pattern;
      parse_pattern_message(f, &pattern);
    }
    break;

  case 0x69:
    {
      md_song_t song;
      parse_song_message(f, &song);
    }
    break;

  default:
    error("Unknown machinedrum message");
    goto error;
  }

 error:
  return;
}

void usage(void) {
  fprintf(stderr, "Usage: ./parse sysexfile\n");
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    usage();
    return 1;
  }

  FILE *fin = fopen(argv[1], "r");
  if (fin == NULL) {
    usage();
    return 1;
  }

  parse(fin);

  fclose(fin);

  return 0;
}
