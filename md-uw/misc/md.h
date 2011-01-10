#ifndef MD_H__
#define MD_H__

#define MIN(a, b) ((a) > (b) ? (b) : (a))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

typedef struct md_keymap_s {
} md_keymap_t;

typedef struct md_global_s {
  unsigned char version, revision;
  unsigned char orig_pos;
  unsigned char routing[16];
  md_keymap_t keymap;
  unsigned char base_channel;
  unsigned short tempo;
  unsigned char extended;
  unsigned char external_sync;
  unsigned char local_on;
  unsigned char drum_left, drum_right;
  unsigned char gate_left, gate_right;
  unsigned char sense_left, sense_right;
  unsigned char min_left, min_right;
  unsigned char max_left, max_righ;
  unsigned char program_change;
  unsigned char trig_mode;
} md_global_t;

typedef struct md_lfo_s {
  unsigned char dest_track;
  unsigned char dest_param;
  unsigned char shape1, shape2;
  unsigned char type;
  unsigned char state[31];
} md_lfo_t;

typedef struct md_kit_s {
  unsigned char version, revision;
  unsigned char orig_pos;
  char name[16 + 1];
  unsigned char params[16][24];
  unsigned char levels[16];
  unsigned int models[16];
  md_lfo_t lfos[16];
  unsigned char reverb[8];
  unsigned char delay[8];
  unsigned char eq[8];
  unsigned char dynamics[8];
  unsigned char trig_group[16];
  unsigned char mute_group[16];
} md_kit_t;

typedef struct md_pattern_s {
  unsigned char version, revision;
  unsigned char orig_pos;
  unsigned int trig[16];
  unsigned int lock[16];
  unsigned int accent[16];
  unsigned char accent_amount;
  unsigned char length;
  unsigned char double_tempo;
  unsigned char scale;
  unsigned char kit;
  unsigned char locked_rows;
  unsigned char locks[24][32];
  unsigned int accent_pattern[16];
  unsigned int slide_pattern[16];
  unsigned int swing_pattern[16];
} md_pattern_t;

typedef struct md_song_row_s {
  char pattern;
  unsigned char kit;
  unsigned char loop;
  unsigned char jump;
  unsigned short mutes;
  unsigned short tempo;
  unsigned char start;
  unsigned char end;
} md_song_row_t;

typedef struct md_song_s {
  unsigned char version, revision;
  unsigned char orig_pos;
  char name[16 + 1];
  md_song_row_t rows[256];
} md_song_t;

#endif /* MD_H__ */
