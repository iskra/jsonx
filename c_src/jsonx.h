// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include "erl_nif.h"

#ifdef _MSC_VER
#define inline __inline
#endif

#if ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 7
#define ERL_MAP_SUPPORT
#endif

typedef struct{
  ERL_NIF_TERM am_true;
  ERL_NIF_TERM am_false;
  ERL_NIF_TERM am_null;

  ERL_NIF_TERM am_error;
  ERL_NIF_TERM am_erange;
  ERL_NIF_TERM am_estr;
  ERL_NIF_TERM am_esyntax;
  ERL_NIF_TERM am_etrailing;
  ERL_NIF_TERM am_undefined_record;

  ERL_NIF_TERM am_json;
  ERL_NIF_TERM am_struct;
  ERL_NIF_TERM am_proplist;
  ERL_NIF_TERM am_eep18;
  ERL_NIF_TERM am_map;
  ERL_NIF_TERM am_no_match;

  ErlNifResourceType* encoder_RSTYPE;
  ErlNifResourceType* decoder_RSTYPE;
}PrivData;

ERL_NIF_TERM decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/*
    Records descriptor for encode 

    +---------------------------------------+ <- 0
    | sizeof(EncEntry)                      |
    |                                      ----> [binary storage]
    |  ....                                 |
    +---------------------------------------+ <- (EncRecord*) ((void*)entry + sizeof(EncEntry))
    | sizeof(EncRecord) * Entry.records_cnt |
    | ...                                   |
    +---------------------------------------+ <- (EncField*) ((void*)entry + sizeof(EncEntry) + (sizeof(EncRecord) * entry->records_cnt))
    | sizeof(EncField) * Entry.fields_cnt   |
    | ...                                   |
    +---------------------------------------+

// %% %% Records descriptions for encode
// %% {
// %%   Rcnt                                 %% argv[0], Records count
// %%  ,Fcnt                                 %% argv[1], Counter all fields in records
// %%  ,Records = [{Tag, Fields_off, Arity}] %% argv[2], List of records tag, position and length fields
// %%  ,Fields  = [{Name_off, Size}]         %% argv[3], List of position and size fields names in binary storage
// %%  ,Bsz                                  %% argv[4], Binary data size
// %%  ,Bin                                  %% argv[5], Binary storage for names of fields, format - <,"name": >
// %% }
*/

typedef struct{
  unsigned offset; // set off in binary storage
  unsigned size; 
}EncField;

typedef struct{
  ERL_NIF_TERM tag; //atom
  unsigned arity;
  unsigned fds_offset;
}EncRecord;

typedef struct{
  ErlNifBinary bin;
  unsigned records_cnt;
  unsigned fields_cnt;
  ERL_NIF_TERM* ignored;
  unsigned ignored_len;
}EncEntry;

static inline EncRecord*
enc_records_base(EncEntry *entry){
  return (EncRecord*)((void*)entry + sizeof(EncEntry));
}

static inline EncField*
enc_fields_base(EncEntry *entry){
  return (EncField*)((void*)entry + sizeof(EncEntry) + (sizeof(EncRecord) * entry->records_cnt));
}

static inline size_t
enc_resource_size(unsigned rec_cnt, unsigned field_cnt){
return sizeof(EncEntry) + sizeof(EncRecord)*rec_cnt + sizeof(EncField)*field_cnt;
}

/** Records descriptor for decoder
%% { RecCnt         %% Records Counter
%%   , UKeyCnt      %% Uniq Keys Counter
%%   , KeyCnt       %% Keys Counter
%%   , UKeys        %% [Key]
%%   , Keys         %% [KeyNum]
%%   , Records3     %% [{Tag, Off, Len}]
%% };
%% 
 */

#define BITS_PER_WORD       (sizeof(long) * 8)
#define BITS_TO_WORDS(nb)   ((nb + BITS_PER_WORD - 1) / BITS_PER_WORD)
#define BITS_TO_BYTES(nb)   (BITS_TO_WORDS(nb) * sizeof(long))
#define BITS_TO_ETERM(nb)   (BITS_TO_WORDS(nb) * sizeof(long) / sizeof(ERL_NIF_TERM))

static inline void
set_bit(int nb, long *ptr){
  ptr[nb / BITS_PER_WORD] |= 1UL << (nb % BITS_PER_WORD);
}

static inline int
cmp_mask(int nwords, long *ptr1, long *ptr2){
  int i;
  for(i = 0; i < nwords; i++){
    if(ptr1[i] != ptr2[i])
      return 0;
  }
  return 1;
}
//return -1 on not search
static inline int
find_mask(unsigned nb, long *pattern, unsigned nelem, long *masks){
  unsigned ws = BITS_TO_WORDS(nb);
  int i;
  for(i = 0; i < nelem; i++){
    long *ptr = masks + i * ws;
    if(cmp_mask(ws, pattern, ptr))
      return i;
  }
  return -1;
}

typedef struct{
  ERL_NIF_TERM tag; //atom
  unsigned keys_off;
  unsigned arity;
}DecRecord;

typedef struct{
  unsigned records_cnt;
  unsigned ukeys_cnt;
  unsigned keys_cnt;
}DecEntry;

static inline ERL_NIF_TERM*
ukeys_base(DecEntry *dec_entry){
  return (ERL_NIF_TERM *)((void *)dec_entry + sizeof(DecEntry)) ;
}

static inline size_t
ukeys_size(unsigned ukeys_cnt){
  return ukeys_cnt * sizeof(ERL_NIF_TERM);
}

static inline unsigned*
keys_base(DecEntry *dec_entry, unsigned ukeys_cnt){
  return (unsigned *)((void *)ukeys_base(dec_entry)  + ukeys_size(ukeys_cnt));
}

static inline size_t
keys_size(unsigned keys_cnt){
  return keys_cnt * sizeof(unsigned);
}

static inline DecRecord*
records_base(DecEntry *dec_entry, unsigned ukeys_cnt, unsigned keys_cnt){
  return (DecRecord*)((void *)keys_base(dec_entry, ukeys_cnt)  + keys_size(keys_cnt));
}

static inline size_t
records_size(unsigned records_cnt){
  return records_cnt * sizeof(DecRecord);
}

static inline long *
bit_mask_base(DecEntry *dec_entry, unsigned ukeys_cnt, unsigned keys_cnt, unsigned records_cnt){
  return (long *)((void *)records_base(dec_entry, ukeys_cnt, keys_cnt) + records_size(records_cnt));
}

static inline size_t
bit_mask_array_size(unsigned ukeys_cnt, unsigned records_cnt){
  return records_cnt * BITS_TO_BYTES(ukeys_cnt);
}

static inline size_t
dec_resource_size(unsigned records_cnt, unsigned ukeys_cnt, unsigned keys_cnt){
  size_t ret = sizeof(DecEntry) + ukeys_size(ukeys_cnt) + keys_size(keys_cnt)
    + records_size(records_cnt) + bit_mask_array_size(ukeys_cnt, records_cnt);
  return ret;
}
