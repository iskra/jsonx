// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#ifdef _MSC_VER
#define inline __inline
#endif

//2-byte utf8 seq(110xxxxx)
#define U2 (1<<0)
//3-byte utf8 seq(1110xxxx)
#define U3 (1<<1)
//4-byte utf8 seq(11110xxxx)
#define U4 (1<<2)
//Utf8 rest byte(10xxxxxx), or bad start sequence
#define R  (1<<3)
//Not valid Utf8 byte
#define B  (1<<4)
//Char escaped to \X
// 8-13:"\b\t\n\v\f\r", 34:'"', 92:'\\'
#define E2  (1<<5)
//Byte escaped to \uXXXX, 0-31 without E2 and 127
#define E6  (1<<6)
#define E2E6  (E2|E6)

static const char hexval[] = "0123456789abcdef";

static const unsigned char js_map[] = {
//    0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f

     E6,  E6,  E6,  E6,  E6,  E6,  E6,  E6,   E2,  E2,  E2,  E2,  E2,  E2,  E6,  E6, 
     E6,  E6,  E6,  E6,  E6,  E6,  E6,  E6,   E6,  E6,  E6,  E6,  E6,  E6,  E6,  E6,
      0,   0,  E2,   0,   0,   0,   0,   0,    0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,    0,   0,   0,   0,   0,   0,   0,   0,

      0,   0,   0,   0,   0,   0,   0,   0,    0,   0,   0,   0,   0,   0,   0,   0, 
      0,   0,   0,   0,   0,   0,   0,   0,    0,   0,   0,   0,  E2,   0,   0,   0, 
      0,   0,   0,   0,   0,   0,   0,   0,    0,   0,   0,   0,   0,   0,   0,   0, 
      0,   0,   0,   0,   0,   0,   0,   0,    0,   0,   0,   0,   0,   0,   0,  E6,

      R,   R,   R,   R,   R,   R,   R,   R,    R,   R,   R,   R,   R,   R,   R,   R, 
      R,   R,   R,   R,   R,   R,   R,   R,    R,   R,   R,   R,   R,   R,   R,   R, 
      R,   R,   R,   R,   R,   R,   R,   R,    R,   R,   R,   R,   R,   R,   R,   R, 
      R,   R,   R,   R,   R,   R,   R,   R,    R,   R,   R,   R,   R,   R,   R,   R,

    /* 110xxxxx C0..DF */
     U2,  U2,  U2,  U2,  U2,  U2,  U2,  U2,   U2,  U2,  U2,  U2,  U2,  U2,  U2,  U2, 
     U2,  U2,  U2,  U2,  U2,  U2,  U2,  U2,   U2,  U2,  U2,  U2,  U2,  U2,  U2,  U2,
    /* 1110xxxx E0..EF */
     U3,  U3,  U3,  U3,  U3,  U3,  U3,  U3,   U3,  U3,  U3,  U3,  U3,  U3,  U3,  U3,
    /* 11110xxx F0..F7 */
     U4,  U4,  U4,  U4,  U4,  U4,  U4,  U4,    B,   B,   B,   B,   B,   B,   B,   B
};

#define MINUS1 0xFF
#define M1 MINUS1

static const unsigned char hex_tab[256] = {
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, M1, M1, M1, M1, M1, M1,
    M1, 10, 11, 12, 13, 14, 15, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, 10, 11, 12, 13, 14, 15, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,

    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
    M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1, M1,
};

static inline int
ucs_from_4hex(unsigned char* ptr, unsigned* hval ){
  unsigned char h;

  h = hex_tab[*(ptr)];
  if(h != M1)  *hval = h << 12; else return 0;
  h = hex_tab[*(++ptr)];
  if(h != M1) *hval = *hval + (h << 8); else return 0;
  h = hex_tab[*(++ptr)];
  if(h != M1) *hval = *hval + (h << 4); else return 0;
  h = hex_tab[*(++ptr)];
  if(h != M1) *hval = *hval + h; else return 0;

  return 1;
}

#undef M1
#undef MINUS1

static inline unsigned char*
ucs_to_utf8(unsigned char* ptr, unsigned ucs){
  if(ucs < 0x80) {
    // 0yyyyyyy
    *(ptr++) = (unsigned char)ucs;
    return ptr;
  } else if(ucs < 0x800) {
    // 110xxxxy 10yyyyyy
    *(ptr++) = (unsigned char) 0xC0 + (ucs >> 6);
    *(ptr++) = (unsigned char) 0x80 + (ucs & 0x3F);
    return ptr;
  }else if(ucs < 0x1000) {
    // 1110xxxx 10xyyyyy 10yyyyyy
    if(ucs < 0xD800 || (ucs > 0xDFFF && ucs < 0xFFFE)) {
      *(ptr++) = (unsigned char) 0xE0 + (ucs >> 12);
      *(ptr++) = (unsigned char) 0x80 + ((ucs >> 6) & 0x3F);
      *(ptr++) = (unsigned char) 0x80 + (ucs & 0x3F);
      return ptr;
    } else {
      return NULL;
    }
  } else if(ucs < 0x10FFFF) {
    // 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
    *(ptr++) = (unsigned char) 0xF0 + (ucs >> 18);
    *(ptr++) = (unsigned char) 0x80 + ((ucs >> 12) & 0x3F);
    *(ptr++) = (unsigned char) 0x80 + ((ucs >> 6) & 0x3F);
    *(ptr++) = (unsigned char) 0x80 + (ucs & 0x3F);
    return ptr;
  }
  return NULL;
}

static inline  int
check_with_unescape_jstr(unsigned char *str, unsigned char **endstr, unsigned char **endptr){
  unsigned char c, k;
  unsigned char *src = str;
  unsigned char *dst = str;
  for(;;){
    c = *src;
    k = js_map[c];
    if(!k){ *dst++ = *src++; continue;}
    if(c == '"'){*endstr = dst;*endptr = src; return 1;}
    switch(k){
    case U2:
      *dst++ = *src++;
      if(js_map[*src] == R){
	*dst++ = *src++; continue;
      }else{goto error;}
    case U3:
      *dst++ = *src++;
      if(js_map[*src] == R){
	*dst++ = *src++;
      }else{goto error;}
      if(js_map[*src] == R){
	*dst++ = *src++; continue;
      }else{goto error;}
    case U4:
      *dst++ = *src++;
      if(js_map[*src] == R){
	*dst++ = *src++;
      }else{goto error;}
      if(js_map[*src] == R){
	*dst++ = *src++;
      }else{goto error;}
      if(js_map[*src] == R){
	*dst++ = *src++; continue;
      }else{goto error;}
    }
    if(c == '\\'){
      src++;
      switch(*src){
      case 'b' : {src++; *dst++ =  8U; continue;}
      case 't' : {src++; *dst++ =  9U; continue;}
      case 'n' : {src++; *dst++ = 10U; continue;}
      case 'v' : {src++; *dst++ = 11U; continue;}
      case 'f' : {src++; *dst++ = 12U; continue;}
      case 'r' : {src++; *dst++ = 13U; continue;}
      case '"' : {src++; *dst++ = 34U; continue;}
      case '/' : {src++; *dst++ = 47U; continue;}
      case '\\': {src++; *dst++ = 92U; continue;}
      case 'u': {
	unsigned hval;
	src++;
	if(!ucs_from_4hex(src, &hval)) {goto error;}
	if(!(dst = ucs_to_utf8(dst, hval))) {goto error;}
	src += 4;
	continue;
      }
      default: {goto error;}
      }
    }
  error:
    *endptr = NULL;*endstr = NULL; return 0;
  }
}

//Check json string(without escaped char)
static inline int
check_noescaped_jstr(unsigned char *str, unsigned char **endptr){
  unsigned char c, k;
  size_t i = 0;
  for(;;){
    c = str[++i];
    k = js_map[c];
    if(!k){ continue;}
    if(c == '"'){*endptr = str + i; return 1;}
    switch(k){
    case U2:
      if(js_map[str[i + 1]] & R){
	i++; continue;
      }else{goto error;}
    case U3:
      if(js_map[str[i + 1]] & js_map[str[i + 2]] & R){
	i += 2; continue;
      }else{goto error;}
    case U4:
      if(js_map[str[i + 1]] & js_map[str[i + 2]] & js_map[str[i + 3]] & R){
	i += 3; continue;
      }else{goto error;}
    }
    if(c == '\\'){*endptr = str + i; return 1;}
  error:  
    *endptr = str + i; return 0;  
  }
}

//Validate utf8 and calculate reserve bytes for escape chars
static inline int
check_str_for_json(unsigned char *str, unsigned len, unsigned *reserve){
  unsigned char c;
  unsigned i;
  *reserve = 0;
  for(i=0; i<len; i++){
    c = js_map[str[i]];
    if(c){
      if(c == U2){
	if(i <= len - 2){
	  if(js_map[str[++i]] == R){ continue;}
	}
	return 0;
      }else if(c == E2){
	(*reserve)++;
	continue;
      }else if(c == E6){
	(*reserve) += 5;
	continue;
      }else if(c == U3){
	if(i <= len - 3){
	  if(js_map[str[i+1]] & js_map[str[i+2]] & R){
	    i += 2;
	    continue;
	  }
	}
	return 0;   
      }else if(c == U4){
	if(i <= len - 4){
	  if(js_map[str[i + 1]] & js_map[str[i + 2]] & js_map[str[i + 3]] & R){
	    i += 3;
	    continue;
	  }
	}
	return 0;
      }else{
	return 0;
      }
    }
  }
  return 1;
}

//Encode: copy with escape string to json string
static inline void
copy_str_to_jstr(unsigned char *dst, unsigned char *src, unsigned len){
  unsigned di, si;
  unsigned char c, k;
  for(si = 0, di = 0; si < len; si++){
    c = src[si];
    k = js_map[c];
    if(k & E2E6){
      dst[di++] = '\\';
      if(k == E2){
	switch(c){
	case 8:  dst[di++] = 'b';  continue;
	case 9:  dst[di++] = 't';  continue;
	case 10: dst[di++] = 'n';  continue;
	case 11: dst[di++] = 'v';  continue;
	case 12: dst[di++] = 'f';  continue;
	case 13: dst[di++] = 'r';  continue;
	case 34: dst[di++] = '"';  continue;
	case 39: dst[di++] = '\''; continue;
	case 47: dst[di++] = '/';  continue;
	case 92: dst[di++] = '\\'; continue;
	}
      }else{
	dst[di]   = 'u';
	dst[di+1] = '0';
	dst[di+2] = '0';
	dst[di+3] = hexval[c >> 4];
	dst[di+4] = hexval[c & 0x0fU];
	di += 5;
	continue;
      }
    }
    dst[di++] = src[si];
  }
  return;
}

//Encode: escape string to json string
static inline void
extend_str_to_jstr(unsigned char *data, unsigned len, unsigned ext){
  int di, si;
  unsigned char c, k;
  for(si = len - 1, di = len + ext - 1; si >= 0; si--){
    c = data[si];
    k = js_map[c];
    if(k & E2E6){
      //     
      if(k == E2){
	di -= 2;
	data[di+1] = '\\';
	switch(c){
	case 8:  data[di+2] = 'b';  continue;
	case 9:  data[di+2] = 't';  continue;
	case 10: data[di+2] = 'n';  continue;
	case 11: data[di+2] = 'v';  continue;
	case 12: data[di+2] = 'f';  continue;
	case 13: data[di+2] = 'r';  continue;
	case 34: data[di+2] = '"';  continue;
	case 92: data[di+2] = '\\'; continue;
	}
      }else{
	data[di-5] = '\\';
	data[di-4]   = 'u';
	data[di-3] = '0';
	data[di-2] = '0';
	data[di-1] = hexval[c >> 4];
	data[di]   = hexval[c & 0x0fU];
	di -= 6;
	continue;
      }
    }
    data[di--] = data[si];
  }
  return;
};
