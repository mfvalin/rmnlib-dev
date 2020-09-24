
// bit stream packing macros
#if ! defined(StreamPackingMacros)
#define StreamPackingMacros
typedef struct{ // stream of 32 bit tokens
  uint32_t *b;  // start of packed stream
  uint32_t *s;  // pointer into packed stream
  uint64_t t;   // temporary accumulator (64 bits)
  int32_t  n;   // number of bits accumulator
  int32_t  l;   // max length of stream
} stream_32;
typedef struct{ // stream of 16 bit tokens
  uint16_t *b;  // start of packed stream
  uint16_t *s;  // pointer into packed stream
  uint64_t t;   // temporary accumulator (64 bits)
  int32_t  n;   // number of bits in accumulator
  int32_t  l;   // max length of stream
} stream_16;
typedef struct{ // stream of 8 bit tokens
  uint8_t *b;   // start of packed stream
  uint8_t *s;   // pointer into packed stream
  uint64_t t;   // temporary accumulator (64 bits)
  int32_t  n;   // number of bits in accumulator
  int32_t  l;   // max length of stream
} stream_8;

// create and initialize a bit packing stream
// streamxx  : pointer to a stream_8/stream_16/stream_32 object
// siz       : size in 8/16/32 bit elements of the stream buffer
// p         : if NULL, it will be allocated automatically,
//             if not NULL it must point at an area large enough for siz 8/16/32 bit elements
// function value : the stream buffer address
static inline void *OpenStream32(stream_32 *stream32, uint32_t siz, void *p){
  stream32->t = 0 ;
  stream32->s = (p != NULL) ? (uint32_t *)p : (uint32_t *)malloc(siz*sizeof(uint32_t)) ;
  stream32->b = stream32->s ;
  stream32->n = 0 ;
  stream32->l = siz ;
  return stream32->s ;
}
static inline void *OpenStream16(stream_16 *stream16, uint32_t siz, void *p){
  stream16->t = 0 ;
  stream16->s = (p != NULL) ? (uint16_t *)p : (uint16_t *)malloc(siz*sizeof(uint16_t)) ;
  stream16->b = stream16->s ;
  stream16->n = 0 ;
  stream16->l = siz ;
  return stream16->s ;
}
static inline void *OpenStream8(stream_8 *stream8, uint32_t siz, void *p){
  stream8->t = 0 ;
  stream8->s = (p != NULL) ? (uint8_t *)p : (uint8_t *)malloc(siz*sizeof(uint8_t)) ;
  stream8->b = stream8->s ;
  stream8->n = 0 ;
  stream8->l = siz ;
  return stream8->s ;
}

// WriteStream functions ASSUME that there is enough room for the token, token is ASSUMED to be nb wide (or less)
// nb bits from token will be inserted into the stream ACCUMULATOR
// nb is ASSUMED to be 32 or less
#define SafeInsertToken(t, token, nb) { (t) = ((t) << (nb)) | ((token) & (~ ((-1) << (nb)))) ; }
#define FastInsertToken(t, token, nb) { (t) = ((t) << (nb)) |  (token)                        ; }
// streamxx  : pointer to a stream_8/stream_16/stream_32 object
// token     : the lower nb bits of token will be inserted into the bit stream
// nb        : number of bits to insert (least significant bist of token)
static inline void WriteStream32(stream_32 *stream32, uint32_t token, int32_t nb){
  SafeInsertToken(stream32->t, token, nb)
  stream32->n += nb ;
}
static inline void WriteStream16(stream_16 *stream16, uint32_t token, int32_t nb){
  SafeInsertToken(stream16->t, token, nb)
  stream16->n += nb ;
}
static inline void WriteStream8(stream_8 *stream8, uint32_t token, int32_t nb){
  SafeInsertToken(stream8->t, token, nb)
  stream8->n += nb ;
}

// ReadStream functions ASSUME that there are enough bits in the stream ACCUMULATOR to satisfy a read of nb bits
// nb is ASSUMED to be 32 or less
#define FastExtractToken(t, token, nb) { (token) = (t) >> (64 -(nb)) ; (t) = (t) << (nb) ; }
// streamxx  : pointer to a stream_8/stream_16/stream_32 object
// nb        : number of bits to extract (least significant bist of token)
// functoin value : the extracted nb bits token
static inline uint32_t ReadStream32(stream_32 *stream32, int32_t nb){
  uint32_t t ;
  FastExtractToken(stream32->t, t, nb) ;
  stream32->n -= nb ;
  return t;
}
static inline uint32_t ReadStream16(stream_16 *stream16, int32_t nb){
  uint32_t t ;
  FastExtractToken(stream16->t, t, nb) ;
  stream16->n -= nb ;
  return t;
}
static inline uint32_t ReadStream8(stream_8 *stream8, int32_t nb){
  uint32_t t ;
  FastExtractToken(stream8->t, t, nb) ;
  stream8->n -= nb ;
  return t;
}

// check if one or more tokens must be ejected to the stream buffer, if so, do it and update pointers
// an eject will happen if the number of available bits in the ACCUMULATOR falls below 32/48/56
// after ejction, the number of bits in ACCUMULATOR will be <= 32/16/8
// streamxx  : pointer to a stream_8/stream_16/stream_32 object
static inline void CheckStream32(stream_32 *stream32){
  if( stream32->n - 32 > 0) {                             // more than 32 bits in accumulator
    *(stream32->s) = stream32->t >> (stream32->n - 32) ;  // write a 32 bit token
    stream32->s = stream32->s + 1 ;                       // bump storage pointer
    stream32->n = stream32->n - 32 ;                      // decrement used bits count
  }
}
static inline void CheckStream16(stream_16 *stream16){
  while( stream16->n - 16 > 0) {                          // while more than 16 bits in accumulator
    *(stream16->s) = stream16->t >> (stream16->n - 16) ;  // write a 16 bit token
    stream16->s = stream16->s + 1 ;                       // bump storage pointer
    stream16->n = stream16->n - 16 ;                      // decrement used bits count
  }
}
static inline void CheckStream8(stream_8 *stream8){
  while( stream8->n - 8 > 0) {                            // while more than 8 bits in accumulator
    *(stream8->s) = stream8->t >> (stream8->n - 8) ;      // write a 8 bit token
    stream8->s = stream8->s + 1 ;                         // bump storage pointer
    stream8->n = stream8->n - 8 ;                         // decrement used bits count
  }
}

// make sure that there are at least enough usable bits in stream buffer to satisfy a 32 bit request
// streamxx  : pointer to a stream_8/stream_16/stream_32 object
static inline void FillStream32(stream_32 *stream32){
  if(stream32->n - 32 >= 0) return ;                      // at least 32 bits in accumulator
  stream32->t >>= (32 - stream32->n) ;                    // align to 32 bit middle boundary
  stream32->t |= *(stream32->s) ;                         // fill lower 32 bits
  stream32->t <<= (32 - stream32->n) ;                    // realign to top of 64 bit word
  stream32->s = stream32->s + 1 ;                         // bump buffer pointer
  stream32->n += 32 ;                                     // add 32 bits to count os usable bits
}
static inline void FillStream16(stream_16 *stream16){
  if(stream16->n - 32 >= 0) return ;                      // at least 32 bits in accumulator
  while(stream16->n - 48 <= 0) {                          // fill to > 48 bits
    stream16->t >>= (48 - stream16->n) ;                  // align to 16 bit lower boundary
    stream16->t |= *(stream16->s) ;                       // fill lower 16 bits
    stream16->s = stream16->s + 1 ;                       // bump buffer pointer
    stream16->t <<= (48 - stream16->n) ;                  // realign to top of 64 bit word
    stream16->n += 16 ;                                   // add 16 bits to count os usable bits
  }
}
static inline void FillStream8(stream_8 *stream8){
  if(stream8->n - 32 >= 0) return ;                       // at least 32 bits in accumulator
  while(stream8->n - 56 <= 0) {                           // fill to > 56 bits
    stream8->t >>= (56 - stream8->n) ;                    // align to 8 bit lower boundary
    stream8->t |= *(stream8->s) ;                         // fill lower 8 bits
    stream8->s = stream8->s + 1 ;                         // bump buffer pointer
    stream8->t <<= (56 - stream8->n) ;                    // realign to top of 64 bit word
    stream8->n += 8 ;                                     // add 8 bits to count os usable bits
  }
}
  

// flush a stream accumulator to the stream buffer, return number of buffer elements used in siz
// streamxx  : pointer to a stream_8/stream_16/stream_32 object
static inline int FlushStream32(stream_32 *stream32){
  CheckStream32(stream32) ;
  if(stream32->n != 0) {
    *(stream32->s) = (stream32->t << (64 - stream32->n)) >> 32 ;
    stream32->s = stream32->s + 1 ;
  }
  stream32->t = 0 ;
  stream32->n = 0 ;
  return (stream32->s - stream32->b) ;
}
static inline int FlushStream16(stream_16 *stream16){
  CheckStream16(stream16) ;
  if(stream16->n != 0) {
    *(stream16->s) = (stream16->t << (64 - stream16->n)) >> 48 ;
    stream16->s = stream16->s + 1 ;
  }
  stream16->t = 0 ;
  stream16->n = 0 ;
  return (stream16->s - stream16->b) ;
}
static inline int FlushStream8(stream_8 *stream8){
  CheckStream8(stream8) ;
  if(stream8->n != 0) {
    *(stream8->s) = (stream8->t << (64 - stream8->n)) >> 56 ;
    stream8->s = stream8->s + 1 ;
  }
  stream8->t = 0 ;
  stream8->n = 0 ;
  return (stream8->s - stream8->b) ;
}

// rewind a bit packing stream (reset all pointers to the start)
// if writing, the appropriate FlushStream MUST be called before RewindStreamxx
//             or data might get lost
// streamxx  : pointer to a stream_8/stream_16/stream_32 object
// function return : the number of elements used in the stream buffer
//          (not to be confused with the number of valid bits)
static inline int RewindStream32(stream_32 *stream32){
  int32_t l = stream32->s - stream32->b ;
  stream32->s = stream32->b ;     // reset stream buffer to its starting position
  stream32->t = 0 ;
  stream32->n = 0 ;
  return l ;
}
static inline int RewindStream16(stream_16 *stream16){
  int32_t l = stream16->s - stream16->b ;
  stream16->s = stream16->b ;     // reset stream buffer to its starting position
  stream16->t = 0 ;
  stream16->n = 0 ;
  return l ;
}
static inline int RewindStream8(stream_8 *stream8){
  int32_t l = stream8->s - stream8->b ;
  stream8->s = stream8->b ;     // reset stream buffer to its starting position
  stream8->t = 0 ;
  stream8->n = 0 ;
  return l ;
}

// set the read/write position of a bit packing stream
// FlushStreamxx MUST be called BEFORE SeekStreamxx if the stream was in write mode
//               (data could be lost otherwise)
// FlushStreamxx SHOULD be called AFTER SeekStreamxx if positioning for writing
// FillStreamxx  SHOULD be called AFTER SeekStreamxx if positioning for reading
static inline void SeekStream32(stream_32 *stream32, uint32_t offset, int write){
  uint32_t t_offset = (offset >> 5) ;                    // offset in 32 bit units
  uint32_t b_offset = (offset & 0x1F) ;                  // bit offset (modulo 32)
// fprintf(stderr,"o = %d, t = %d, b = %d\n", offset, t_offset, b_offset);
  stream32->s = stream32->b + t_offset ;                 // position to the proper buffer token
  stream32->t = *(stream32->s) ;                         // read it into accumulator
  if(write){                                             // set position for writing
    stream32->t >>= (32 - b_offset) ;                    // align with the bottom of the accumulator
    stream32->n = b_offset ;                             // there are b_offset valid bits in accumulator
// fprintf(stderr,"acc = %16.16x\n",stream32->t);
  }else{                                                 // set position for reading
    stream32->s = stream32->s + 1 ;                      // bump stream read pointer
    stream32->t <<= (32 + b_offset) ;                    // eliminate the leading b_offset bits, align to top of accumulator
    stream32->n = (32 - b_offset) ;                      // number of valid bits
  }
}
static inline void SeekStream16(stream_16 *stream16, uint32_t offset, int write){
  uint32_t t_offset = (offset >> 4) ;                    // offset in 16 bit units
  uint32_t b_offset = (offset & 0xF) ;                   // bit offset (modulo 32)
  stream16->s = stream16->b + t_offset ;                 // position to the proper buffer token
  stream16->t = *(stream16->s) ;                         // read it into accumulator
  if(write){                                             // set position for writing
    stream16->t >>= (16 - b_offset) ;                    // align with the bottom of the accumulator
    stream16->n = b_offset ;                             // there are b_offset valid bits in accumulator
  }else{                                                 // set position for reading
    stream16->s = stream16->s + 1 ;                      // bump stream read pointer
    stream16->t <<= (48 + b_offset) ;                    // eliminate the leading b_offset bits, align to top of accumulator
    stream16->n = (16 - b_offset) ;                      // number of valid bits
  }
}
static inline void SeekStream8(stream_8 *stream8, uint32_t offset, int write){
  uint32_t t_offset = (offset >> 3) ;                    // offset in 8 bit units
  uint32_t b_offset = (offset & 0x7) ;                   // bit offset (modulo 32)
  stream8->s = stream8->b + t_offset ;                   // position to the proper buffer token
  stream8->t = *(stream8->s) ;                           // read it into accumulator
  if(write){                                             // set position for writing
    stream8->t >>= (8 - b_offset) ;                      // align with the bottom of the accumulator
    stream8->n = b_offset ;                              // there are b_offset valid bits in accumulator
  }else{                                                 // set position for reading
    stream8->s = stream8->s + 1 ;                        // bump stream read pointer
    stream8->t <<= (56 + b_offset) ;                     // eliminate the leading b_offset bits, align to top of accumulator
    stream8->n = (8 - b_offset) ;                        // number of valid bits
  }
}
#endif

