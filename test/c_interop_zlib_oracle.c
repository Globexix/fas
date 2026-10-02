#include <stdio.h>
#include <string.h>
#include <zlib.h>

int main(void) {
  const Bytef input[] = "fas zlib";
  Bytef compressed[128];
  Bytef restored[128];
  uLongf compressed_len = sizeof(compressed);
  z_stream deflater = {0};
  z_stream inflater = {0};
  uInt compressed_size;
  int status;

  if (compress(compressed, &compressed_len, input, sizeof(input) - 1) != Z_OK)
    return 1;
  if (compressed_len == 0) return 2;
  deflater.zalloc = Z_NULL;
  deflater.zfree = Z_NULL;
  deflater.opaque = Z_NULL;
  deflater.next_in = (Bytef *)input;
  deflater.avail_in = (uInt)(sizeof(input) - 1);
  deflater.next_out = compressed;
  deflater.avail_out = sizeof(compressed);
  if (deflateInit(&deflater, 6) != Z_OK) return 3;
  status = deflate(&deflater, Z_FINISH);
  if (status != Z_STREAM_END) return 4;
  compressed_size = (uInt)deflater.total_out;
  if (deflateEnd(&deflater) != Z_OK) return 5;

  inflater.zalloc = Z_NULL;
  inflater.zfree = Z_NULL;
  inflater.opaque = Z_NULL;
  inflater.next_in = compressed;
  inflater.avail_in = compressed_size;
  inflater.next_out = restored;
  inflater.avail_out = sizeof(restored);
  if (inflateInit(&inflater) != Z_OK) return 6;
  status = inflate(&inflater, Z_FINISH);
  if (status != Z_STREAM_END) return 7;
  if (inflateEnd(&inflater) != Z_OK) return 8;
  if (inflater.total_out != sizeof(input) - 1) return 9;
  if (memcmp(input, restored, sizeof(input) - 1) != 0) return 10;
  puts("zlib: ok");
  return 0;
}
