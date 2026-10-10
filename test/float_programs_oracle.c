#include <math.h>
#include <stdint.h>

void program_value(double);
void program_count(uint32_t);
void program_header(int);
void program_pixel(uint32_t);

static double sphere_x(int i) { return i == 0 ? -0.8 : i == 1 ? 0.9 : 0.0; }
static double sphere_y(int i) { return i == 2 ? 0.6 : -0.3; }
static double sphere_z(int i) { return i == 2 ? 4.0 : 3.0; }
static double sphere_r(int i) { return i == 2 ? 0.5 : 0.7; }
static double distance(double ox, double oy, double oz, double dx, double dy, double dz, int i) {
  if (i == 3) {
    if (dy >= 0.0) return 1.0e30;
    double t = (-1.0 - oy) / dy;
    return t > 0.00001 ? t : 1.0e30;
  }
  double x = ox - sphere_x(i), y = oy - sphere_y(i), z = oz - sphere_z(i);
  double b = x * dx + y * dy + z * dz;
  double c = x * x + y * y + z * z - sphere_r(i) * sphere_r(i);
  double d = b * b - c;
  if (d < 0.0) return 1.0e30;
  double t = -b - sqrt(d);
  return t > 0.00001 ? t : 1.0e30;
}
static double trace_ray(double ox, double oy, double oz, double dx, double dy, double dz, int depth) {
  double nearest = 1.0e30;
  int object = -1;
  for (int i = 0; i < 4; ++i) {
    double t = distance(ox, oy, oz, dx, dy, dz, i);
    if (t < nearest) { nearest = t; object = i; }
  }
  if (object < 0) return 0.08 + 0.12 * (dy + 1.0);
  double x = ox + nearest * dx, y = oy + nearest * dy, z = oz + nearest * dz;
  double nx = 0.0, ny = 1.0, nz = 0.0;
  if (object < 3) {
    nx = (x - sphere_x(object)) / sphere_r(object);
    ny = (y - sphere_y(object)) / sphere_r(object);
    nz = (z - sphere_z(object)) / sphere_r(object);
  }
  double lx = -3.0 - x, ly = 5.0 - y, lz = -4.0 - z;
  double length = sqrt(lx * lx + ly * ly + lz * lz);
  lx /= length; ly /= length; lz /= length;
  double intensity = nx * lx + ny * ly + nz * lz;
  if (intensity < 0.0) intensity = 0.0;
  double sx = x + nx * 0.0001, sy = y + ny * 0.0001, sz = z + nz * 0.0001;
  for (int i = 0; i < 4; ++i)
    if (distance(sx, sy, sz, lx, ly, lz, i) < length) intensity = 0.0;
  double shade = 0.1 + 0.6 * intensity;
  if (depth < 2) {
    double dot = dx * nx + dy * ny + dz * nz;
    double reflected = trace_ray(sx, sy, sz, dx - 2.0 * dot * nx,
      dy - 2.0 * dot * ny, dz - 2.0 * dot * nz, depth + 1);
    shade += 0.25 * reflected;
  }
  return shade;
}
static void ray_program(int n) {
  program_header(n);
  for (int y = 0; y < n; ++y) for (int x = 0; x < n; ++x) {
    double dx = ((double)x + 0.5) * 2.0 / (double)n - 1.0;
    double dy = 1.0 - ((double)y + 0.5) * 2.0 / (double)n;
    double length = sqrt(dx * dx + dy * dy + 1.0);
    double shade = trace_ray(0.0, 0.0, -2.0, dx / length, dy / length, 1.0 / length, 0);
    if (shade > 1.0) shade = 1.0;
    program_pixel((uint32_t)(shade * 255.0));
  }
}
static void nbody_program(int steps) {
  double px[16] = {0}, py[16] = {0}, pz[16] = {0};
  double vx[16] = {0}, vy[16] = {0}, vz[16] = {0};
  for (int i = 0; i < 16; ++i) {
    px[i] = (double)i * 0.3 - 2.0;
    py[i] = (double)(i % 3) * 0.2;
    pz[i] = (double)(i % 5) * 0.1;
    vy[i] = (double)(i % 2) * 0.02 - 0.01;
  }
  for (int step = 0; step < steps; ++step) {
    for (int i = 0; i < 16; ++i) {
      double ax = 0.0, ay = 0.0, az = 0.0;
      for (int j = 0; j < 16; ++j) if (i != j) {
        double dx = px[j] - px[i], dy = py[j] - py[i], dz = pz[j] - pz[i];
        double r2 = dx * dx + dy * dy + dz * dz + 0.01;
        double force = 1.0 / (r2 * sqrt(r2));
        ax += dx * force; ay += dy * force; az += dz * force;
      }
      vx[i] += 0.001 * ax; vy[i] += 0.001 * ay; vz[i] += 0.001 * az;
    }
    for (int i = 0; i < 16; ++i) {
      px[i] += 0.001 * vx[i]; py[i] += 0.001 * vy[i]; pz[i] += 0.001 * vz[i];
    }
  }
  for (int i = 0; i < 16; ++i) {
    program_value(px[i]); program_value(py[i]); program_value(pz[i]);
    program_value(vx[i]); program_value(vy[i]); program_value(vz[i]);
  }
}
static void matrix_program(int repeats) {
  double a[4096] = {0}, b[4096] = {0}, product[4096] = {0}, lu[4096] = {0};
  for (int i = 0; i < 64; ++i) for (int j = 0; j < 64; ++j) {
    int index = i * 64 + j;
    a[index] = (double)((i * 17 + j * 13) % 23) / 23.0;
    b[index] = (double)((i * 7 + j * 19) % 29) / 29.0;
    lu[index] = a[index] + (i == j ? 64.0 : 0.0);
  }
  for (int repeat = 0; repeat < repeats; ++repeat)
    for (int i = 0; i < 64; ++i) for (int j = 0; j < 64; ++j) {
      double sum = 0.0;
      for (int k = 0; k < 64; ++k) sum += a[i * 64 + k] * b[k * 64 + j];
      product[i * 64 + j] = sum;
    }
  for (int k = 0; k < 64; ++k) for (int i = k + 1; i < 64; ++i) {
    lu[i * 64 + k] /= lu[k * 64 + k];
    for (int j = k + 1; j < 64; ++j) lu[i * 64 + j] -= lu[i * 64 + k] * lu[k * 64 + j];
  }
  for (int i = 0; i < 4096; ++i) { program_value(product[i]); program_value(lu[i]); }
}
static void fft_program(int n) {
  double real[1024] = {0}, imag[1024] = {0};
  for (int i = 0; i < n; ++i) real[i] = (double)((i * 13) % 31) / 31.0;
  int j = 0;
  for (int i = 1; i < n; ++i) {
    int bit = n >> 1;
    while (j & bit) { j ^= bit; bit >>= 1; }
    j ^= bit;
    if (i < j) { double temp = real[i]; real[i] = real[j]; real[j] = temp; }
  }
  for (int length = 2; length <= n; length *= 2) {
    double angle = -6.283185307179586 / (double)length;
    double wr_step = cos(angle), wi_step = sin(angle);
    for (int base = 0; base < n; base += length) {
      double wr = 1.0, wi = 0.0;
      for (j = 0; j < length / 2; ++j) {
        int left = base + j, right = left + length / 2;
        double vr = real[right] * wr - imag[right] * wi;
        double vi = real[right] * wi + imag[right] * wr;
        double ur = real[left], ui = imag[left];
        real[left] = ur + vr; imag[left] = ui + vi;
        real[right] = ur - vr; imag[right] = ui - vi;
        double next_wr = wr * wr_step - wi * wi_step;
        wi = wr * wi_step + wi * wr_step; wr = next_wr;
      }
    }
  }
  for (int i = 0; i < n; ++i) { program_value(real[i]); program_value(imag[i]); }
}
static void mandelbrot_program(int n, int limit) {
  for (int y = 0; y < n; ++y) for (int base = 0; base < n; base += 8) {
    float scale = 3.0f / (float)n;
    for (int lane = 0; lane < 8; ++lane) {
      float cx = (float)(base + lane) * scale - 2.0f;
      float cy = (float)y * 2.0f / (float)n - 1.0f;
      float x = 0.0f, z = 0.0f;
      uint32_t count = 0;
      int active = 1;
      for (int iteration = 0; iteration < limit; ++iteration) {
        float xx = x * x, zz = z * z;
        active = active && xx + zz <= 4.0f;
        count += active;
        float next_z = 2.0f * x * z + cy;
        if (active) { x = xx - zz + cx; z = next_z; }
      }
      program_count(count);
    }
  }
}
void programs_run(int kind, int size) {
  if (kind == 0) ray_program(size);
  else if (kind == 1) nbody_program(size);
  else if (kind == 2) matrix_program(size);
  else if (kind == 3) fft_program(size);
  else mandelbrot_program(size, 96);
}
